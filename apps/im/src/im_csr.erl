-module(im_csr).

-include("im_common.hrl").

-export([get_open/2, room_csr/2, create/2, grab/2, drop/2, close/2, add_tag/2, remove_tag/2]).

get_open(#'OpenCsr'{ref=Ref, top=Top, stop=Stop, count=Count}, User=#im_usr{}) ->
  FilterFun = case User#im_usr.vendorId =/= undefined of
    true -> fun({ok, Request=#im_csr{}}) -> Request#im_csr.vendorId =:= User#im_usr.vendorId end;
    false -> undefined
  end,
  Requests = ctail_feed:get(im_csr, {<<"csr">>,<<"open">>}, im_common:parse_id(Top), im_common:parse_id(Stop), Count, FilterFun),
  FilteredRequests = lists:filter(fun(Request) ->
    Request#im_csr.status =:= ?CSR_STATUS_OPEN orelse Request#im_csr.status =:= ?CSR_STATUS_DROPPED
  end, Requests),
  #'OpenCsrResp'{ref=Ref, requests=[im_dto:format_csr(Request) || Request <- FilteredRequests]}.

room_csr(#'RoomCsr'{ref=Ref, roomId=RoomId}, #im_usr{vendorId=VendorId}) ->
  Requests = ctail_mongo:find(im_csr, {
    <<"roomId">>, im_common:parse_id(RoomId),
    <<"vendorId">>, im_common:parse_id(VendorId)
  }, 0, 999),
  #'RoomCsrResp'{ref=Ref, requests=[im_dto:format_csr(Request) || Request <- Requests]}.

create(#'CreateCsr'{ref=Ref, request=RequestEntity}, #im_usr{id=UserId}) ->
  Request = im_dto:parse_csr(RequestEntity),
  RoomId = Request#im_csr.roomId,

  Now = sm:now(),
  Activity = make_action(UserId, <<"create">>, Now),
  RequestId = ctail:next_id(),

  UpdateRoomResult = case RoomId =/= undefined of
    true ->
      Room = try im_roster_muc:get(RoomId) catch _:_ -> undefined end,
      case Room =/= undefined of
        true ->
          case Room#im_grp.requestId =:= undefined of
            true ->
              ctail:put(Room#im_grp{requestId=RequestId}),
              im_roster_muc:refresh(RoomId),

              {User, UpdatedUpdate} = im_chatupdate:quick_update(Request#im_csr.userId, ?MESSAGE_FEED_TYPE_ROOM, Request#im_csr.roomId, fun(Update, User) ->
                ReplyKeyboardMarkup = #'ReplyKeyboardMarkupEntity'{buttonRows = [
                  #'KeyboardButtonRowEntity'{buttons = [
                    #'KeyboardButtonEntity'{text= <<"Close ticket">>, payload= <<"/request close">>}
                  ]}
                ], resizeKeyboard = false,oneTimeKeyboard = false, selective = false},
                Update1 = Update#im_chat_update{replyKeyboardMarkup=im_dto:parse_reply_keyboard_markup(ReplyKeyboardMarkup)},
                {{User, Update1}, Update1}
              end),
              im_chatupdate:send_update(User, UpdatedUpdate),
              {ok, Room};
            false ->
              {error, request_exists}
          end;
        false -> {ok, undefined}
      end;
    false -> {ok, undefined}
  end,

  case UpdateRoomResult of
    {ok, Room1} ->
      Thumbnail = case Request#im_csr.thumbnail =/= undefined of
        true -> Request#im_csr.thumbnail;
        false ->
          case Room1 =/= undefined of
            true -> Room1#im_grp.thumbnail;
            false -> undefined
          end
      end,
      Descr = case Request#im_csr.descr =/= undefined of
        true -> Request#im_csr.descr;
        false ->
          case Room1 =/= undefined of
            true -> Room1#im_grp.topic;
            false -> undefined
          end
      end,
      Request1 = Request#im_csr{id=RequestId,
        feed_id={<<"csr">>,<<"open">>},
        status=?CSR_STATUS_OPEN,
        created=Now,
        updated=Now,
        thumbnail=Thumbnail,
        descr=Descr,
        activity=Activity},
      ctail_feed:add(Request1),

      notificate_managers(Request1),

      #'CreateCsrResp'{ref=Ref, request=im_dto:format_csr(Request1)};
    {error, request_exists} ->
      #'ErrorResp'{ref=Ref, code=?ERROR_CODE_INVALID_MESSAGE, message="This room already has open CSR"}
  end.

notificate_managers(Request) ->
  RoleIds = [RoleId || #im_usr_role{id=RoleId} <- im_acl:get_roles_for_perm("manage_csr")],
  Managers = ctail_mongo:find(im_usr, #{<<"roles">> => #{<<"$in">> => RoleIds}}, 0, 999999999),
  im_logger:info(undefined, "[CSR] Notify managers: ~p, roles: ~p", [length(Managers), RoleIds]),
  lists:foreach(fun(#im_usr{id=UserId}) ->
    send_ticket_created_push(UserId, Request)
  end, Managers).

send_ticket_created_push(UserId, Request=#im_csr{}) ->
  IOSAlertText = im_trans:t(im_locale:get(UserId), <<"push.message.csr.created">>),
  AndroidJson = [{<<"csrId">>, im_common:format_id(Request#im_csr.id)}],
  im_push_ios:send_alert(UserId, IOSAlertText, <<"csr">>),
  im_push_web:send_alert(UserId, IOSAlertText, <<"csr">>),
  im_push:send_json(UserId, ?PLATFORM_ANDROID, [{<<"data">>, AndroidJson}, {<<"priority">>, <<"high">>}, {<<"collapse_key">>, <<"csr">>}]).

grab(#'GrabCsr'{ref=Ref, id=RequestId}, #im_usr{id=UserId, vendorId=VendorId}) ->
  case ctail:get(im_csr, im_common:parse_id(RequestId)) of
    {ok, Request} ->
      case im_common:format_id(Request#im_csr.vendorId) =:= im_common:format_id(VendorId) of
        true ->
          case Request#im_csr.status =:= ?CSR_STATUS_OPEN orelse Request#im_csr.status =:= ?CSR_STATUS_DROPPED of
            true ->
              Now = sm:now(),
              Request1 = Request#im_csr{
                feed_id=undefined,
                status=?CSR_STATUS_GRABBED,
                updated=Now,
                activity=Request#im_csr.activity ++ make_action(UserId, <<"grab">>, Now),
                staff=im_common:list_unique(Request#im_csr.staff ++ [UserId]),
                grabbedBy=UserId
              },
              case im_room:add_inner(Request#im_csr.roomId, [UserId]) of
                {ok, _} ->
                  {User, UpdatedUpdate} = im_chatupdate:quick_update(UserId, ?MESSAGE_FEED_TYPE_ROOM, Request#im_csr.roomId, fun(Update, User) ->
                    ReplyKeyboardMarkup = #'ReplyKeyboardMarkupEntity'{buttonRows = [
                      #'KeyboardButtonRowEntity'{buttons = [
                        #'KeyboardButtonEntity'{text= <<"Drop ticket">>, payload= <<"/request drop">>},
                        #'KeyboardButtonEntity'{text= <<"Close ticket">>, payload= <<"/request close">>}
                      ]}
                    ], resizeKeyboard = false,oneTimeKeyboard = false, selective = false},
                    Update1 = Update#im_chat_update{
                      replyKeyboardMarkup=im_dto:parse_reply_keyboard_markup(ReplyKeyboardMarkup),
                      removeKeyboardMarkup=undefined
                    },
                    {{User, Update1}, Update1}
                  end),
                  im_chatupdate:send_update(User, UpdatedUpdate),
                  ctail_feed:remove(im_csr, Request#im_csr.id),
                  ctail:put(Request1),
                  #'GrabCsrResp'{
                    ref=Ref,
                    request=im_dto:format_csr(Request1),
                    update=im_dto:format_chat_update(UserId, UpdatedUpdate)
                  };
                _ ->
                  #'ErrorResp'{code=?ERROR_CODE_UNKNOWN, message = <<"Failed to add user in room">>}
              end;
            false ->
              #'ErrorResp'{ref=Ref, code=?ERROR_CODE_INVALID_MESSAGE, message= <<"Cannot transit to grabbed state from current state">>}
          end;
        false ->
          #'ErrorResp'{ref=Ref, code=?ERROR_CODE_PERMISSION_DENIED, message= <<"This request in other vendor scope">>}
      end;
    {error, _} ->
      #'ErrorResp'{ref=Ref, code=?ERROR_CODE_NOT_FOUND}
  end.

drop(#'DropCsr'{ref=Ref, id=RequestId}, #im_usr{id=UserId}) ->
  case ctail:get(im_csr, im_common:parse_id(RequestId)) of
    {ok, Request} ->
      case UserId =:= Request#im_csr.grabbedBy of
        true ->
          case Request#im_csr.status =:= ?CSR_STATUS_GRABBED of
            true ->
              Now = sm:now(),
              Request1 = Request#im_csr{feed_id={<<"csr">>,<<"open">>},
                status=?CSR_STATUS_DROPPED,
                updated=Now,
                activity=Request#im_csr.activity ++ make_action(UserId, <<"drop">>, Now),
                grabbedBy=undefined},
              ctail:delete(im_csr, Request#im_csr.id),
              ctail_feed:add(Request1),

              %% Quit room and remove replyKeyboard then send update to users.
              im_room:quit(#'QuitFromRoom'{roomId=Request#im_csr.roomId}, UserId),
              {User, UpdatedUpdate} = im_chatupdate:quick_update(UserId, ?MESSAGE_FEED_TYPE_ROOM, Request#im_csr.roomId, fun(Update, User) ->
                ReplyKeyboardMarkup = undefined,
                RemoveKeyboardMarkup = #'RemoveKeyboardMarkupEntity'{},
                Update1 = Update#im_chat_update{
                  replyKeyboardMarkup=im_dto:parse_reply_keyboard_markup(ReplyKeyboardMarkup),
                  removeKeyboardMarkup=im_dto:parse_remove_keyboard_markup(RemoveKeyboardMarkup)
                },
                {{User, Update1}, Update1}
              end),
              im_chatupdate:send_update(User, UpdatedUpdate),

              #'DropCsrResp'{ref=Ref, request=im_dto:format_csr(Request1)};
            false ->
              #'ErrorResp'{ref=Ref, code=?ERROR_CODE_INVALID_MESSAGE, message= <<"Cannot transit to dropped state from current state">>}
          end;
        false ->
          #'ErrorResp'{ref=Ref, code=?ERROR_CODE_PERMISSION_DENIED, message= <<"Request was grabbed by another user">>}
      end;
    {error, _} ->
      #'ErrorResp'{ref=Ref, code=?ERROR_CODE_NOT_FOUND}
  end.

close(#'CloseCsr'{ref=Ref, id=RequestId}, #im_usr{id=UserId}) ->
  case ctail:get(im_csr, im_common:parse_id(RequestId)) of
    {ok, Request} ->
      case UserId =:= Request#im_csr.userId orelse UserId =:= Request#im_csr.grabbedBy of
        true ->
          case Request#im_csr.status =/= ?CSR_STATUS_GRABBED andalso UserId =:= Request#im_csr.grabbedBy of
            true ->
              #'ErrorResp'{ref=Ref, code=?ERROR_CODE_PERMISSION_DENIED, message= <<"Invalid status to close grabbed request.">>};
            false ->
              Room = im_roster_muc:get(im_common:parse_id(Request#im_csr.roomId)),

              ctail:put(Room#im_grp{requestId=RequestId}),
              im_roster_muc:refresh(Room#im_grp.id),
              im_room:quit(#'QuitFromRoom'{roomId=Request#im_csr.roomId}, UserId),
              lists:foreach(fun(TargetUserId) ->
                {User, UpdatedUpdate} = im_chatupdate:quick_update(TargetUserId, ?MESSAGE_FEED_TYPE_ROOM, Request#im_csr.roomId, fun(Update, User) ->
                  ReplyKeyboardMarkup = undefined,
                  RemoveKeyboardMarkup = #'RemoveKeyboardMarkupEntity'{},
                  Update1 = Update#im_chat_update{
                    replyKeyboardMarkup=im_dto:parse_reply_keyboard_markup(ReplyKeyboardMarkup),
                    removeKeyboardMarkup=im_dto:parse_remove_keyboard_markup(RemoveKeyboardMarkup)
                  },
                  {{User, Update1}, Update1}
                end),
                im_chatupdate:send_update(User, UpdatedUpdate)
              end, Room#im_grp.members),

              Now = sm:now(),
              Request1 = Request#im_csr{status=?CSR_STATUS_CLOSED,
                updated=Now,
                feed_id=undefined,
                activity=Request#im_csr.activity ++ make_action(UserId, <<"close">>, Now),
                grabbedBy=undefined},

              ctail:put(Request1),

              #'CloseCsrResp'{ref=Ref, request=im_dto:format_csr(Request1)}
          end;
        false ->
          #'ErrorResp'{ref=Ref, code=?ERROR_CODE_PERMISSION_DENIED, message= <<"Only reporter or grabbed user can close request.">>}
      end;
    {error, _} ->
      #'ErrorResp'{ref=Ref, code=?ERROR_CODE_NOT_FOUND}
  end.

add_tag(#'AddTagCsr'{ref=Ref, id=RequestId, tag=Tag}, #im_usr{vendorId=VendorId}) ->
  case ctail:get(im_csr, im_common:parse_id(RequestId)) of
    {ok, Request} ->
      case im_common:format_id(Request#im_csr.vendorId) =:= im_common:format_id(VendorId) of
        true ->
          Request1 = Request#im_csr{updated=sm:now(),
            tags=im_common:list_unique(Request#im_csr.tags ++ [im_common:format_utf8(Tag)])},
          ctail:put(Request1),
          #'AddTagCsrResp'{ref=Ref, request=im_dto:format_csr(Request1)};
        false ->
          #'ErrorResp'{ref=Ref, code=?ERROR_CODE_PERMISSION_DENIED, message= <<"This request in other vendor scope">>}
      end;
    {error, _} ->
      #'ErrorResp'{ref=Ref, code=?ERROR_CODE_NOT_FOUND}
  end.

remove_tag(#'RemoveTagCsr'{ref=Ref, id=RequestId, tag=Tag}, #im_usr{vendorId=VendorId}) ->
  case ctail:get(im_csr, im_common:parse_id(RequestId)) of
    {ok, Request} ->
      case im_common:format_id(Request#im_csr.vendorId) =:= im_common:format_id(VendorId) of
        true ->
          Request1 = Request#im_csr{updated=sm:now(), tags=Request#im_csr.tags -- [im_common:format_utf8(Tag)]},
          ctail:put(Request1),
          #'RemoveTagCsrResp'{ref=Ref, request=im_dto:format_csr(Request1)};
        false ->
          #'ErrorResp'{ref=Ref, code=?ERROR_CODE_PERMISSION_DENIED, message= <<"This request in other vendor scope">>}
      end;
    {error, _} ->
      #'ErrorResp'{ref=Ref, code=?ERROR_CODE_NOT_FOUND}
  end.

make_action(UserId, Action, Date) ->
  [{<<"activity">>, [
    {<<"userId">>, UserId},
    {<<"action">>, im_common:format_utf8(Action)},
    {<<"date">>, Date}
  ]}].
