-module(im_room).

-include("im_common.hrl").

-export([create/2, new/2, topic/2, picture/2, kick/2, add/2, role/2, quit/2, delete/2]).
-export([get_marker/2, get_marker_time/2, upsert_marker/2, upsert_marker/3]).
-export([add_inner/2]).
-export([sys_msg/3]).

-define(MAX_TOPIC_SIZE, 255).

%% API

create(#'Room'{ref=Ref, room=Room}, UserId) ->
  create(Ref, Room, UserId).

create(Ref, Room, UserId) ->
  RoomItem = new(im_dto:parse_room(Room), UserId),
  #'RoomResp'{ref=Ref, room=im_dto:format_room(RoomItem)}.

new(RoomItem, UserId) ->
  Admins = case lists:member(UserId, RoomItem#im_grp.admins) of
    false -> RoomItem#im_grp.admins ++ [UserId];
    true  -> RoomItem#im_grp.admins
  end,

  RoomItem1 = RoomItem#im_grp{admins=Admins, topic=im_common:crop_str(RoomItem#im_grp.topic, ?MAX_TOPIC_SIZE)},

  Members = lists:foldl(fun(UId, Acc) ->
    case im_roster_chat:get(UId) of
      undefined -> Acc;
      U -> [U#im_usr.id|Acc]
    end
  end, [], RoomItem1#im_grp.members),

  % AllUserContacts = ctail_feed:get(im_contact, {contacts, UserId}, -1),
  % Members1 = lists:foldl(fun(Contact, Acc) ->
  %   case lists:member(Contact#im_contact.userId, Members) andalso ?CONTACT_STATUS_FRIEND =:= Contact#im_contact.status of
  %     true  -> [Contact#im_contact.userId|Acc];
  %     false -> Acc
  %   end
  % end, [], AllUserContacts),
  Members2 = Members -- [UserId] ++ [UserId],

  RoomItem2 = RoomItem1#im_grp{members=Members2, createdBy=UserId},

  {ok, RoomItem3} = im_roster:create_group(RoomItem2),
  RoomId = RoomItem3#im_grp.id,

  lists:foreach(fun(MemberId) ->
    ok = im_roster_muc:join(RoomId, MemberId),
    upsert_marker(MemberId, RoomId)
  end, RoomItem#im_grp.members ++ [UserId]),

  Room = im_roster_muc:get(RoomId),
  sys_msg(RoomId, <<"system.chat.created">>, []),
  Room.

topic(#'ChangeRoomTopic'{ref=Ref, roomId=RoomId, topic=Topic}, UserId) ->
  RoomId1 = im_common:parse_id(RoomId),
  RoomItem = im_roster_muc:get(RoomId1),

  case lists:member(UserId, RoomItem#im_grp.members) of
    false ->
      #'ErrorResp'{code=?ERROR_CODE_NOT_A_MEMBER,
        ref=Ref, messageParams=[],
        message=im_common:format_utf8(im_trans:t(<<"messaging.not.a.member">>))};
    true ->
      case lists:member(UserId, RoomItem#im_grp.admins) of
        false ->
          #'ErrorResp'{code=?ERROR_CODE_PERMISSION_DENIED,
            ref=Ref, messageParams=[],
            message=im_common:format_utf8(im_trans:t(<<"messaging.only.admins.can.kick.from.room">>))};
        true ->
          RoomItem1 = RoomItem#im_grp{topic=im_common:crop_str(Topic, ?MAX_TOPIC_SIZE), updated=sm:now()},
          update_room(RoomItem1),

          sys_msg(RoomId1, <<"system.user.changed.room.topic">>, [im_message:markup_user(UserId)]),

          #'ChangeRoomTopicResp'{ref=Ref, room=im_dto:format_room(RoomItem1)}
      end
  end.

picture(#'ChangeRoomPicture'{ref=Ref, roomId=RoomId, picture=Picture, thumbnail=Thumbnail}, UserId) ->
  RoomId1 = im_common:parse_id(RoomId),
  RoomItem = im_roster_muc:get(RoomId1),

  case lists:member(UserId, RoomItem#im_grp.members) of
    false ->
      #'ErrorResp'{code=?ERROR_CODE_NOT_A_MEMBER,
        ref=Ref, messageParams=[],
        message=im_common:format_utf8(im_trans:t(<<"messaging.not.a.member">>))};
    true ->
      case lists:member(UserId, RoomItem#im_grp.admins) of
        false ->
          #'ErrorResp'{code=?ERROR_CODE_PERMISSION_DENIED,
            ref=Ref, messageParams=[],
            message=im_common:format_utf8(im_trans:t(<<"messaging.only.admins.can.kick.from.room">>))};
        true ->
          RoomItem1 = RoomItem#im_grp{picture=Picture, thumbnail=Thumbnail, updated=sm:now()},
          update_room(RoomItem1),

          sys_msg(RoomId1, <<"system.user.changed.room.avatar">>, [im_message:markup_user(UserId)]),

          #'ChangeRoomPictureResp'{ref=Ref, room=im_dto:format_room(RoomItem1)}
      end
  end.

kick(#'KickFromRoom'{ref=Ref, roomId=RoomId, members=Members}, UserId) ->
  RoomId1 = im_common:parse_id(RoomId),
  RoomItem = im_roster_muc:get(RoomId1),

  case lists:member(UserId, RoomItem#im_grp.members) or is_valid_members(Members, RoomItem#im_grp.members) of
    false ->
      #'ErrorResp'{code=?ERROR_CODE_NOT_A_MEMBER,
        ref=Ref, messageParams=[],
        message=im_common:format_utf8(im_trans:t(<<"messaging.not.a.member">>))};
    true ->
      case lists:member(UserId, RoomItem#im_grp.admins) of
        false ->
          #'ErrorResp'{code=?ERROR_CODE_PERMISSION_DENIED,
            ref=Ref, messageParams=[],
            message=im_common:format_utf8(im_trans:t(<<"messaging.only.admins.can.kick.from.room">>))};
        true ->
          Members1 = [im_common:parse_id(MemberId) || MemberId <- Members],

          RoomItem1 = RoomItem#im_grp{members=RoomItem#im_grp.members -- Members1, updated=sm:now()},
          update_room(RoomItem1),
          update_chatupdates(RoomItem1, Members1, false),

          lists:foreach(fun(MemberId) ->
            im_chatupdate:quick_update(MemberId, ?MESSAGE_FEED_TYPE_ROOM, RoomId1, fun(Update, _User) ->
              Update1 = im_chatupdate:remove(Update),
              {none, Update1}
            end),

            sys_msg(RoomId1, <<"system.user.has.been.kicked.from.room">>, [im_message:markup_user(MemberId)]),

            {Key, Params} = case RoomItem#im_grp.topic =/= undefined of
              true -> {<<"system.user.has.been.kicked.from.room">>, [im_message:markup_user(UserId), im_common:format_utf8(RoomItem#im_grp.topic)]};
              false -> {<<"system.you.kicked.from.room.without.topic">>, [im_common:format_id(UserId)]}
            end,

            im_message:send_from_sys_user(MemberId, Key, Params)
          end, Members1),

          #'KickFromRoomResp'{ref=Ref, room=im_dto:format_room(RoomItem1)}
      end
  end.

add(#'AddToRoom'{ref=Ref, roomId=RoomId, members=Members}, UserId) ->
  RoomId1 = im_common:parse_id(RoomId),
  RoomItem = im_roster_muc:get(RoomId1),
  RoomMembers = RoomItem#im_grp.members,

  case lists:member(UserId, RoomMembers) of
    false ->
      #'ErrorResp'{code=?ERROR_CODE_NOT_A_MEMBER,
        ref=Ref, messageParams=[],
        message=im_common:format_utf8(im_trans:t(<<"messaging.not.a.member">>))};
    true ->
      case add_inner(RoomId, Members) of
        {ok, Room} -> #'AddToRoomResp'{ref=Ref, room=im_dto:format_room(Room)};
        _          -> #'ErrorResp'{code=?ERROR_CODE_UNKNOWN, message = <<"Failed to add user in room">>}
      end
  end.

role(#'ChangeRoleInRoom'{ref=Ref, roomId=RoomId, members=Members}, UserId) ->
  RoomId1 = im_common:parse_id(RoomId),
  RoomItem = im_roster_muc:get(RoomId1),

  case lists:member(UserId, RoomItem#im_grp.members) of
    false ->
      #'ErrorResp'{code=?ERROR_CODE_NOT_A_MEMBER,
        ref=Ref, messageParams=[],
        message=im_common:format_utf8(im_trans:t(<<"messaging.not.a.member">>))};
    true ->
      case lists:member(UserId, RoomItem#im_grp.admins) of
        false ->
          #'ErrorResp'{code=?ERROR_CODE_PERMISSION_DENIED,
            ref=Ref, messageParams=[],
            message=im_common:format_utf8(im_trans:t(<<"messaging.only.admins.can.kick.from.room">>))};
        true ->
          RoomItem1 = lists:foldl(fun(Member, Acc) ->
            MemberId1 = im_common:parse_id(Member#'ChangeRoleInRoomMember'.id),
            case Member#'ChangeRoleInRoomMember'.role of
              ?ROOM_MEMBER_ROLE_MEMBER ->
                case lists:member(MemberId1, Acc#im_grp.admins) andalso (length(Acc#im_grp.admins) > 0) of
                  true -> Acc#im_grp{admins=[AdminId || AdminId <- Acc#im_grp.admins, AdminId =/= MemberId1]};
                  false -> Acc
                end;
              ?ROOM_MEMBER_ROLE_ADMIN ->
                case lists:member(MemberId1, Acc#im_grp.admins) of
                  true -> Acc;
                  false ->
                    % sys_msg(RoomId1, <<"system.user.became.room.admin">>, [im_message:markup_user(MemberId1)]),
                    Acc#im_grp{admins=Acc#im_grp.admins ++ [MemberId1]}
                end
            end
          end, RoomItem, Members),

          update_room(RoomItem1),
          update_chatupdates(RoomItem1, [], true),

          #'ChangeRoleInRoomResp'{ref=Ref, room=im_dto:format_room(RoomItem1)}
      end
  end.

quit(#'QuitFromRoom'{ref=Ref, roomId=RoomId}, UserId) ->
  RoomId1 = im_common:parse_id(RoomId),
  RoomItem = im_roster_muc:get(RoomId1),

  case lists:member(UserId, RoomItem#im_grp.admins) andalso length(RoomItem#im_grp.admins) < 2 of
    true  ->
      #'ErrorResp'{code=?ERROR_CODE_CHANGE_ROOM_ADMIN,
        ref=Ref, messageParams=[],
        message=im_common:format_utf8(im_trans:t(<<"chat_change_admin_notification_before_delete">>))};
    false ->
      Members = lists:delete(UserId, RoomItem#im_grp.members),
      Admins = lists:delete(UserId, RoomItem#im_grp.admins),

      RoomItem1 = RoomItem#im_grp{members=Members, admins=Admins, updated=sm:now()},
      update_room(RoomItem1),

      im_chatupdate:quick_update(UserId, ?MESSAGE_FEED_TYPE_ROOM, RoomId1, fun(Update, _User) ->
        Update1 = im_chatupdate:remove(Update),
        {none, Update1}
      end),

      sys_msg(RoomId1, <<"system.user.quit.from.room">>, [im_message:markup_user(UserId)]),

      #'QuitFromRoomResp'{ref=Ref}
  end.

delete(#'DeleteRoom'{ref=Ref, roomId=RoomId}, UserId) ->
  RoomId1 = im_common:parse_id(RoomId),
  RoomItem = im_roster_muc:get(RoomId1),

  case lists:member(UserId, RoomItem#im_grp.members) of
    false ->
      #'ErrorResp'{code=?ERROR_CODE_NOT_A_MEMBER,
        ref=Ref, messageParams=[],
        message=im_common:format_utf8(im_trans:t(<<"messaging.not.a.member">>))};
    true ->
      case lists:member(UserId, RoomItem#im_grp.admins) of
        false ->
          #'ErrorResp'{code=?ERROR_CODE_PERMISSION_DENIED,
            ref=Ref, messageParams=[],
            message=im_common:format_utf8(im_trans:t(<<"messaging.only.admins.can.kick.from.room">>))};
        true ->
          case length(RoomItem#im_grp.members) > 1 of
            true  ->
              #'ErrorResp'{code=?ERROR_CODE_ROOM_MEMBERS_NOT_KICKED,
                ref=Ref, messageParams=[],
                message=im_common:format_utf8(im_trans:t(<<"messaging.only.room.without.members.can.be.deleted">>))};
            false ->
              sys_msg(RoomId1, <<"system.user.destroy.room">>, [im_message:markup_user(UserId)]),

              RoomItem1 = RoomItem#im_grp{updated=sm:now(), deleted=true},
              update_room(RoomItem1),

              {Update, User} = im_chatupdate:quick_update(UserId, ?MESSAGE_FEED_TYPE_ROOM, RoomId1, fun(Update, User) ->
                Update1 = im_chatupdate:remove(Update),
                {{Update1, User}, Update1}
              end),
              im_chatupdate:send_update(User, Update),

              #'DeleteRoomResp'{ref=Ref}
          end
      end
  end.

%% Internal

add_inner(RoomId, Members) ->
  RoomId1 = im_common:parse_id(RoomId),
  RoomItem = im_roster_muc:get(RoomId1),
  RoomMembers = RoomItem#im_grp.members,

  MappedMembers = lists:map(fun(Id) -> im_common:parse_id(Id) end, Members),
  IdsToJoin = lists:filter(fun(MemberId) -> not lists:member(MemberId, RoomMembers) end, MappedMembers),

  im_roster_muc:join(RoomId1, IdsToJoin),

  case IdsToJoin of
    [] -> {ok, RoomItem};
    _  ->
      RoomItem1 = im_roster_muc:get(RoomId1),
      RoomMembers1 = RoomMembers ++ IdsToJoin,
      RoomItem2 = RoomItem1#im_grp{members = RoomMembers1},
      lists:foreach(fun(MemberId) ->
        im_chatupdate:quick_update(MemberId, ?MESSAGE_FEED_TYPE_ROOM, RoomId1, fun(Update, _User) ->
          {none, Update#im_chat_update{deleted=false}}
        end),
        sys_msg(RoomId1, <<"system.user.has.been.added.to.room">>, [im_message:markup_user(MemberId)])
      end, IdsToJoin),
      update_chatupdates(RoomItem2),
      {ok, RoomItem2}
  end.

update_room(Room=#im_grp{id=RoomId}) ->
  Fun = fun(_) ->
    ctail:put(Room),
    {none, Room}
  end,
  im_roster_muc:execute(RoomId, Fun, []).

update_chatupdates(Room) -> update_chatupdates(Room, [], true).
update_chatupdates(Room=#im_grp{id=RoomId}, ExcludeUserIds, SendUpdatesToUsers) ->
  lists:foreach(fun(TargetUserId) ->
    {Update, User} = im_chatupdate:quick_update(TargetUserId, ?MESSAGE_FEED_TYPE_ROOM, RoomId, fun(Update, User) ->
      Update1 = Update#im_chat_update{name=Room#im_grp.topic, thumbnail=Room#im_grp.thumbnail},
      {{Update1, User}, Update1}
    end),
    case SendUpdatesToUsers =:= true of
      true -> im_chatupdate:send_update(User, Update);
      false -> skip
    end
  end, Room#im_grp.members -- ExcludeUserIds).

sys_msg(RoomId, MsgId, MsgArgs) ->
  im_message:send_sys_msg(?SYS_USER_ID, ?MESSAGE_FEED_TYPE_ROOM, RoomId, MsgId, MsgArgs).

upsert_marker(UserId, FeedId) -> upsert_marker(UserId, FeedId, sm:now()).
upsert_marker(UserId, FeedId, Time) ->
  ctail:put(#im_muc_history_marker{id={UserId, FeedId}, time=Time}).

get_marker_time(UserId, FeedId) ->
  case get_marker(UserId, FeedId) of
    undefined -> 0;
    Marker -> im_common:ensure_integer(Marker#im_muc_history_marker.time)
  end.

get_marker(UserId, FeedId) ->
  case ctail:get(im_muc_history_marker, {UserId, FeedId}) of
    {ok, Marker} -> Marker;
    _ -> undefined
  end.

is_valid_members(Members, RoomMembers) ->
  lists:all(fun(MemberId) -> lists:member(im_common:parse_id(MemberId), RoomMembers) end, Members).
