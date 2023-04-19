-module(im_transport_router).

-include("im_common.hrl").

-export([process/4, msg_needs_auth/1, get_version_number/0, parse_version_number/1, handle/4]).
-export([format_record/1]).

msg_needs_auth(Msg) ->
  AnonMessages = ['Pages', 'ClearState', 'User', 'RequestVerification', 'ConfirmVerification', 'SocialAuth',
    'AddFeedback', 'ChangeCronTime', 'UserRoles', 'RtcMessage'],

  NonVersionedMsg = remove_record_version(Msg),
  lists:member(element(1, NonVersionedMsg), AnonMessages) =/= true.

process(Msg, User, TokenRec, Version) ->
  case ets:info(protocol_version) =:= undefined of
    true ->
      ets:new(protocol_version, [set, named_table, protected]),
      ets:insert(protocol_version, {Version});
    false -> skip
  end,

  UserId = case User of
    #im_usr{id=UId} -> UId;
    _               -> undefined
  end,

  Result = case UserId =:= undefined of
    true ->
      case msg_needs_auth(Msg) of
        false -> handle(Msg, Version, undefined, undefined);
        true -> {ok, #'ErrorResp'{code = ?ERROR_CODE_PERMISSION_DENIED, message="Anon users are not permitted to send this message"}, #sm_response{status = 401}}
      end;
    false ->
      IsBlocked = User#im_usr.active =/= true,
      case IsBlocked of
        true ->
          {ok, #'ErrorResp'{
            code = ?ERROR_CODE_BLOCKED_BY_SYSTEM,
            ref = 0, messageParams = [],
            messageType = <<"user.user.blocked.by.system">>,
            message = im_common:format_utf8(im_trans:t(<<"user.user.blocked.by.system">>))}, #sm_response{}};
        false ->
          HasRights = acl_filter(Msg, UserId),
          case HasRights of
            true ->
              IsExistingUser = User#im_usr.isNew =:= false orelse User#im_usr.isNew =:= undefined,
              IsBotUser = im_common:ensure_boolean(User#im_usr.isBot),
              case IsExistingUser or IsBotUser of
                true ->
                  handle(Msg, Version, User, TokenRec);
                false ->
                  case Msg of
                    #'ClearState'{}     -> handle(Msg, Version, undefined, undefined);
                    #'Pages'{}          -> handle(Msg, Version, undefined, undefined);
                    #'User'{}           -> handle(Msg, Version, User, TokenRec);
                    #'UpdateUser'{}     -> handle(Msg, Version, User, TokenRec);
                    #'UploadContacts'{} -> handle(Msg, Version, User, TokenRec);
                    _ -> #'ErrorResp'{
                      code = ?ERROR_CODE_REGISTRATION_NOT_FINISH,
                      ref = 0, messageParams = [],
                      messageType = <<"auth.you.need.finish.registration">>,
                      message = im_common:format_utf8(im_trans:t(<<"auth.you.need.finish.registration">>))}
                  end
              end;
            false ->
              #'ErrorResp'{
                ref = element(2, Msg),
                code = ?ERROR_CODE_PERMISSION_DENIED,
                message = "ACL filter failed"
              }
          end
      end
  end,

  Response = case Result of
    undefined -> #'ErrorResp'{code=?ERROR_CODE_INVALID_MESSAGE};
    _         -> Result
  end,

  log(Version, Msg, Response, User),

  Response.

handle(Message, Version, User, TokenInfo) ->
  MinVersion = 7,
  MaxVersion = 7,
  IsBaseMessage = case is_list(Version) of
    true ->
      VersionNumber = im_common:ensure_integer(list_to_integer(Version)),
      case VersionNumber =< MaxVersion andalso VersionNumber >= MinVersion of
        true -> false;
        false -> true
      end;
    false -> true
  end,
  Message1 = case IsBaseMessage of
    true  -> Message;
    false -> parse_record(Version, Message)
  end,

  % im_logger:debug(case User of
  %   #im_usr{id=Uid} -> Uid;
  %   _ -> undefined
  % end, "IsBaseMessage: ~p ~p ~p", [IsBaseMessage, Version, Message1]),

  case handle_message(Message1, Version, User, TokenInfo) of
    not_found ->
      case IsBaseMessage of
        true ->
          {ok, #'ErrorResp'{code=?ERROR_CODE_INVALID_MESSAGE, message="No version provided"}};
        false ->
          handle(Message, integer_to_list(im_common:ensure_integer(list_to_integer(Version)) - 1), User, TokenInfo)
      end;
    R ->
      case R of
        none                          -> {ok, none};
        {ok, ProcessedData}           -> {ok, format_record(ProcessedData)};
        {ok, ProcessedData, Response} -> {ok, format_record(ProcessedData), Response};
        ProcessedData                 -> {ok, format_record(ProcessedData)}
      end
  end.

handle_message(Msg, _Version, undefined, undefined) ->
  case Msg of
    #'RtcMessage'{}          -> im_rtc_communicator:receive_message(Msg);
    #'SocialAuth'{}          -> im_auth:social(Msg);
    #'Pages'{}               -> im_page:get(Msg);
    #'ClearState'{}          -> im:clear_state(Msg);
    #'RequestVerification'{} -> im_auth:request_verification(Msg);
    #'ConfirmVerification'{} -> im_auth:confirm_verification(Msg);
    #'AddFeedback'{}         -> im_feedback:add(Msg);
    #'ChangeCronTime'{}      -> im_cron:change_cron_time(Msg);
    #'UserRoles'{}           -> im_acl:get(Msg);
    _                        -> not_found
  end;
handle_message(Msg, Version, User, TokenInfo) ->
  UserId = User#im_usr.id,
  case Msg of
    #'ClearState'{}              -> im:clear_state(Msg);

    #'Pages'{}                   -> im_page:get(Msg);

    #'AddPushToken'{}            -> im_device_api:add_token(Msg, UserId);

    #'Status'{}                  -> im_usr:status(Msg, UserId);
    #'Statuses'{}                -> im_usr:statuses(Msg, UserId);

    #'ChatUpdates'{}             -> im_chatupdate_api:get(Msg, UserId);
    #'MarkAsRead'{}              -> im_chatupdate_api:mark_as_read(Msg, UserId);
    #'DeleteChat'{}              -> im_chatupdate_api:delete(Msg, UserId);

    #'Message'{}                 -> im_message_api:send(Msg, UserId);
    #'Updates'{}                 -> im_message_api:get_updates(Msg, UserId);
    #'EditMessage'{}             -> im_message_api:edit(Msg, UserId);
    #'Typing'{}                  -> im_message_api:typing(Msg, UserId);
    #'Delete'{}                  -> im_message_api:delete(Msg, UserId);
    #'Star'{}                    -> im_message_api:star(Msg, UserId);
    #'GetStarred'{}              -> im_message_api:get_starred(Msg, UserId);

    #'Retrieve'{}                -> im_retrieve_api:retrieve(Msg, UserId);
    #'Pointer'{}                 -> im_pointer:update(Msg, UserId);

    #'Room'{}                    -> im_room:create(Msg, UserId);
    #'ChangeRoomTopic'{}         -> im_room:topic(Msg, UserId);
    #'ChangeRoomPicture'{}       -> im_room:picture(Msg, UserId);
    #'KickFromRoom'{}            -> im_room:kick(Msg, UserId);
    #'AddToRoom'{}               -> im_room:add(Msg, UserId);
    #'ChangeRoleInRoom'{}        -> im_room:role(Msg, UserId);
    #'QuitFromRoom'{}            -> im_room:quit(Msg, UserId);
    #'DeleteRoom'{}              -> im_room:delete(Msg, UserId);

    #'UserSetting'{}             -> im_user_settings:set(Msg, UserId);
    #'UserSettings'{}            -> im_user_settings:get(Msg, UserId);

    #'Calls'{}                   -> im_call_api:get(Msg, UserId);
    #'Call7'{}                   -> im_call_api:get_one(Msg, UserId);
    #'InitCall'{}                -> im_call_api:init(Msg, UserId, TokenInfo);
    #'JoinCall7'{}               -> im_call_api:join(Msg, UserId);
    #'DeclineCall7'{}            -> im_call_api:decline(Msg, UserId);
    #'LeaveCall7'{}              -> im_call_api:leave(Msg, UserId);
    #'ReceiveMediaFrom7'{}       -> im_call_api:receive_media_from(Msg, UserId);
    #'IceCandidate7'{}           -> im_call_api:ice_candidate(Msg, UserId);

    #'CreateChannel'{}           -> im_channel:create_channel(Msg, UserId);
    #'PublishPost'{}             -> im_channel:publish_post(Msg, UserId);
    #'GetPosts'{}                -> im_channel:get_posts(Msg, UserId);

    #'FetchOgData'{}             -> im_og:fetch(Msg, UserId);

    #'GetChannels'{}             -> im_channel:get_channels(Msg, UserId);
    #'GetChannelCategories'{}    -> im_channel:get_channel_categories(Msg, UserId);
    #'LikeChannelPost'{}         -> im_channel:like_post(Msg, UserId);
    #'UnLikeChannelPost'{}       -> im_channel:unlike_post(Msg, UserId);
    #'GetChannelPostThread'{}    -> im_channel:get_post_thread(Msg, UserId);
    #'CommentChannelPost'{}      -> im_channel:add_comment_post(Msg, UserId);
    #'SubscribeChannel'{}        -> im_channel:subscribe_channel(Msg, UserId);
    #'UnsubscribeChannel'{}      -> im_channel:unsubscribe_channel(Msg, UserId);
    #'GetMySubscribedChannels'{} -> im_channel:get_my_channels(Msg, UserId);
    #'GetMyChannels'{}           -> im_channel:my_channels(Msg, UserId);

    #'FeedPost'{}                -> im_feed_post:get(Msg, UserId);
    #'FeedPostByLocation'{}      -> im_feed_post:get_by_location(Msg, UserId);
    #'FeedPostCreate'{}          -> im_feed_post:create(Msg, UserId);
    #'FeedPostUpdate'{}          -> im_feed_post:update(Msg, UserId);
    #'FeedPostDelete'{}          -> im_feed_post:delete(Msg, UserId);
    #'FeedPostTag'{}             -> im_feed_post:get_tags(Msg, UserId);
    #'FeedPostCategory'{}        -> im_feed_post:get_categories(Msg, UserId);
    #'FeedPostCategoryCreate'{}  -> im_feed_post:create_category(Msg, UserId);
    #'FeedPostCategoryUpdate'{}  -> im_feed_post:update_category(Msg, UserId);
    #'FeedPostCategoryDelete'{}  -> im_feed_post:delete_category(Msg, UserId);

    #'Like'{}                    -> im_like:like(Msg, UserId);
    #'Dislike'{}                 -> im_like:dislike(Msg, UserId);

    #'LocalizedStore'{}          -> im_localized_store:get(Msg, UserId);
    #'LocalizedStoreCreate'{}    -> im_localized_store:create(Msg, UserId);
    #'LocalizedStoreUpdate'{}    -> im_localized_store:update(Msg, UserId);
    #'LocalizedStoreDelete'{}    -> im_localized_store:delete(Msg, UserId);

    #'FindUser'{}                -> im_elastic_search:find_user(Msg, UserId);

    #'Bots'{}                    -> im_bot_api:get(Msg, UserId);
    #'Bot'{}                     -> im_bot_api:get_one(Msg, UserId);
    #'AddBot'{}                  -> im_bot_api:add(Msg, UserId);
    #'BotCallbackQuery'{}        -> im_bot_api:callback(Msg, UserId);
    #'BotInlineQuery'{}          -> im_bot_api:query(Msg, UserId);
    #'BotSendMessage'{}          -> im_bot_api:send(Msg, UserId);
    #'BotSendSms'{}              -> im_bot_api:send_sms(Msg, UserId);
    #'ExecAsUser'{}              -> im_bot_api:exec_as(Msg, Version, TokenInfo);

    #'Task'{}                    -> im_task:get(Msg, UserId);
    #'CreateTask'{}              -> im_task:create(Msg, UserId);
    #'UpdateTask'{}              -> im_task:update(Msg, UserId);
    #'AssignTask'{}              -> im_task:assign(Msg, UserId);
    #'UpdateTaskStatus'{}        -> im_task:status(Msg, UserId);
    #'DeleteTask'{}              -> im_task:delete(Msg, UserId);

    #'UploadMedia'{}             -> im_uploader:upload(Msg, UserId);

    #'SuggestLocation'{}         -> im_gmaps:suggest_location(Msg, UserId);
    #'LocationByGps'{}           -> im_gmaps:location_by_gps(Msg, UserId);
    #'Department'{}              -> im_department:get(Msg, UserId);

    #'CreateOrder'{}             -> im_product:create_order(Msg, UserId);
    #'Orders'{}                  -> im_product:get_orders(Msg, UserId);
    #'Order'{}                   -> im_product:get_order(Msg, UserId);
    #'DeleteOrder'{}             -> im_product:delete_order(Msg, UserId);
    #'ChangeOrder'{}             -> im_product:change_order(Msg, UserId);
    #'ChargeOrder'{}             -> im_product:charge_order(Msg, UserId);
    #'UseOrder'{}                -> im_product:use_order(Msg, UserId);
    #'VendorStatistics'{}        -> im_statistics:get_vendor_statistics(Msg, UserId);
    #'VendorUsers'{}             -> im_usr:get_vendor_users(Msg, User);

    #'Devices'{}                 -> im_device_api:get(Msg, UserId);
    #'RemoveDevice'{}            -> im_device_api:remove(Msg, UserId);

    #'Contact'{}                 -> im_contact_api:get(Msg, User);
    #'SyncContacts'{}            -> im_contact_api:sync_contacts(Msg, User);
    #'AddContact'{}              -> im_contact_api:add(Msg, User);
    #'DeleteContact'{}           -> im_contact_api:delete(Msg, User);
    #'BlockContact'{}            -> im_contact_api:block(Msg, User);
    #'UnBlockContact'{}          -> im_contact_api:unblock(Msg, User);
    #'AddToSpam'{}               -> im_contact_api:add_to_spam(Msg, User);
    #'UpdateContact'{}           -> im_contact_api:update(Msg, User);

    #'AddFeedback'{}             -> im_feedback:add(Msg, User);
    #'AddReport'{}               -> im_report:add(Msg, User);
    #'AddReportWithLocale'{}     -> im_report:add_with_locale(Msg, User);

    #'Logout'{}                  -> im_auth:logout(Msg, User, TokenInfo);
    #'User'{}                    -> im_usr:get(Msg, User, TokenInfo);
    #'UpdateUser'{}              -> im_usr:update(Msg, User, TokenInfo);
    #'UploadContacts'{}          -> im_contact_api:upload(Msg, User, TokenInfo);
%%    #'SubmitLocation'{}         -> im_gmaps:submit_location(Msg, UserId);

    #'LiberateSynapse'{}         -> im_synapse:liberate(Msg, UserId);
    #'GetCircle'{}               -> im_circle:get(Msg, UserId);

    #'OpenCsr'{}                 -> im_csr:get_open(Msg, User);
    #'RoomCsr'{}                 -> im_csr:room_csr(Msg, User);
    #'CreateCsr'{}               -> im_csr:create(Msg, User);
    #'GrabCsr'{}                 -> im_csr:grab(Msg, User);
    #'DropCsr'{}                 -> im_csr:drop(Msg, User);
    #'CloseCsr'{}                -> im_csr:close(Msg, User);
    #'AddTagCsr'{}               -> im_csr:add_tag(Msg, User);
    #'RemoveTagCsr'{}            -> im_csr:remove_tag(Msg, User);

    #'GetWorkflowState'{}        -> im_workflow:get_state(Msg, UserId);
    #'StartWorkflow'{}           -> im_workflow:start(Msg, UserId);
    #'TransitWorkflow'{}         -> im_workflow:transit(Msg, UserId);
    #'GetWorkflow'{}             -> im_workflow:get(Msg, UserId);
    #'CreateWorkflow'{}          -> im_workflow:create(Msg, UserId);

    #'UserRoles'{}               -> im_acl:get(Msg);

    _                            -> not_found
  end.

acl_filter(Msg, UserId) ->
  % manage_products
  % manage_orders
  Map = #{
    'MessageRevisions'       => {["super_admin"], []},
    'FeedPostCreate'         => {[], ["manage_products"]},
    'FeedPostUpdate'         => {[], ["manage_products"]},
    'FeedPostDelete'         => {[], ["manage_products"]},
    'LocalizedStoreCreate'   => {[], ["manage_products"]},
    'LocalizedStoreUpdate'   => {[], ["manage_products"]},
    'LocalizedStoreDelete'   => {[], ["manage_products"]},
    'FeedPostCategoryCreate' => {[], ["manage_products"]},
    'FeedPostCategoryUpdate' => {[], ["manage_products"]},
    'FeedPostCategoryDelete' => {[], ["manage_products"]},
    'OpenCsr'                => {[], ["manage_csr"]},
    'RoomCsr'                => {[], ["manage_csr"]},
    'CreateCsr'              => {[], ["manage_csr"]},
    'GrabCsr'                => {[], ["manage_csr"]},
    'DropCsr'                => {[], ["manage_csr"]},
    'CloseCsr'               => {[], ["manage_csr"]}
  },
  %% TODO: strip version number
  MsgName = element(1, Msg),
  case maps:find(MsgName, Map) of
    error                -> true;
    {ok, {Roles, Perms}} -> im_acl:has_role_one_of(Roles, UserId) orelse im_acl:has_perm_one_of(Perms, UserId)
  end.

get_version_number() ->
  case ets:info(protocol_version) =/= undefined of
    true ->
      case ets:last(protocol_version) of
        "" -> undefined;
        Value when is_list(Value) ->
          try
            list_to_integer(Value)
          catch error:badarg ->
            undefined
          end;
        _ -> undefined
      end;
    false -> undefined
  end.

parse_version_number(Req) ->
  Path = case cowboy_req:binding(version, Req) of
    undefined                                      -> undefined;
    {<<>>, Req}                                    -> undefined;
    {PathVersion, Req} when is_binary(PathVersion) -> binary_to_list(PathVersion);
    _                                              -> undefined
  end,

  Header = case cowboy_req:parse_header(<<"version">>, Req) of
    {ok, undefined, _} -> undefined;
    {ok, HeaderV, _} when is_binary(HeaderV) -> binary_to_list(HeaderV);
    _ -> undefined
  end,

  QueryString = case cowboy_req:qs_val(<<"version">>, Req) of
    {undefined, _} -> undefined;
    {QueryStringV, _} when is_binary(QueryStringV) -> binary_to_list(QueryStringV);
    _ -> undefined
  end,

  Result = case Path of
    undefined -> case Header of
      undefined -> case QueryString of
        undefined -> undefined;
        Version -> Version
      end;
      Version -> Version
    end;
    Version -> Version
  end,
  case Result of
    "v1" -> undefined;
    undefined -> undefined;
    _ ->
      [H|T] = Result,
      case H =:= 118 orelse H =:= 86 orelse H =:= "v" orelse H =:= "V" of
        true -> T;
        false -> undefined
      end
  end.

parse_record(Version, Msg) ->
  R = modify_record_name_nested(Msg, fun(M) ->
    list_to_atom(atom_to_list(element(1, M)) ++ Version)
  end),
%%  io:format("Parse record ~nSource: ~p~nResult: ~p", [Msg, R]),
  R.

format_record(none) -> none;
format_record(Msg) when is_tuple(Msg)->
  R = modify_record_name_nested(Msg, fun(M) ->
    case element(1, M) of
      V when is_atom(V) -> list_to_atom(re:replace(atom_to_list(element(1, M)), "[0-9]*$", "", [{return,list}]));
      V                 -> V
    end
  end),
%%  io:format("Format record ~nSource: ~p~nResult: ~p", [Msg, R]),
  R.

remove_record_version(Msg) ->
  MsgName = list_to_atom(re:replace(atom_to_list(element(1, Msg)), "[0-9]*$", "", [{return,list}])),
  list_to_tuple([MsgName] ++ tl(tuple_to_list(Msg))).

modify_record_name_nested(Msg, Predicate) when is_tuple(Msg) ->
  Msg1 = setelement(1, Msg, Predicate(Msg)),
  list_to_tuple(lists:map(fun(Item) -> modify_record_name_nested(Item, Predicate) end, tuple_to_list(Msg1)));
modify_record_name_nested(Item, Predicate) when is_list(Item) -> lists:map(fun(I) -> modify_record_name_nested(I, Predicate) end, Item);
modify_record_name_nested(Item, _) -> Item.

log(Version, RequestMsg, Response, User) ->
  Func = fun() ->
    UserId = case User of
      #im_usr{id=UId} -> UId;
      _               -> undefined
    end,

    UserName = case User of
      #im_usr{name=UN} -> im_common:ensure_list(UN);
      _                -> "undefined"
    end,

    ResponseMsg = case Response of
      {ok, R} -> R;
      {ok, R, _} -> R;
      R -> R
    end,

    case element(1, ResponseMsg) of
      'ErrorResp' ->
        im_logger:error(UserId, "[im_transport_router][~p][~p][~p][~p code: ~p message: ~p] ", [
          UserName,
          Version,
          element(1, RequestMsg),
          element(1, ResponseMsg),
          ResponseMsg#'ErrorResp'.code,
          ResponseMsg#'ErrorResp'.message
        ]);
      _ ->
        im_logger:debug(UserId, "[im_transport_router][~p][~p][~p][~p]", [
          UserName,
          Version,
          element(1, RequestMsg),
          element(1, ResponseMsg)
        ])
    end
  end,

  case Response of
    {ok, none} -> skip;
    _ ->
      try Func() of _ -> ok
      catch
        error:_ -> im_logger:error(undefined, "Cannot write log ~p", [Response])
      end
  end.
