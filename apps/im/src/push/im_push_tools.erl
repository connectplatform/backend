-module(im_push_tools).

-include("im_common.hrl").

-export([format_message_alert/2, get_chat_name/3]).

format_message_alert(UserId, #im_msg{feed_id=Feed, type=Type, kind=Kind, payload=Payload, callId=CallId,
  systemMessageType=SystemMessageType, systemMessageParams=SystemMessageParams}) ->
  {FeedType, FeedId} = im_chatupdate:roster_to_msg_feed(Feed, UserId),
  case Kind =:= ?MESSAGE_KIND_CALL of
    true -> format_alert_call(UserId, FeedType, FeedId, CallId);
    false ->
      case Type of
        ?MESSAGE_TYPE_USER_MESSAGE        -> format_alert_normal(UserId, FeedType, FeedId, Kind, Payload);
        ?MESSAGE_TYPE_SYSTEM_NOTIFICATION -> format_alert_system(UserId, FeedType, FeedId, SystemMessageType, SystemMessageParams)
      end
  end.

format_alert_call(UserId, FeedType, FeedId, CallId) ->
  case im_call_sup:is_missed(CallId, UserId) of
    true ->
      {ChatName, _} = get_chat_name(UserId, FeedType, FeedId),
      im_trans:t(im_locale:get(UserId), <<"push.message.call.missed">>, [ChatName]);
    false -> undefined
  end.

format_alert_normal(UserId, FeedType, FeedId, Kind, Payload) ->
  case FeedType of
    ?MESSAGE_FEED_TYPE_CHAT ->
      {Name, _} = get_chat_name(UserId, FeedType, FeedId),
      format_alert_normal_chat(UserId, Kind, Payload, Name);
    ?MESSAGE_FEED_TYPE_ROOM ->
      Group = im_roster_muc:get(FeedId),
      Topic = im_common:format_utf8(Group#'im_grp'.topic),
      case Topic =:= undefined orelse Topic =:= <<>> of
        true -> format_alert_normal_room(UserId, Kind, Payload, undefined);
        false -> format_alert_normal_room(UserId, Kind, Payload, Topic)
      end
  end.

format_alert_normal_chat(UserId, Kind, Payload, Name) ->
  case Kind of
    ?MESSAGE_KIND_TEXT ->
      case Name =/= undefined of
        true ->
          case Payload =/= undefined of
            true -> <<Name/binary, ": ", Payload/binary>>;
            false -> Name
          end;
        false -> Payload
      end;
    _ ->
      Key = case Kind of
        ?MESSAGE_KIND_AUDIO    -> "audio";
        ?MESSAGE_KIND_DOCUMENT -> "document";
        ?MESSAGE_KIND_IMAGE    -> "image";
        ?MESSAGE_KIND_MAP      -> "map";
        ?MESSAGE_KIND_STICKER  -> "sticker";
        ?MESSAGE_KIND_VIDEO    -> "video"
      end,
      im_trans:t(im_locale:get(UserId), im_common:format_utf8("push.message." ++ Key), [Name])
  end.

format_alert_normal_room(UserId, Kind, Payload, undefined) ->
  case Kind of
    ?MESSAGE_KIND_TEXT ->
        im_trans:t(im_locale:get(UserId), <<"group_without_topic.push.message">>, Payload);
    _ ->
      Key = case Kind of
        ?MESSAGE_KIND_AUDIO    -> "audio";
        ?MESSAGE_KIND_DOCUMENT -> "document";
        ?MESSAGE_KIND_IMAGE    -> "image";
        ?MESSAGE_KIND_MAP      -> "map";
        ?MESSAGE_KIND_STICKER  -> "sticker";
        ?MESSAGE_KIND_VIDEO    -> "video"
      end,
      im_trans:t(im_locale:get(UserId), im_common:format_utf8("group_without_topic.push.message." ++ Key))
  end;
format_alert_normal_room(UserId, Kind, Payload, Name) ->
  format_alert_normal_chat(UserId, Kind, Payload, Name).

format_alert_system(_UserId, _FeedType, _FeedId, _SystemMessageType, []) -> undefined;
format_alert_system(UserId, FeedType, FeedId, SystemMessageType, SystemMessageParams) when is_list(SystemMessageParams) ->
  case SystemMessageParams of
    [FirstParam|_] ->
      Fun = fun() ->
        case im_common:parse_id(binary:part(im_common:ensure_binary(FirstParam), 6, 24)) of
          undefined ->
            undefined;
          UserIdParam ->
            {Params, TransKey} = case FeedType of
              ?MESSAGE_FEED_TYPE_CHAT ->
                {ChatName, _} = get_chat_name(UserId, FeedType, FeedId),
                {[ChatName], SystemMessageType};
              ?MESSAGE_FEED_TYPE_ROOM ->
                Group = im_roster_muc:get(FeedId),
                Topic = im_common:format_utf8(Group#'im_grp'.topic),
                ContactName = get_group_user_name(UserId, UserIdParam),
                Params1 = case Topic of
                  undefined -> [ContactName];
                  _         -> [Topic, ContactName]
                end,
                get_group_system_push_key(Topic, SystemMessageType, Params1)
            end,
            im_trans:t(im_locale:get(UserId), TransKey, Params)
        end
      end,
      try Fun() catch _:_ -> undefined end;
    _ ->
      undefined
  end.

get_group_user_name(UId, UserId) ->
  case im_contact:find_contact(UId, UserId) of
    {ok, Contact} ->
      Contact#im_contact.name;
    _ ->
      case im_roster_chat:get(UserId) of
        undefined -> undefined;
        User -> User#im_usr.name
      end
  end.

get_group_system_push_key(undefined, SystemMessageType, Params) ->
  case SystemMessageType of
    <<"system.user.changed.room.avatar">>       -> {[], <<"group_without_topic_system.user.changed.room.avatar">>};
    <<"system.user.has.been.added.to.room">>    -> {Params, <<"group_without_topic_system.has.been.added.to.room">>};
    <<"system.user.has.been.kicked.from.room">> -> {Params, <<"group_without_topic_system.has.been.kicked.from.room">>};
    <<"system.user.quit.from.room">>            -> {Params, <<"group_without_topic_system.quit.from.room">>};
    _                                           -> {Params, SystemMessageType}
  end;
get_group_system_push_key(_, SystemMessageType, Params) ->
  [H|T] = Params,
  case SystemMessageType of
    <<"system.user.changed.room.avatar">>       -> {[H], <<"group_system.user.changed.room.avatar">>};
    <<"system.user.has.been.added.to.room">>    -> {Params, <<"group_system.has.been.added.to.room">>};
    <<"system.user.has.been.kicked.from.room">> -> {Params, <<"group_system.has.been.kicked.from.room">>};
    <<"system.user.quit.from.room">>            -> {Params, <<"group_system.quit.from.room">>};
    _                                           -> {[T], SystemMessageType}
  end.

get_chat_name(UserId, FeedType, FeedId) ->
  {NameInUpdate, Thumbnail} = case im_chatupdate:quick_find(FeedType, FeedId, UserId) of
    {ok, Update} -> {Update#im_chat_update.name, Update#im_chat_update.thumbnail};
    _            -> {undefined, undefined}
  end,
  case NameInUpdate =/= undefined andalso NameInUpdate =/= <<>> of
    true ->
      {NameInUpdate, Thumbnail};
    false ->
      case FeedType of
        ?MESSAGE_FEED_TYPE_CHAT ->
          case im_roster_chat:get(FeedId) of
            undefined -> {undefined, undefined};
            Sender -> {Sender#im_usr.name, Sender#im_usr.thumbnail}
          end;
        ?MESSAGE_FEED_TYPE_ROOM ->
          Room = im_roster_muc:get(FeedId),
          {Room#im_grp.topic, Room#im_grp.thumbnail}
      end
  end.
