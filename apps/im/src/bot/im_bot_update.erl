-module(im_bot_update).

-include("im_common.hrl").

-export([send_update/3]).
-export([send_message_update/6, send_system_message/2]).
-export([send_callback_query_update/6, send_inline_query_update/7]).
-export([send_circle_update/4]).

send_update(BotId, Update, UserId) ->
  im_bot_worker:send(BotId, im_transport_router:format_record(Update), UserId).

send_message_update(BotId, BotUserId, UserId, FeedType, FeedId, Message) ->
  ChatUpdate = im_chatupdate:wait_update(BotUserId, FeedId),
  Update = #'BotUpdate7'{
    feedType=FeedType,
    feedId=im_common:format_id(FeedId),
    userId=im_common:format_id(UserId),
    message=im_dto:format_message(UserId, Message),
    meta=im_message:parse_msg_meta(Message),
    chat=im_dto:format_chat_update(BotUserId, ChatUpdate)
  },
  send_update(BotId, Update, UserId).

send_circle_update(BotId, UserId, RoomId, IsNewCircle) ->
  im_logger:debug(UserId, "[BotUpdate] Sending update: bot = ~p, user = ~p, room = ~p, isNew = ~p",
    [im_common:format_id(BotId), im_common:format_id(UserId), im_common:format_id(RoomId), IsNewCircle]),

  case im:is_test() of
    true ->
      {ok, Bot} = ctail:get(im_bot, BotId),
      Message = #im_msg{
        type=?MESSAGE_TYPE_USER_MESSAGE,
        kind=?MESSAGE_KIND_TEXT,
        payload= <<"Welcome to circle!">>
      },
      im_message:send(Bot#im_bot.userId, ?MESSAGE_FEED_TYPE_ROOM, RoomId, Message);
    false ->
      Room = im_roster_muc:get(RoomId),
      Json = [{<<"roomId">>, im_common:format_id(RoomId)},
        {<<"params">>, Room#im_grp.circleParams},
        {<<"isNew">>, IsNewCircle}],

      Update = #'BotUpdate7'{
        feedType=?MESSAGE_FEED_TYPE_ROOM,
        feedId=im_common:format_id(RoomId),
        userId=im_common:format_id(UserId),
        serverMessage=#'ServerMessageEntity7'{action="CircleCreatedEvent", jsonData=jsx:encode(Json)}
      },
      send_update(BotId, Update, UserId)
  end.

send_system_message(Msg, UserId) ->
  % im_logger:debug(UserId, "[BotUpdate] Sending system message: ~p", [Msg]),

  try format_system_message(Msg) of
    {ok, ServerMessage}  ->
      lists:foreach(fun(Bot) ->
        ServerMsgId = ctail:next_id(),
        ServerMsgEntity = ServerMessage#'ServerMessageEntity7'{id=im_common:format_id(ServerMsgId)},
        Update = #'BotUpdate7'{
          feedType=?MESSAGE_FEED_TYPE_CHAT,
          feedId=im_common:format_id(UserId),
          userId=im_common:format_id(UserId),
          serverMessage=ServerMsgEntity
        },
        ServerMsg = #im_bot_server_msg{id=ServerMsgId,
          feed_id={<<"botServerMessage">>, Bot#im_bot.id},
          action=im_common:format_utf8(ServerMsgEntity#'ServerMessageEntity7'.action),
          jsonData=im_common:format_utf8(ServerMsgEntity#'ServerMessageEntity7'.jsonData)},
        ctail_feed:add(ServerMsg),
        send_update(Bot#im_bot.id, Update, UserId)
      end, im_bot:get_sys_bots());
    {error, not_found} ->
      im_logger:error(undefined, "[Bot] unknown message ~p", [Msg])
  catch
    error:_ -> im_logger:error(undefined, "[Bot] Cannot parse server message ~p", [Msg])
  end.

format_system_message({workflow_state_changed, #im_workflow_state{name=Name, target=Target, entityId=EntityId, step=Step, active=Active, json=Json}}) ->
  {ok, StateData} = yaws_json2:decode_string(im_common:ensure_list(Json)),
  SendJson = {struct, [
    {<<"name">>, Name},
    {<<"target">>, Target},
    {<<"entityId">>, im_common:format_id(EntityId)},
    {<<"step">>, Step},
    {<<"state">>, StateData},
    {<<"active">>, Active}
  ]},
  {ok, #'ServerMessageEntity7'{
    action="WorkflowStateChangedEvent",
    jsonData=SendJson
  }};
format_system_message({user_updated, User=#im_usr{}}) ->
  {ok, #'ServerMessageEntity7'{
    action="UserUpdatedEvent",
    jsonData=im_dto:user_to_json(User)
  }};
format_system_message({user_registered, User=#im_usr{}}) ->
  {ok, #'ServerMessageEntity7'{
    action="UserRegisteredEvent",
    jsonData=im_dto:user_to_json(User)
  }};
format_system_message(_) -> {error, not_found}.

send_callback_query_update(BotId, UserId, FeedType, FeedId, Message, Data) ->
  Update = #'BotUpdate7'{
    feedType=FeedType,
    feedId=im_common:format_id(FeedId),
    userId=im_common:format_id(UserId),
    message=im_dto:format_message(UserId, Message),
    callbackQuery=#'CallbackQueryEntity'{data=im_common:format_utf8(Data)}
  },
  send_update(BotId, Update, UserId).

send_inline_query_update(BotId, UserId, FeedType, FeedId, Id, Query, Offset) ->
  Update = #'BotUpdate7'{
    feedType=FeedType,
    feedId=im_common:format_id(FeedId),
    userId=im_common:format_id(UserId),
    inlineQuery=#'InlineQueryEntity'{id=Id, query=im_common:format_utf8(Query), offset=im_common:format_utf8(Offset)}
  },
  send_update(BotId, Update, UserId).
