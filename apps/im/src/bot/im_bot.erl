-module(im_bot).

-include("im_common.hrl").

-export([init/0, add/2, callback/3, query/6, send/8, send_sms/3, exec_as/6]).
-export([ensure_sys_bots/1, get_sys_bots/0, lookup_by_user_id/1, get_by_user_id/1, lookup_by_username/1, is_system/1]).

init() ->
  lists:foreach(fun(#im_bot{userId=BotUserId}) -> im_roster_chat:worker(BotUserId) end, ctail:all(im_bot)).

callback(UserId, MessageId, Data) ->
  {ok, Message} = ctail:get(im_msg, im_common:parse_id(MessageId)),
  BotUserId = Message#im_msg.origin,
  {ok, BotId} = lookup_by_user_id(BotUserId),
  case Message#im_msg.feed_id of
    {_, RoomId} -> im_bot_update:send_callback_query_update(BotId, UserId, ?MESSAGE_FEED_TYPE_ROOM, RoomId, Message, Data);
    _           -> im_bot_update:send_callback_query_update(BotId, UserId, ?MESSAGE_FEED_TYPE_CHAT, UserId, Message, Data)
  end.

query(UserId, FeedType, FeedId, QueryId, Query, Offset) ->
  Query1 = im_common:trim(Query),
  BotUsername = string:substr(hd(string:tokens(Query1, " ")), 2),
  {ok, BotId} = lookup_by_username(BotUsername),
  FeedId1 = case FeedType of
    ?MESSAGE_FEED_TYPE_CHAT -> im_common:parse_id(UserId);
    ?MESSAGE_FEED_TYPE_ROOM -> im_common:parse_id(FeedId)
  end,
  im_bot_update:send_inline_query_update(BotId, UserId, FeedType, FeedId1, QueryId, Query, Offset).

send(UserId, BotUserId, FeedType, FeedId, Message, ReplyKeyboardMarkup, RemoveKeyboardMarkup, InlineQueryResult) ->
  ReplyKeyboardChanged = ReplyKeyboardMarkup =/= undefined orelse RemoveKeyboardMarkup =/= undefined,

  case ReplyKeyboardChanged of
    true ->
      {ReplyKeyboardMarkup1, RemoveKeyboardMarkup1} = case RemoveKeyboardMarkup =:= undefined of
        true -> {ReplyKeyboardMarkup, undefined};
        false -> {undefined, RemoveKeyboardMarkup}
      end,
      RealFeedId = case FeedType of
        ?MESSAGE_FEED_TYPE_CHAT -> BotUserId;
        ?MESSAGE_FEED_TYPE_ROOM -> im_common:parse_id(FeedId)
      end,
      {User, Update} = im_chatupdate:quick_update(UserId, FeedType, RealFeedId, fun(Update, User) ->
        Update1 = Update#im_chat_update{
          replyKeyboardMarkup=im_dto:parse_reply_keyboard_markup(ReplyKeyboardMarkup1),
          removeKeyboardMarkup=im_dto:parse_remove_keyboard_markup(RemoveKeyboardMarkup1)
        },
        {{User, Update1}, Update1}
      end),
      im_chatupdate:send_update(User, Update);
    false -> skip
  end,

  case Message =/= undefined of
    true -> im_message:send(BotUserId, FeedType, im_common:parse_id(FeedId), im_dto:parse_message(Message), false);
    false -> skip
  end,

  case InlineQueryResult =:= undefined of
    true -> skip;
    false ->
      MsgReply = #'BotInlineQueryAnswer'{
        feedType=FeedType,
        feedId=FeedId,
        userId=UserId,
        queryId=InlineQueryResult#'InlineQueryResultEntity'.queryId,
        results=InlineQueryResult#'InlineQueryResultEntity'.results,
        nextOffset=im_common:format_utf8(InlineQueryResult#'InlineQueryResultEntity'.nextOffset)
      },

      MsgReply1 = case FeedType =:= ?MESSAGE_FEED_TYPE_CHAT of
        true -> MsgReply#'BotInlineQueryAnswer'{feedId=im_common:format_id(BotUserId)};
        false -> MsgReply
      end,
      im_user_state:broadcast(BotUserId, [im_common:parse_id(UserId)], MsgReply1, false)
  end.

send_sms(UserId, Phone, Text) ->
  User = im_roster_chat:get(UserId),
  case User#im_usr.isBot of
    true ->
      case im_common:get_county_code(Phone) of
        {ok, CountryCode} ->
          Phone1 = list_to_binary("+" ++ integer_to_list(CountryCode) ++ binary_to_list(im_common:format_phone(Phone))),
          im_sms:send(CountryCode, binary_to_list(Phone1), Text),
          {ok, send};
        {error, invalid_phone} ->
          {error, invalid_phone}
      end;
    _ -> {error, permission_denied}
  end.

exec_as(UserId, ExecAsUserId, Token, TokenInfo, Request, Version) ->
  case im_acl:has_super_perms(UserId) of
    true ->
      BecomeUser = case ExecAsUserId =/= undefined of
        true -> im_roster_chat:get(im_common:parse_id(ExecAsUserId));
        false ->
          case im_auth:find_token_by_id(Token) of
            undefined -> {error, not_found};
            TokenRec -> im_roster_chat:get(TokenRec#im_usr_token.userId)
          end
      end,
      case BecomeUser of
        {error, _} ->
          {error, auth_failed};
        _ ->
          {ok, Response} = im_transport_router:handle(binary_to_term(Request), Version, BecomeUser, TokenInfo),
          im_logger:debug(undefined, "[Bot] ExecAs. Request: ~p, Response: ~p", [binary_to_term(Request), Response]),
          Response1 = term_to_binary(im_transport_router:format_record(Response)),
          {ok, Response1}
      end;
    false ->
      {error, permission_denied}
  end.

add(BotUserId, UserId) ->
  case get_by_user_id(BotUserId) of
    {ok, Bot} ->
      im_contact_api:add(#'AddContact'{contact=#'ContactEntity'{userId=BotUserId}}, im_roster_chat:get(UserId)),
      Message = #im_msg{
        type=?MESSAGE_TYPE_USER_MESSAGE,
        kind=?MESSAGE_KIND_TEXT,
        created=sm:now(),
        origin=UserId,
        recipient=BotUserId,
        payload= <<"/start">>
      },
      im_bot_update:send_message_update(Bot#im_bot.id, Bot#im_bot.userId, UserId, ?MESSAGE_FEED_TYPE_CHAT, UserId, Message),
      {ok, Bot};
    {error, Reason} -> {error, Reason}
  end.

update(Username, Name, Descr, WebhookUrl, Photo, Thumb, AccessToken, IsInline, Type, Roles) ->
  UpdateHandler = fun() ->
    {ok, BotId} = lookup_by_username(Username),

    {ok, Bot} = ctail:get(im_bot, BotId),
    Bot1 = Bot#im_bot{
      username=im_common:format_utf8(Username),
      descr=im_common:format_utf8(Descr),
      accessToken=im_common:format_utf8(AccessToken),
      webhookUrl=im_common:format_utf8(WebhookUrl),
      isInline=im_common:parse_boolean(IsInline),
      updated=sm:now(),
      type=Type
    },
    ok = ctail:put(Bot1),

    BotUser = im_roster_chat:get(Bot#im_bot.userId),
    BotUser1 = BotUser#im_usr{
      name=im_common:format_utf8(Name),
      username=im_common:format_utf8(Username),
      photo=im_common:format_utf8(Photo),
      thumbnail=im_common:format_utf8(Thumb),
      roles=Roles,
      active=true,
      isNew=false
    },
    UpdatedBotUser = im_roster_chat:execute(BotUser1#im_usr.id, fun(#im_usr{}) ->
      ctail:put(im_user_transform:serialize(BotUser1)),
      {BotUser1, BotUser1}
    end),

    im_event:fire(?USER_UPDATED_EVENT, UpdatedBotUser),
    ctail:delete(im_usr_token, Bot#im_bot.accessToken),
    im_auth:create_token(0, <<"botApiClient">>, <<"Bot API">>, UpdatedBotUser, im_common:ensure_binary(AccessToken)),
    im_user_state:broadcast(undefined, [BotUser1#im_usr.id], {user_changed, UpdatedBotUser}),

    {ok, Bot1}
  end,
  try UpdateHandler() catch
    Error:Reason ->
      im_logger:error(undefined, "[Bot] update terminated: ~p. Reason: ~p", [Error, Reason]),
      {error, Reason}
  end.

create(Username, Name, Descr, WebhookUrl, Photo, Thumb, AccessToken, IsInline, Type, Roles) ->
  BotUserId = ctail:next_id(),
  NowTime = sm:now(),
  BotUser = #im_usr{
    id=BotUserId,
    name=im_common:format_utf8(Name),
    username=im_common:format_utf8(Username),
    photo=im_common:format_utf8(Photo),
    thumbnail=im_common:format_utf8(Thumb),
    roles=Roles,
    active=true,
    isBot=true,
    isNew=false,
    createdAt=NowTime,
    updatedAt=NowTime
  },
  ok = ctail:put(BotUser),
  im_event:fire(?USER_UPDATED_EVENT, BotUser),
  BotId = ctail:next_id(),
  Bot = #im_bot{
    id=BotId,
    userId=im_common:parse_id(BotUserId),
    username=im_common:format_utf8(Username),
    descr=im_common:format_utf8(Descr),
    accessToken=im_common:format_utf8(AccessToken),
    webhookUrl=im_common:format_utf8(WebhookUrl),
    isSystem=true,
    isInline=im_common:parse_boolean(IsInline),
    updated=sm:now(),
    type=Type
  },
  ok = ctail:put(Bot),

  im_auth:create_token(0, <<"botApiClient">>, <<"Bot API">>, BotUser, Bot#im_bot.accessToken),

  ok = ctail:put(#im_bot_user_lookup{id=BotUserId, botId=BotId}),
  ok = ctail:put(#im_bot_username_lookup{id=im_common:ensure_binary(Username), botId=BotId}),

  im_roster_chat:worker(BotUserId),

  {ok, Bot}.

is_system(UserId) ->
  case im_bot:lookup_by_user_id(UserId) of
    {ok, BotId} ->
      Bot = im_bot_worker:get(BotId),
      Bot#im_bot.isSystem =:= true;
    {error, _} ->
      false
  end.

ensure_sys_bots(_Json={array, BotConfigs}) ->
  im_logger:debug(undefined, "[Bot] Loading sys bot config: ~p", [BotConfigs]),

  lists:foreach(fun({struct, ConfigProps}) ->
    Name = proplists:get_value("name", ConfigProps),
    Username = proplists:get_value("username", ConfigProps),
    Descr = proplists:get_value("descr", ConfigProps),
    WebhookUrl = proplists:get_value("webhookUrl", ConfigProps),
    Photo = proplists:get_value("photo", ConfigProps),
    Thumb = proplists:get_value("thumb", ConfigProps),
    IsInline = proplists:get_value("isInline", ConfigProps),
    AccessToken = proplists:get_value("accessToken", ConfigProps),
    Type = proplists:get_value("type", ConfigProps),
    Roles = case proplists:get_value("roles", ConfigProps, []) of
      {array, Roles1} ->
        lists:foldl(fun(Role, Acc) ->
          case is_list(Role) of
            true -> Acc ++ [im_common:ensure_binary(Role)];
            false -> Acc
          end
        end, [], Roles1);
      _ ->
        []
    end,
    UpsertResult = case lookup_by_username(Username) of
      {ok, _} -> update(Username, Name, Descr, WebhookUrl, Photo, Thumb, AccessToken, IsInline, Type, Roles);
      _       -> create(Username, Name, Descr, WebhookUrl, Photo, Thumb, AccessToken, IsInline, Type, Roles)
    end,

    case UpsertResult of
      {ok, Bot} -> im_bot_worker:reconfigure(Bot#im_bot.id);
      _         -> skip
    end
  end, BotConfigs).

get_sys_bots() ->
  ctail:index(im_bot, <<"isSystem">>, <<"true">>).

get_by_user_id(UserId) ->
  case ctail:get(im_bot_user_lookup, im_common:parse_id(UserId)) of
    {ok, Lookup}    -> ctail:get(im_bot, Lookup#im_bot_user_lookup.botId);
    {error, Reason} -> {error, Reason}
  end.

lookup_by_user_id(UserId) ->
  case ctail:get(im_bot_user_lookup, im_common:parse_id(UserId)) of
    {ok, Lookup} -> {ok, Lookup#im_bot_user_lookup.botId};
    _            -> {error, not_found}
  end.

lookup_by_username(Username) ->
  case ctail:get(im_bot_username_lookup, im_common:ensure_binary(Username)) of
    {ok, Lookup} -> {ok, Lookup#im_bot_username_lookup.botId};
    _            -> {error, not_found}
  end.
