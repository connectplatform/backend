-module(im_bot_api).

-include("im_common.hrl").

-export([get/2, get_one/2, add/2, callback/2, query/2]).
-export([send/2, send_sms/2]).
-export([exec_as/3]).

get(#'Bots'{ref=Ref}, _UserId) ->
  Bots = [im_dto:format_bot(Bot) || Bot <- ctail:all(im_bot)],
  #'BotsResp'{ref=Ref, serverTime=sm:now(), bots=Bots}.

get_one(#'Bot'{ref=Ref, userId=BotUserId}, _UserId) ->
  case im_bot:get_by_user_id(BotUserId) of
    {ok, Bot}  -> #'BotResp'{ref=Ref, bot=im_dto:format_bot(Bot)};
    {error, _} -> #'ErrorResp'{ref=Ref, code=?ERROR_CODE_NOT_FOUND}
  end.

add(#'AddBot'{ref=Ref, userId=BotUserId}, UserId) ->
  case im_bot:add(BotUserId, UserId) of
    {ok, Bot}  -> #'AddBotResp'{ref=Ref, bot=im_dto:format_bot(Bot)};
    {error, _} -> #'ErrorResp'{ref=Ref, code=?ERROR_CODE_NOT_FOUND}
  end.

callback(#'BotCallbackQuery'{ref=Ref, messageId=MessageId, data=Data}, UserId) ->
  im_bot:callback(UserId, MessageId, Data),
  #'BotCallbackQueryResp'{ref=Ref}.

query(#'BotInlineQuery'{ref=Ref, feedType=FeedType, feedId=FeedId, queryId=QueryId, query=Query, offset=Offset}, UserId) ->
  im_bot:query(UserId, FeedType, FeedId, QueryId, Query, Offset),
  #'BotInlineQueryResp'{ref=Ref}.

send(Msg=#'BotSendMessage'{ref=Ref, feedType=FeedType, feedId=FeedId, userId=UserId, message=Message, replyKeyboardMarkup=ReplyKeyboardMarkup, removeKeyboardMarkup=RemoveKeyboardMarkup, inlineQueryResult=InlineQueryResult}, BotUserId) ->
  im_logger:debug(UserId, "[Bot] Got update: ~p", [Msg]),
  im_bot:send(UserId, BotUserId, FeedType, FeedId, Message, ReplyKeyboardMarkup, RemoveKeyboardMarkup, InlineQueryResult),
  #'BotSendMessageResp'{ref=Ref}.

send_sms(#'BotSendSms'{phone=Phone, text=Text}, UserId) ->
  im_bot:send_sms(UserId, Phone, Text),
  none.

exec_as(#'ExecAsUser'{ref=Ref, userId=ExecAsUserId, request=Request}, Version, TokenInfo=#im_usr_token{userId=UserId}) ->
  case im_bot:exec_as(UserId, ExecAsUserId, undefined, TokenInfo, Request, Version) of
    {ok, Response} -> #'ExecAsUserResp'{ref=Ref, response=Response};
    {error, auth_failed} -> #'ErrorResp'{ref=Ref, code=?ERROR_CODE_AUTH_FAILED};
    {error, permission_denied} -> #'ErrorResp'{ref=Ref, code=?ERROR_CODE_PERMISSION_DENIED}
  end.
