-module(im_bot_worker).
-behaviour(gen_server).
-include("im_common.hrl").

-export([get/1, send/3, reconfigure/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

-record(state, {id, accessToken, webhookUrl, username, userId, bot}).

worker(BotId) ->
  im_worker_pool:ensure(im_bot_sup, im_common:parse_id(BotId), [im_common:parse_id(BotId)]).

get(BotId) ->
  case worker(BotId) of
    undefined -> undefined;
    Pid -> gen_server:call(Pid, {get})
  end.
send(BotId, Msg, UserId) -> gen_server:cast(worker(BotId), {send, Msg, UserId}).
reconfigure(BotId)       -> gen_server:cast(worker(BotId), {reconfigure}).

start_link(BotId) ->
  gen_server:start_link(?MODULE, [BotId], []).

init([BotId]) ->
  get_state(BotId).

get_state(BotId) ->
  {ok, Bot} = ctail:get(im_bot, BotId),
  {ok, #state{
    id=BotId,
    accessToken=Bot#im_bot.accessToken,
    webhookUrl=Bot#im_bot.webhookUrl,
    username=Bot#im_bot.username,
    userId=Bot#im_bot.userId,
    bot=Bot
  }}.

handle_cast({reconfigure}, #state{id=BotId, userId=UserId, username=Username}) ->
  im_logger:debug(UserId, "[Bot] ~s> Reconfiguring", [Username]),
  {ok, State1} = get_state(BotId),
  {noreply, State1};
handle_cast({send, Msg, UserId}, State=#state{webhookUrl=WebhookUrl, accessToken=AccessToken, username=Username}) ->
  case im:is_test() of
    true ->
      {noreply, State};
    false ->
      Url = im_common:ensure_list(WebhookUrl) ++ "?token=" ++ im_common:ensure_list(AccessToken),
      im_logger:debug(UserId, "[Bot] ~s> Send to bot. Url: ~p, message: ~p", [Username, Url, Msg]),
      Response = httpc:request(post, {Url, [], "application/binary", term_to_binary(Msg)}, [{timeout, timer:seconds(90)}], []),
      case Response of
        {ok, {{_, StatusCode, _}, _, Data}} -> im_logger:debug(UserId, "[Bot] Bot responded with status ~p and body: ~p", [StatusCode, Data]);
        {error, Reason}                      -> im_logger:debug(UserId, "[Bot] Bot responded with error: ~p", [Reason])
      end,
      {noreply, State}
  end;
handle_cast(_Request={}, State) -> {noreply, State}.

handle_call({get}, _, State=#state{bot=Bot}) ->
  {reply, Bot, State};
handle_call(_Request={}, _, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(Reason, State=#state{username=Username, userId=UserId}) ->
  BotId = State#state.id,
  case im_ets:get_one(im_bot_sup, BotId) of
    {BotId, _} -> im_ets:delete(im_bot_sup, BotId);
    _          -> skip
  end,
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
