-module(im_fcm_pool).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_pool/0, send/3]).

start_pool() ->
  wpool:start_pool(?MODULE, [
    {workers, 10},
    {worker, {?MODULE, []}}
  ]).

send(UserId, Token, Message) ->
  wpool:cast(?MODULE, {send, UserId, Token, Message}).

%% gen_server

init(_) ->
  {ok, undefined}.

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast({send, UserId, Token, Message}, State) ->
  im_logger:debug(UserId, "[FCM] Send push. Token: ~p, Message: ~p", [Token, Message]),
  case fcm:sync_push(im_fcm_sender, Token, Message) of
    [{_Token, ok}] -> skip;
    [{_Token, Error}] ->
      im_logger:error(UserId, "[FCM] Send push for token ~p failed: ~p", [Token, Error]),
      im_device:remove_push_token(UserId, Token)
  end,
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
