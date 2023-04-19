-module(im_apn_pool).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_pool/0, send/3]).

-record(state, {appId}).

start_pool() ->
  wpool:start_pool(?MODULE, [
    {workers, 10},
    {worker, {?MODULE, []}}
  ]).

send(UserId, Token, Message) ->
  wpool:cast(?MODULE, {send, UserId, Token, Message}).

%% gen_server

init(_) ->
  State = #state{appId=sm:env(im, app_id)},
  {ok, State}.

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast({send, UserId, Token, Data}, State=#state{appId=AppId}) ->
  TokenEncoded = im_encode_uri:encode(im_common:ensure_list(Token)),
  Url = "http://" ++ sm:env(im, apn_url) ++ "/push/" ++ AppId ++ "/" ++ TokenEncoded,
  Json = jsx:encode(Data),

  im_logger:info(UserId, "[PushIOS] Target APN url: ~p, body: ~p", [Url, Json]),

  case im_http:post(Url, Json, [{<<"Content-Type">>, <<"application/json">>}]) of
    {ok, 200, Body, _} ->
      Response = jsx:decode(Body),
      SuccessDevices = proplists:get_value(<<"sent">>, Response),
      FailedDevices = proplists:get_value(<<"failed">>, Response),

      case SuccessDevices =:= [] orelse SuccessDevices =:= undefined of
        true -> skip;
        false ->
          Ids = [proplists:get_value(<<"device">>, Device) || Device <- SuccessDevices],
          im_logger:info(UserId, "[PushIOS] Successfully sent VoIP notification to: ~p", [Ids])
      end,

      case FailedDevices =:= [] orelse FailedDevices =:= undefined of
        true -> skip;
        false ->
          im_logger:error(UserId, "[PushIOS] Failed to deliver VoIP notification: ~p", [FailedDevices]),
          lists:foreach(fun(Device) ->
            FailedToken = proplists:get_value(<<"device">>, Device),
            im_device:remove_push_token(FailedToken)
          end, FailedDevices)
      end;
    {ok, _, Body, _} ->
      im_logger:error(UserId, "[PushIOS] Failed to deliver VoIP notification. Response: ~p", [Body]);
    {error, _, Reason, _} ->
      im_logger:error(UserId, "[PushIOS] Failed to deliver VoIP notification: request to APN server failed: ~p", [Reason])
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
