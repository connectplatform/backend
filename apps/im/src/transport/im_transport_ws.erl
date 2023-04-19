-module(im_transport_ws).
-behaviour(sm_websocket).

-include("im_common.hrl").

-export([handle/2]).

handle(handshake, {_, Req}) ->
  {Token, _} = cowboy_req:qs_val(<<"token">>, Req),
  {Locale, _} = cowboy_req:qs_val(<<"locale">>, Req),
  Version = im_transport_router:parse_version_number(Req),

  im_logger:catch_errors(fun() ->
    case im_init:authenticate(binary_to_list(Token), Locale) of
      {ok, User, TokenRec} ->
        {ok, {User, TokenRec, Version}};
      {error, token_not_found} ->
        im_logger:error(undefined, "[Transport] Token not found: ~p", [Token]),
        {shutdown, 401};
      {error, _} ->
        shutdown
    end
  end, undefined);

handle(init, {ErrorResp=#'ErrorResp'{}, _}) ->
  ErrorResp;

handle(init, {{User, TokenRec, Version}, _}) ->
  UserId1 = im_common:parse_id(User#im_usr.id),
  im_logger:catch_errors(fun() ->
    im_init:initialize(User, TokenRec, Version)
  end, UserId1);

handle({stream, {binary, Data}}, {State=#im_state{user=User, userId=UserId, token=TokenRec, version=Version}, _}) ->
  % im_logger:debug(User#im_usr.id, "Got WS message: ~p", [Data]),
  case im_transport_router:process(Data, User, TokenRec, Version) of
    none         -> {ok, State};
    {ok, Result} -> {reply, im_transport_router:format_record(Result), State};
    Result       -> {reply, im_transport_router:format_record(Result), State}
  end;

handle({stream, Data}, {State, _}) ->
  im_logger:error(State#im_state.userId, "Got unknown data from WebSocket: ~p", [Data]),
  {reply, Data, State};

handle({info, {user_changed, User}}, {State, _}) ->
  {ok, State#im_state{user=User}};

handle({info, authenticated}, {State, _}) ->
  {reply, {binary, #'AuthResp'{}}, State};

handle({info, logout}, {#im_state{userId=UserId}, _}) ->
  DeviceId = im_user_state:attr(UserId, deviceId),
  im_user_state:checkout(UserId, DeviceId),
  ok;

handle({info, Data}, {State=#im_state{}, _}) ->
  {reply, {binary, Data}, State};

handle({logout}, {#im_state{userId=UserId}, _}) ->
  DeviceId = im_user_state:attr(UserId, deviceId),
  im_user_state:checkout(UserId, DeviceId);

handle({terminate, _Reason}, {State, _})  ->
  im_init:terminate(State#im_state.userId),
  ok;

handle(_, {State, _}) ->
  {ok, State}.
