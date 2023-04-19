-module(im_std_worker_server).
-behaviour(gen_server).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {options, handler, userState}).

start_link(Name, Options) ->
  % im_logger:debug(undefined, "[ImStdWorkerServer] NAME: ~p, OPTIONS: ~p", [Name, Options]),
  gen_server:start_link({local, im_std_worker:sys_name(wrk, Name)}, ?MODULE, [Options], []).

init([Args]) ->
  process_flag(trap_exit, true),

  Handler = proplists:get_value(handler, Args),
  Options = proplists:get_value(options, Args),

  % im_logger:debug(undefined, "[ImStdWorkerServer][~p] INIT. ARGS: ~p, OPTIONS: ~p", [Handler, Args, Options]),

  UserState = case Handler(init, Options, undefined) of
    {ok, UserState1} -> UserState1;
    ok               -> undefined
  end,

  % im_logger:debug(undefined, "[ImStdWorkerServer][~p] USER_STATE: ~p", [Handler, UserState]),

  {ok, #state{options=Options, handler=Handler, userState=UserState}}.

handle_call(Request, _, State=#state{handler=Handler, userState=UserState}) ->
  % im_logger:debug(undefined, "[ImStdWorkerServer][~p] CALL: ~p", [Handler, Request]),

  case Handler(call, Request, UserState) of
    ok                           -> {reply, ok, State};
    {reply, Reply, NewUserState} -> {reply, Reply, State#state{userState=NewUserState}};
    {noreply, NewUserState}      -> {noreply, State#state{userState=NewUserState}};
    {stop, Reason, NewUserState} -> {stop, Reason, State#state{userState=NewUserState}};
    Result                       -> im_logger:error(undefined, "[ImStdWorkerServer][~p] Handler returned invalid result: ~p, Action: ~p", [Handler, Result, call])
  end.

handle_cast(Request, State=#state{handler=Handler, userState=UserState}) ->
  % im_logger:debug(undefined, "[ImStdWorkerServer] CAST: ~p", [Request]),

  case Handler(cast, Request, UserState) of
    ok                           -> {noreply, State};
    {noreply, NewUserState}      -> {noreply, State#state{userState=NewUserState}};
    {stop, Reason, NewUserState} -> {stop, Reason, State#state{userState=NewUserState}};
    Result                       -> im_logger:error(undefined, "[ImStdWorkerServer][~p] Handler returned invalid result: ~p, Action: ~p", [Handler, Result, cast])
  end.

handle_info(Info, State=#state{handler=Handler, userState=UserState}) ->
  case Handler(info, Info, UserState) of
    ok                           -> {noreply, State};
    {noreply, NewUserState}      -> {noreply, State#state{userState=NewUserState}};
    {stop, Reason, NewUserState} -> {stop, Reason, State#state{userState=NewUserState}};
    Result                       -> im_logger:error(undefined, "[ImStdWorkerServer] Handler returned invalid result: ~p, Action: ~p", [Result, info])
  end.

terminate(Reason, #state{handler=Handler, userState=UserState}) ->
  Handler(terminate, Reason, UserState),
  % im_logger:debug(undefined, "[ImStdWorkerServer][~p] Terminating call. Reason: ~p", [Handler, Reason]),
  % im_worker_pool:cleanup(im_std_worker:sys_name(sup, Name), WorkerId),
  ok.

code_change(_OldVsn, State=#state{}, _Extra) -> {ok, State}.
