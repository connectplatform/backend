-module(im_worker_pool).

-export([spec/0, handle/3]).
-export([get/2, ensure/3, exists/2, cleanup/2]).

spec() -> im_std_worker:spec(worker_pool, [], fun ?MODULE:handle/3).

get(Module, Id)                    -> im_std_worker:call(worker_pool, {get, Module, Id}).
ensure(Module, Id, StartChildArgs) -> im_std_worker:call(worker_pool, {ensure, Module, Id, StartChildArgs}).
exists(Module, Id)                 -> im_std_worker:call(worker_pool, {exists, Module, Id}).
cleanup(Module, Id)                -> im_std_worker:call(worker_pool, {cleanup, Module, Id}).

handle(call, {get, Module, Id}, State) ->
  Result = case im_ets:get_one(?MODULE, {Module, Id}) of
    {{Module, Id}, WorkerPid} -> WorkerPid;
    _               -> undefined
  end,
  {reply, Result, State};

handle(call, {ensure, Module, Id, ChildArgs}, State) ->
  Result = case im_ets:get_one(?MODULE, {Module, Id}) of
    {{Module, Id}, WorkerPid} ->
      WorkerPid;
    _ ->
      {ok, WorkerPid} = supervisor:start_child(Module, ChildArgs),
      ok = im_ets:put(?MODULE, {{Module, Id}, WorkerPid}),
      WorkerPid
  end,
  {reply, Result, State};

handle(call, {exists, Module, Id}, State) ->
  {reply, im_ets:exists(?MODULE, {Module, Id}), State};

handle(call, {cleanup, Module, Id}, State) ->
  im_ets:delete(?MODULE, {Module, Id}),
  {reply, ok, State};

handle(_, _, _) -> ok.
