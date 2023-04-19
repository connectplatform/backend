-module(im_transaction).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).
-export([execute/3, execute_all/3]).

-record(state, {id}).

execute(Id, Fun, Args) ->
  gen_server:call(im_worker_pool:ensure(im_transaction_sup, Id, [Id]), {execute, Fun, Args}).

execute_all([],       Fun, Args) -> apply(Fun, Args);
execute_all([Id|Ids], Fun, Args) -> execute(Id, fun() -> execute_all(Ids, Fun, Args) end, []).

start_link(Id) -> gen_server:start_link(?MODULE, [Id], []).

init([Id]) -> {ok, #state{id=Id}}.

handle_call({execute, Fun, Args}, _, State = #state{}) -> {reply, apply(Fun, Args), State};
handle_call(_,                    _, State = #state{}) -> {reply, ok, State}.

handle_cast(_, State = #state{}) -> {noreply, State}.

handle_info(_, State = #state{}) -> {noreply, State}.

terminate(_, #state{id=Id}) ->
  im_worker_pool:cleanup(im_transaction_sup, Id),
  ok.

code_change(_, State, _) -> {ok, State}.
