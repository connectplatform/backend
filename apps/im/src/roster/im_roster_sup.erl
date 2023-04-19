-module(im_roster_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([worker/2, worker_cleanup/2, terminate_worker/2, restart_all_workers/0]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
  {ok, {{one_for_one, 5, 10}, [pool(im_roster_chat_sup), pool(im_roster_muc_sup)]}}.

worker_spec(im_roster_chat_sup) -> {none, {im_roster_chat,  start_link, []}, transient, 2000, worker, [im_roster_chat]};
worker_spec(im_roster_muc_sup) -> {none, {im_roster_muc, start_link, []}, transient, 2000, worker, [im_roster_muc]}.

pool(SupName) ->
  MFA = {supervisor, start_link, [{local, SupName}, im_roster, worker_spec(SupName)]},
  {SupName, MFA, permanent, infinity, supervisor, []}.

worker(_SupName, undefined) -> undefined;
worker(SupName, EntityId) ->
  EntityId1 = im_common:parse_id(EntityId),
  case im_ets:get_one(im_roster_worker, {SupName, EntityId1}) of
    {{SupName, EntityId1}, WorkerPid} ->
      WorkerPid;
    _ ->
      {ok, WorkerPid} = supervisor:start_child(SupName, [EntityId1]),
      im_ets:put(im_roster_worker, {{SupName, EntityId1}, WorkerPid}),
      WorkerPid
  end.

worker_cleanup(SupName, Id) ->
  im_ets:delete(im_roster_worker, {SupName, Id}).

terminate_worker(SupName, Pid) ->
  supervisor:terminate_child(SupName, Pid).

restart_all_workers() ->
  lists:foreach(fun({{SupName, EntityId}, WorkerPid}) ->
    case im_ets:get_one(im_roster_worker, {SupName, EntityId}) of
      {_, WorkerPid} ->
        terminate_worker(SupName, WorkerPid),
        worker_cleanup(SupName, EntityId);
      _ ->
        skip
    end
  end, im_ets:all(im_roster_worker)).
