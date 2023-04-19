-module(im_roster_muc).
-behaviour(gen_server).

-include("im_common.hrl").

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([worker/1, kill/1, get/1, refresh/1, topic/2, join/2, quit/2, list/1, message/3, retrieve/7, execute/2, execute/3]).

worker(Id) -> im_roster_sup:worker(im_roster_muc_sup, Id).
kill(Id)   -> im_roster_sup:terminate_worker(im_roster_muc_sup, worker(Id)).

call(Id, Msg) ->
  case worker(Id) of
    undfined -> undefined;
    Pid -> gen_server:call(Pid, Msg)
  end.

get(Id)                                                          -> call(Id, {get}).
refresh(Id)                                                      -> call(Id, {refresh}).
topic(Id, Topic)                                                 -> call(Id, {topic, Topic}).
join(Id, UserIds) when is_list(UserIds)                          -> call(Id, {join, UserIds});
join(Id, UserId)                                                 -> call(Id, {join, UserId}).
quit(Id, UserId)                                                 -> call(Id, {quit, UserId}).
list(Id)                                                         -> call(Id, {list}).
message(Id, FromId, Message)                                     -> call(Id, {message, FromId, Message}).
retrieve(Id, UserId, TopId, StopId, Direction, Count, FilterFun) -> call(Id, {retrieve, UserId, TopId, StopId, Direction, Count, FilterFun}).
execute(Id, Fun)                                                 -> call(Id, {execute, Fun, []}).
execute(Id, Fun, Args)                                           -> call(Id, {execute, Fun, Args}).

start_link(GroupId) ->
  gen_server:start_link(?MODULE, GroupId, []).

init(GroupId) ->
  process_flag(trap_exit, true),
  case ctail:get(im_grp, GroupId) of
    {ok, Group} -> {ok, Group};
    {error, _} -> {stop, io_lib:format("Unexisting group: ~p", [GroupId])}
  end.

handle_call({get}, _, Group) ->
  {reply, Group, Group};

handle_call({refresh}, _, #im_grp{id=GroupId}) ->
  case ctail:get(im_grp, GroupId) of
    {ok, Group} -> {reply, Group, Group};
    {error, _} -> {stop, io_lib:format("Unexisting group: ~p", [GroupId])}
  end;

handle_call({topic, Topic}, _, Group) ->
  Group1 = Group#im_grp{topic=Topic},
  ok = ctail:put(Group1),
  {reply, ok, Group1};

handle_call({join, RosterIds}, _, Group=#im_grp{id=Id}) when is_list(RosterIds) ->
  Members = Group#im_grp.members,
  ToAdd = lists:filter(fun(RosterId) -> not lists:member(RosterId, Members) end, RosterIds),

  lists:foreach(fun(RosterId) -> ok = im_roster_chat:join(RosterId, Id) end, ToAdd),
  Group1 = Group#im_grp{members=Members ++ ToAdd, updated=sm:now()},
  ok = ctail:put(Group1),

  {reply, ok, Group1};

handle_call({join, RosterId}, _, Group=#im_grp{id=Id}) ->
  Members = Group#im_grp.members,
  Group1 = case lists:member(RosterId, Members) of
    false ->
      Group2 = Group#im_grp{members=[RosterId|Members], updated=sm:now()},
      ok = ctail:put(Group2),
      Group2;
    true ->
      Group
  end,
  ok = im_roster_chat:join(RosterId, Id),
  {reply, ok, Group1};

handle_call({quit, RosterId}, _, Group=#im_grp{id=Id}) ->
  Members = Group#im_grp.members,
  Group1 = case lists:member(RosterId, Members) of
    true ->
      Group2 = Group#im_grp{members=lists:delete(RosterId, Members)},
      ctail:put(Group2),
      Group2;
    false ->
      Group
  end,
  ok = im_roster_chat:leave(RosterId, Id),
  {reply, ok, Group1};

handle_call({list}, _, Group=#im_grp{members=Members}) ->
  {reply, Members, Group};

handle_call({message, FromId, Message}, _, Group=#im_grp{id=Id}) ->
  {reply, im_roster:public(Id, FromId, Message), Group};

handle_call({retrieve, UserId, TopId, StopId, Direction, Count, FilterFun}, _, Group=#im_grp{id=Id}) ->
  {reply, im_roster:retrieve(im_roster:feed_id(muc, Id, UserId), TopId, StopId, Direction, Count, FilterFun), Group};

handle_call({execute, Fun, Args}, _, Group) ->
  {Result, Group1} = apply(Fun, Args ++ [Group]),
  {reply, Result, Group1};

handle_call(Command, _, Group)->
  {reply, {unknown, Command}, Group}.

handle_cast(stop, Group) ->
  {stop, normal, Group};

handle_cast(Msg, Group = #im_grp{id = Id}) ->
  im_logger:error(Id, "ROSTER MUC. Unknown API async: ~p", [Msg]),

  {stop, {error, {unknown_cast, Msg}}, Group}.

handle_info({'DOWN', _MonitorRef, _Type, _Object, _Info} = _Msg, Group = #im_grp{id=Id}) ->
  im_roster_sup:worker_cleanup(im_roster_muc_sup, Id),
  {stop, normal, Group};

handle_info(Info, Group = #im_grp{id = Id}) ->
  im_logger:error(Id, "ROSTER MUC. Unrecognized info: ~p", [Info]),
  {noreply, Group}.

terminate(_Reason, #im_grp{id=Id}) ->
  im_roster_sup:worker_cleanup(im_roster_muc_sup, Id),
  ok.

code_change(_OldVsn, Group, _Extra) ->
  {ok, Group}.
