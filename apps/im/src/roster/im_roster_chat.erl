-module(im_roster_chat).
-behaviour(gen_server).

-include("im_common.hrl").

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([worker/1, kill/1, get/1, refresh/1, add/2, remove/2, list/1, join/2, leave/2, rooms/1, message/3, public_message/3, retrieve/7, public_retrieve/6, execute/2, execute/3]).

worker(Id)                                          -> im_roster_sup:worker(im_roster_chat_sup, Id).
kill(Id)                                            -> im_roster_sup:terminate_worker(im_roster_chat_sup, worker(Id)).

call(Id, Msg) ->
  case worker(Id) of
    undfined -> undefined;
    Pid -> gen_server:call(Pid, Msg)
  end.

get(Id)                                                        -> call(Id, {get}).
refresh(Id)                                                    -> call(Id, {refresh}).
add(Id, RosterId)                                              -> call(Id, {add, RosterId}).
remove(Id, RosterId)                                           -> call(Id, {remove, RosterId}).
list(Id)                                                       -> call(Id, {list}).
join(Id, GroupId)                                              -> call(Id, {join, GroupId}).
leave(Id, GroupId)                                             -> call(Id, {leave, GroupId}).
rooms(Id)                                                      -> call(Id, {rooms}).
message(Id, ToId, Message)                                     -> call(Id, {message, ToId, Message}).
public_message(Id, ToId, Message)                              -> call(Id, {public_message, ToId, Message}).
retrieve(Id, ToId, TopId, StopId, Direction, Count, FilterFun) -> call(Id, {retrieve, ToId, TopId, StopId, Direction, Count, FilterFun}).
public_retrieve(Id, ToId, TopId, StopId, Count, FilterFun)     -> call(Id, {public_retrieve, ToId, TopId, StopId, Count, FilterFun}).
execute(Id, Fun)                                               -> call(Id, {execute, Fun, []}).
execute(Id, Fun, Args)                                         -> call(Id, {execute, Fun, Args}).

start_link(UserId) ->
  gen_server:start_link(?MODULE, UserId, []).

init(UserId) ->
  process_flag(trap_exit, true),

  case ctail:get(im_usr, UserId) of
    {ok, User} ->
      {ok, im_user_transform:deserialize(User)};
    {error, _} ->
      ErrorMsg = unicode:characters_to_binary(io_lib:format("Unexisting user: ~p", [UserId])),
      {stop, ErrorMsg}
  end.

handle_call({get}, _, User) ->
  {reply, User, User};
handle_call({refresh}, _, User) ->
  case ctail:get(im_usr, User#im_usr.id) of
    {ok, RefreshedUser} ->
      NewUser = im_user_transform:deserialize(RefreshedUser),
      {reply, NewUser, NewUser};
    {error, _} ->
      {reply, User, User}
  end;

handle_call({add, RosterId}, _, User) ->
  Roster = User#im_usr.roster,
  User1 = case lists:member(RosterId, Roster) of
    false ->
      User2 = User#im_usr{roster=[RosterId|Roster]},
      ok = ctail:put(im_user_transform:serialize(User2)),
      User2;
    true  ->
      User
  end,
  {reply, ok, User1};

handle_call({remove, RosterId}, _, User) ->
  Roster = User#im_usr.roster,
  User1 = case lists:member(RosterId, Roster) of
    true ->
      User2 = User#im_usr{roster=lists:delete(RosterId, Roster)},
      ok = ctail:put(im_user_transform:serialize(User2)),
      User2;
    false ->
      User
  end,
  {reply, ok, User1};

handle_call({list}, _, User) ->
  {reply, User#im_usr.roster, User};

handle_call({join, GroupId}, _, User) ->
  Rooms = User#im_usr.rooms,
  User1 = case lists:member(GroupId, Rooms) of
    false ->
      User2 = User#im_usr{rooms=[GroupId|Rooms]},
      ok = ctail:put(im_user_transform:serialize(User2)),
      User2;
    true  ->
      User
  end,
  {reply, ok, User1};

handle_call({leave, GroupId}, _, User) ->
  Rooms = User#im_usr.rooms,
  User1 = case lists:member(GroupId, Rooms) of
    true ->
      User2 = User#im_usr{rooms=lists:delete(GroupId, Rooms)},
      ok = ctail:put(im_user_transform:serialize(User2)),
      User2;
    false ->
      User
  end,
  {reply, ok, User1};

handle_call({rooms}, _, User) ->
  {reply, User#im_usr.rooms, User};

handle_call({message, ToId, Message}, _, User=#im_usr{id=Id}) ->
  {reply, im_roster:private(chat, ToId, Id, Message), User};

handle_call({retrieve, FromId, TopId, StopId, Direction, Count, FilterFun}, _, User=#im_usr{id=Id}) ->
  {reply, im_roster:retrieve(im_roster:feed_id(chat, Id, FromId), TopId, StopId, Direction, Count, FilterFun), User};

handle_call({execute, Fun, Args}, _, User) ->
  {Result, UpdatedUser} = apply(Fun, Args ++ [User]),
  case User =/= UpdatedUser of
    true ->
      % im_logger:debug(User#im_usr.id, "[RosterChat] USER CHANGED: ~p", [UpdatedUser#im_usr{chatUpdates=[], contactUserIds=[], devices=[]}]),
      ctail:put(im_user_transform:serialize(UpdatedUser));
    false -> skip
  end,
  {reply, Result, UpdatedUser};

handle_call(Command, _, User) ->
  {reply, {unknown, Command}, User}.

handle_cast(Msg, User=#im_usr{id=Id}) ->
  im_logger:error(Id, "ROSTER. Unknown API async: ~p", [Msg]),
  {stop, {error, {unknown_cast, Msg}}, User}.

handle_info({'DOWN', _MonitorRef, _Type, _Object, _Info}=_Msg, User=#im_usr{id=Id}) ->
  im_roster_sup:worker_cleanup(user, Id),
  {stop, normal, User};

handle_info(Info, User=#im_usr{id=Id}) ->
  im_logger:error(Id, "ROSTER. Unrecognized info: ~p", [Info]),
  {noreply, User}.

terminate(_Reason, #im_usr{id=Id}) ->
  im_roster_sup:worker_cleanup(im_roster_chat_sup, Id),
  ok.

code_change(_OldVsn, User, _Extra) ->
  {ok, User}.
