-module(im_call_sup).
-include("im_common.hrl").
-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([add_participant/2, remove_participant/2, destroy/1]). %% called from im_rtc_communicator
-export([init_call/3, decline/2, cleanup_call/2, is_missed/2, find_user_call/2]). %% called from im_call_api

-define(CALL_INFO_STORAGE, call_info_storage).

-record(call_info, {
  rosterFeedId %% {chat, UId1, UId2} {muc, RoomId}
}).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, {
    #{strategy => simple_one_for_one,
      intensity => 6000,
      period => 60},
    [
      #{id => im_call_worker,
        start => {im_call_worker, start_link, []},
        restart => temporary,
        shutdown => infinity,
        type => worker}
    ]
  }}.

init_call(FeedType, FeedId, UserId) ->
  RosterFeedId = im_chatupdate:msg_feed_to_roster(FeedType, FeedId, UserId),
  Pid = im_worker_pool:get(?MODULE, RosterFeedId),
  case is_pid(Pid) andalso is_process_alive(Pid) of
    true ->
      State = gen_server:call(Pid, state),
      CallId = proplists:get_value(callId, State),
      InCall = proplists:get_value(inCall, State),
      im_logger:info(undefined, "[call_sup] try to join to existing call ~p", [im_common:format_id(CallId)]),

      IsUserAlreadyInCall = lists:filter(fun(P) ->
        im_common:format_id(P) =:= im_common:format_id(UserId)
      end, InCall) =/= [],

      case IsUserAlreadyInCall of
        true -> remove_participant(CallId, UserId);
        _ -> skip
      end,

      %% join to exist call on rtc side
      case im_rtc_communicator:join(UserId, CallId) of
        #'RtcMessageResp'{} -> {ok, exist, CallId};
        Error ->
          im_logger:error(undefined, "[call_sup] can't join to existing call ~p", [Error]),
          %% Todo destroy and create new if call not found in rtc server.
          destroy(CallId),
          create_new_call(UserId, RosterFeedId),
          {error, failed_to_join}
      end;
    false ->
      CallId = create_new_call(UserId, RosterFeedId),
      {ok, new, CallId}
  end.

add_participant(CallId, UserId) ->
  case im_ets:get_one(?CALL_INFO_STORAGE, im_common:parse_id(CallId)) of
    {_, CallInfo = #call_info{}} ->
      try gen_server:call(im_worker_pool:get(?MODULE, CallInfo#call_info.rosterFeedId), {add, UserId}) catch
        Error:Reason ->
          im_logger:error(undefined, "[call_sup] join terminated: ~p. Reason: ~p", [Error, Reason]),
          {error, Reason}
      end;
    _ ->
      {error, call_not_found}
  end.

remove_participant(CallId, UserId) ->
  case im_ets:get_one(?CALL_INFO_STORAGE, im_common:parse_id(CallId)) of
    {_, CallInfo = #call_info{}} ->
      try gen_server:call(im_worker_pool:get(?MODULE, CallInfo#call_info.rosterFeedId), {remove, UserId}) catch
        Error:Reason ->
          im_logger:error(undefined, "[call_sup] leave terminated: ~p. Reason: ~p", [Error, Reason]),
          {error, Reason}
      end;
    _ ->
      {error, call_not_found}
  end.

create_new_call(UserId, RosterFeedId) ->
  CallId = ctail:next_id(),
  im_logger:info(undefined, "[call_sup] create new call ~p", [im_common:format_id(CallId)]),
  im_worker_pool:cleanup(?MODULE, RosterFeedId),
  im_worker_pool:ensure(?MODULE, RosterFeedId, [CallId, RosterFeedId, UserId]),
  im_ets:put(?CALL_INFO_STORAGE, {CallId, #call_info{rosterFeedId = RosterFeedId}}),
  CallId.

decline(UserId, CallId) ->
  case im_ets:get_one(?CALL_INFO_STORAGE, im_common:parse_id(CallId)) of
    {_, CallInfo = #call_info{}} ->
      try gen_server:call(im_worker_pool:get(?MODULE, CallInfo#call_info.rosterFeedId), {decline, UserId}) catch
        Error:Reason ->
          im_logger:error(undefined, "[call_sup] decline terminated: ~p. Reason: ~p", [Error, Reason]),
          {error, Reason}
      end;
    _ ->
      {error, call_not_found}
  end.

destroy(CallId) ->
  Fun = fun() ->
    CallIdParsed = im_common:parse_id(CallId),
    case im_ets:get_one(?CALL_INFO_STORAGE, CallIdParsed) of
      {CallIdParsed, #call_info{rosterFeedId = RosterFeedId}} ->
        gen_server:call(im_worker_pool:get(?MODULE, RosterFeedId), mark_as_destroyed);
      _ ->
%%      im_logger:error(undefined, "[call_sup] destroy call_not_found or already destroyed", []),
        {error, call_not_found}
    end
  end,

  try Fun() catch
    Error:Reason ->
      im_logger:error(undefined, "[call_sup] destroy terminated: ~p. Reason: ~p", [Error, Reason]),
      {error, Reason}
  end.

is_missed(CallId, UserId) ->
  case ctail:get(im_call, CallId) of
    {ok, Call} ->
      UserDidNotAnswer = not lists:member(UserId, Call#im_call.participated),
      UserIsNotInitiator = UserId =/= Call#im_call.initiatorId,
      % im_logger:info(UserId, "IS_MISSED: ~p", [[UserDidNotAnswer, UserIsNotInitiator, Call#im_call.status =:= ?RTC_CALL_STATUS_TIMED_OUT]]),
      UserDidNotAnswer andalso UserIsNotInitiator andalso Call#im_call.status =:= ?RTC_CALL_STATUS_MISSED;
    _ -> false
  end.

find_user_call(UserId, CallId) ->
  case ctail:get(im_call_user_lookup, {im_common:parse_id(CallId), im_common:parse_id(UserId)}) of
    {ok, #im_call_user_lookup{feedCallId = FeedCallId}} -> ctail:get(im_call, FeedCallId);
    _ -> {error, not_found}
  end.

cleanup_call(RosterFeedId, CallId) ->
  im_logger:info(undefined, "[im_call_sup] cleanup call: ~p", [CallId]),
  im_ets:delete(?CALL_INFO_STORAGE, im_common:parse_id(CallId)),
  im_rtc_communicator:cleanup(CallId),
  im_worker_pool:cleanup(?MODULE, RosterFeedId).
