-module(im_call_worker).
-behaviour(gen_server).

-include("im_common.hrl").

-define(HEARTBEAT_INTERVAL, 1000).
-define(INIT_CALL_TIMEOUT, 30000).
-define(INIT_CALL_TIMEOUT_TEST, 5000).

-define(STATUS_INITIALIZED, 1).
-define(STATUS_START_PROCESSED, 2).
-define(STATUS_ENDED, 3).
-define(STATUS_TIMEOUT, 4).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/3]).

-record(state, {
  rosterFeedId, %% {chat, UId1, UId2} {muc, RoomId}
  callId,
  initiatorId,

  timer, %% heartbeat

  created, %% Time when call was created
  started,  %% Time when call was started

  participated, %% Array of users who participated in the call
  inCall, %% Array of users who at this moment take a part in call
  declined,

  status,
  destroyed_on_rtc
}).

start_link(CallId, RosterFeedId, UserId) ->
  gen_server:start_link(?MODULE, [CallId, RosterFeedId, UserId], []).

init([CallId, RosterFeedId, UserId]) ->
  im_logger:info(UserId, "[im_call_worker] [~p] Initiating call", [im_common:format_id(CallId)]),
  process_flag(trap_exit, true),

  State = #state{
    rosterFeedId = RosterFeedId,
    callId = CallId,
    initiatorId = UserId,
    timer = erlang:send_after(?HEARTBEAT_INTERVAL, self(), heartbeat),
    created = sm:now(),
    status = ?STATUS_INITIALIZED,
    destroyed_on_rtc = false,
    participated = [im_common:format_id(UserId)],
    inCall = [im_common:format_id(UserId)],
    declined = []
  },

  {FeedType, InitiatorFeedId} = im_chatupdate:roster_to_msg_feed(RosterFeedId, UserId),

  %% broadcast to users
  lists:foreach(fun(TargetUserId) ->
    {_, TargetFeedId} = im_chatupdate:roster_to_msg_feed(RosterFeedId, TargetUserId),

    CallStateJson = [
      {<<"call">>, [
        {<<"callId">>, im_common:format_id(CallId)},
        {<<"expires">>, sm:now() + ?INIT_CALL_TIMEOUT},
        {<<"feedType">>, FeedType},
        {<<"feedId">>, im_common:format_id(TargetFeedId)},
        {<<"status">>, get_user_status(TargetUserId, State)},
        {<<"initiatorId">>, im_common:format_id(UserId)}]}],

    IsInitiator = im_common:format_id(TargetUserId) =:= im_common:format_id(UserId),
    case IsInitiator of
      true ->
        im_user_state:broadcast(UserId, [TargetUserId], #'OutgoingCall7'{
          callId = im_common:format_id(CallId),
          feedType = FeedType,
          feedId = im_common:format_id(TargetFeedId),
          initiatorId = im_common:format_id(UserId)
        });
      false ->
        im_push_ios:send_voip(TargetUserId, CallStateJson),
        im_user_state:broadcast(UserId, [TargetUserId], #'IncomingCall7'{
          callId = im_common:format_id(CallId),
          feedType = FeedType,
          feedId = im_common:format_id(TargetFeedId),
          initiatorId = im_common:format_id(UserId)
        })
    end,

    NotificationText = case IsInitiator of
      true -> im_common:format_utf8("Outgoing Call");
      false -> im_common:format_utf8("Incoming Call")
    end,

    im_push:send(UserId, ?PLATFORM_IOS, [
      {<<"notification">>, [{<<"body">>, NotificationText}]},
      {<<"data">>, CallStateJson},
      {<<"collapse_key">>, im_common:format_utf8(<<"call">>)}
    ]),

    im_push:send_json(TargetUserId, ?PLATFORM_ANDROID, [
      {<<"data">>, CallStateJson},
      {<<"priority">>, <<"high">>},
      {<<"android">>, [{<<"ttl">>, ms_to_fcm_format(?INIT_CALL_TIMEOUT)}]}])

  end, get_roster_chat_users(RosterFeedId)),

  %% Persist calls
  Calls = persist(State),

  %% Update chat_updates
  lists:foreach(fun(Call = #im_call{}) ->
    case Call#im_call.feed_id of
      {<<"calls">>, TargetUserId} ->
        TargetFeedId = Call#im_call.feedId,
        {User, Update} = im_chatupdate:quick_update(TargetUserId, FeedType, TargetFeedId, fun(Update, User) ->
          Update1 = Update#im_chat_update{callId = CallId},
          {{User, Update1}, Update1}
        end),
        im_chatupdate:send_update(User, Update);
      _ ->
        skip
    end
  end, Calls),

  {ok, State}.

handle_call({add, UserId}, _, State = #state{callId = CallId, participated = Participated, inCall = InCall, declined = Declined, status = Status, started = Started, rosterFeedId = RosterFeedId, initiatorId = InitiatorId}) ->
  IsUserAlreadyInCall = lists:filter(fun(P) ->
    im_common:format_id(P) =:= im_common:format_id(UserId)
  end, InCall) =/= [],

  case IsUserAlreadyInCall of
    true ->
      im_logger:error(undefined, "[im_call_worker] user [~p] already added in call [~p]", [UserId, CallId]),
      {reply, ok, State};
    false ->
      NewParticipated = (Participated -- [im_common:format_id(UserId)]) ++ [im_common:format_id(UserId)],
      NewInCall = (InCall -- [im_common:format_id(UserId)]) ++ [im_common:format_id(UserId)],
      NewDeclined = (Declined -- [im_common:format_id(UserId)]),
      NewStarted = case Status =:= ?STATUS_INITIALIZED of
        true -> sm:now();
        false -> Started
      end,
      NewState = State#state{participated = NewParticipated, inCall = NewInCall, declined = NewDeclined, status = ?STATUS_START_PROCESSED, started = NewStarted},

      persist(NewState),

      %% broadcast notifications
      lists:foreach(fun(UId) ->
        TargetUserId = im_common:parse_id(UId),
        {FeedType, TargetFeedId} = im_chatupdate:roster_to_msg_feed(RosterFeedId, TargetUserId),
        CallEndedJson = [
          {<<"call">>, [
            {<<"callId">>, im_common:format_id(CallId)},
            {<<"status">>, get_user_status(TargetUserId, NewState)},
            {<<"feedType">>, FeedType},
            {<<"feedId">>, im_common:format_id(TargetFeedId)},
            {<<"initiatorId">>, im_common:format_id(InitiatorId)}
          ]}
        ],

%%            im_push:send(TargetUserId, ?PLATFORM_IOS, [
%%              {<<"notification">>, [{<<"body">>, im_common:format_utf8("Participant Added")}]},
%%              {<<"data">>, CallEndedJson},
%%              {<<"collapse_key">>, im_common:format_utf8(<<"call">>)}
%%            ]),

        im_push:send_json(TargetUserId, ?PLATFORM_ANDROID, [
          {<<"data">>, CallEndedJson},
          {<<"priority">>, <<"high">>},
          {<<"android">>, [{<<"ttl">>, ms_to_fcm_format(?INIT_CALL_TIMEOUT)}]}])
      end, NewInCall),

      {reply, ok, NewState}
  end;
handle_call({remove, UserId}, _, State = #state{rosterFeedId = RosterFeedId, callId = CallId, initiatorId = InitiatorId, inCall = InCall, participated = Participated, declined = Declined}) ->
  IsUserNotInCall = lists:filter(fun(P) ->
    im_common:format_id(P) =:= im_common:format_id(UserId)
  end, InCall) =:= [],

  case IsUserNotInCall of
    true ->
      {reply, ok, State};
    false ->
      lists:foreach(fun(TargetUserId) ->
        im_user_state:broadcast(UserId, [TargetUserId], #'ParticipantLeft7'{
          callId = im_common:format_id(CallId),
          who = im_common:format_id(UserId)
        })
      end, get_roster_chat_users(RosterFeedId)),
      NewInCall = InCall -- [im_common:format_id(UserId)],

      case is_call_ended(RosterFeedId, Participated, NewInCall, Declined) of
        true ->
          {stop, normal, State#state{inCall = NewInCall, status = ?STATUS_ENDED}};
        false ->
          NewState = State#state{inCall = NewInCall},
          persist(NewState),
          %% broadcast notifications
          lists:foreach(fun(UId) ->
            TargetUserId = im_common:parse_id(UId),
            {FeedType, TargetFeedId} = im_chatupdate:roster_to_msg_feed(RosterFeedId, TargetUserId),
            CallEndedJson = [
              {<<"call">>, [
                {<<"callId">>, im_common:format_id(CallId)},
                {<<"status">>, get_user_status(TargetUserId, NewState)},
                {<<"feedType">>, FeedType},
                {<<"feedId">>, im_common:format_id(TargetFeedId)},
                {<<"initiatorId">>, im_common:format_id(InitiatorId)}
              ]}
            ],
%%        im_push:send(TargetUserId, ?PLATFORM_IOS, [
%%          {<<"notification">>, [{<<"body">>, im_common:format_utf8("Participant Left")}]},
%%          {<<"data">>, CallEndedJson},
%%          {<<"collapse_key">>, im_common:format_utf8(<<"call">>)}
%%        ]),
            im_push:send_json(TargetUserId, ?PLATFORM_ANDROID, [
              {<<"data">>, CallEndedJson},
              {<<"priority">>, <<"high">>},
              {<<"android">>, [{<<"ttl">>, ms_to_fcm_format(?INIT_CALL_TIMEOUT)}]}])
          end, InCall),
          {reply, ok, NewState}
      end
  end;
handle_call({decline, UserId}, _, State = #state{rosterFeedId = RosterFeedId, inCall = InCall, declined = Declined, participated = Participated}) ->
  IsUserAcceptCallForAnyTime = lists:filter(fun(P) ->
    im_common:format_id(P) =:= im_common:format_id(UserId)
  end, Participated) =/= [],

  NewDeclined = case IsUserAcceptCallForAnyTime of
    true -> Declined;
    false -> (Declined -- [im_common:format_id(UserId)]) ++ [im_common:format_id(UserId)]
  end,

  case is_call_ended(RosterFeedId, Participated, InCall, NewDeclined) of
    true ->
      {stop, normal, State#state{declined = NewDeclined, status = ?STATUS_ENDED}};
    false ->
      NewState = State#state{declined = NewDeclined},
      persist(NewState),
      {reply, ok, NewState}
  end;
handle_call(state, _, State = #state{callId = CallId, inCall = InCall}) ->
%%  io:format("GETTING STATE ~p~n~n~n", [State]),
  {reply, [
    {callId, CallId},
    {inCall, InCall}
  ], State};
handle_call(mark_as_destroyed, _, State = #state{}) ->
  {stop, normal, State#state{destroyed_on_rtc = true}};
handle_call(_Request = {}, _, State) -> {noreply, State}.

handle_cast(_Request = {}, State) -> {noreply, State}.

handle_info(heartbeat, State = #state{rosterFeedId = RosterFeedId, inCall = InCall, participated = Participated, declined = Declined, timer = OldTimer, created = Created, status = Status, destroyed_on_rtc = IsDestroyedOnRtc}) ->
  TimeOut = case im:is_test() of
    true -> ?INIT_CALL_TIMEOUT_TEST;
    false -> ?INIT_CALL_TIMEOUT
  end,
  IsTimedOut = Status =:= ?STATUS_INITIALIZED andalso sm:now() > Created + TimeOut,
%%  io:format("~n~n[is_call_ended] UsersInCallCount ~p, Participated ~p, InCall ~p, Declined ~p, IsTimedOut ~p, Created ~p, Status ~p~n", [length(get_roster_chat_users(RosterFeedId)), Participated, InCall, Declined, IsTimedOut, Created, Status]),
  IsCallEnded = is_call_ended(RosterFeedId, Participated, InCall, Declined),
  case IsTimedOut orelse IsCallEnded orelse IsDestroyedOnRtc of
    true ->
      Status1 = case IsTimedOut of
        true -> ?STATUS_TIMEOUT;
        false -> ?STATUS_ENDED
      end,
      {stop, normal, State#state{status = Status1}};
    false ->
      erlang:cancel_timer(OldTimer),
      Timer = erlang:send_after(?HEARTBEAT_INTERVAL, self(), heartbeat),
      {noreply, State#state{timer = Timer}}
  end;
handle_info({'DOWN', _MonitorRef, _Type, _Object, _Info} = _Msg, State) ->
  {stop, normal, State};
handle_info(_Info, State) -> {noreply, State}.

terminate(_, State = #state{rosterFeedId = RosterFeedId, callId = CallId, initiatorId = InitiatorId, status = Status}) ->
  im_logger:info(InitiatorId, "[im_call_worker] [~p] Call terminated with status: ~p", [im_common:format_id(CallId), Status]),

  NewStatus = case Status of
    ?STATUS_INITIALIZED -> ?STATUS_ENDED;
    ?STATUS_START_PROCESSED -> ?STATUS_ENDED;
    _ -> Status
  end,
  NewState = State#state{status = NewStatus},

  {FeedType, InitiatorFeedId} = im_chatupdate:roster_to_msg_feed(RosterFeedId, InitiatorId),

  %% Send pushes and CallEnded event
  lists:foreach(fun(TargetUserId) ->
    {_, TargetFeedId} = im_chatupdate:roster_to_msg_feed(RosterFeedId, TargetUserId),
    CallEndedJson = [
      {<<"call">>, [
        {<<"callId">>, im_common:format_id(CallId)},
        {<<"expires">>, sm:now()},
        {<<"status">>, get_user_status(TargetUserId, NewState)},
        {<<"feedType">>, FeedType},
        {<<"feedId">>, im_common:format_id(TargetFeedId)},
        {<<"initiatorId">>, im_common:format_id(InitiatorId)}
      ]}
    ],
    im_push:send(TargetUserId, ?PLATFORM_IOS, [
      {<<"notification">>, [{<<"body">>, im_common:format_utf8("Call Ended")}]},
      {<<"data">>, CallEndedJson},
      {<<"collapse_key">>, im_common:format_utf8(<<"call">>)}
    ]),
    im_push:send_json(TargetUserId, ?PLATFORM_ANDROID, [
      {<<"data">>, CallEndedJson},
      {<<"priority">>, <<"high">>},
      {<<"android">>, [{<<"ttl">>, ms_to_fcm_format(?INIT_CALL_TIMEOUT)}]}]),
    im_user_state:broadcast(InitiatorId, [TargetUserId], #'CallEnded7'{
      callId = im_common:format_id(CallId),
      feedType = FeedType,
      feedId = im_common:format_id(TargetFeedId)
    })
  end, get_roster_chat_users(RosterFeedId)),

  %% Persist calls
  Calls = persist(NewState),

  %% Update chat_updates
  lists:foreach(fun(Call = #im_call{}) ->
    case Call#im_call.feed_id of
      {<<"calls">>, TargetUserId} ->
        TargetFeedId = Call#im_call.feedId,
        {User, Update} = im_chatupdate:quick_update(TargetUserId, FeedType, TargetFeedId, fun(Update, User) ->
          Update1 = Update#im_chat_update{callId = undefined},
          {{User, Update1}, Update1}
        end),
        im_chatupdate:send_update(User, Update);
      _ ->
        skip
    end
  end, Calls),

  %% Send message into chat
  im_message:send(InitiatorId, FeedType, InitiatorFeedId, #im_msg{
    type = ?MESSAGE_TYPE_USER_MESSAGE,
    created = sm:now(),
    kind = ?MESSAGE_KIND_CALL,
    callId = CallId
  }),

  im_call_sup:cleanup_call(RosterFeedId, CallId),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

persist(State = #state{rosterFeedId = RosterFeedId, callId = CallId, initiatorId = InitiatorId, started = Started, participated = Participated}) ->
  Duration = case Started of
    undefined -> 0;
    _ -> (sm:now() - Started) / 1000
  end,

  lists:map(fun(UId) ->
    Direction = case UId of
      InitiatorId -> ?CALL_DIRECTION_OUTBOUND;
      _ -> ?CALL_DIRECTION_INBOUND
    end,
    {FeedType, FeedId} = im_chatupdate:roster_to_msg_feed(RosterFeedId, UId),
    UserStatus = get_user_status(UId, State),
    case im_call_sup:find_user_call(UId, CallId) of
      {ok, Call} ->
        Call1 = Call#im_call{
          duration = Duration,
          status = UserStatus,
          participated = Participated
        },
        ctail:put(Call1),
        Call1;
      {error, not_found} ->
        Call = #im_call{
          id = ctail:next_id(),
          feed_id = {<<"calls">>, UId},

          feedType = FeedType,
          feedId = FeedId,

          callId = CallId,
          initiatorId = InitiatorId,
          created = Started,
          direction = Direction,

          duration = Duration,
          status = UserStatus,
          participated = Participated
        },
        ctail_feed:add(Call),
        ctail:put(#im_call_user_lookup{id = {im_common:parse_id(CallId), im_common:parse_id(UId)}, feedCallId = Call#im_call.id}),
        Call
    end
  end, get_roster_chat_users(RosterFeedId)).

is_call_ended(RosterFeedId, Participated, InCall, Declined) ->
  AvailableUsersCountInCall = length(get_roster_chat_users(RosterFeedId)),
  InCall =:= []
    orelse length(InCall) =< 1 andalso length(Declined) + length(Participated) >= AvailableUsersCountInCall
    orelse length(InCall) =< 1 andalso length(Declined) + length(InCall) >= AvailableUsersCountInCall.

ms_to_fcm_format(Milliseconds) ->
  Seconds = round(Milliseconds / 1000),
  im_common:format_utf8(integer_to_list(Seconds) ++ "s").

get_user_status(UserId, #state{participated = Participated, inCall = InCall, declined = Declined, status = Status}) ->
  IsUserAcceptCallForAnyTime = lists:filter(fun(P) ->
    im_common:format_id(P) =:= im_common:format_id(UserId)
  end, Participated) =/= [],

  IsUserInCallNow = lists:filter(fun(P) ->
    im_common:format_id(P) =:= im_common:format_id(UserId)
  end, InCall) =/= [],

  case IsUserAcceptCallForAnyTime of
    true ->
      case Status =:= ?STATUS_ENDED orelse Status =:= ?STATUS_TIMEOUT of
        true ->
          IsCanceled = IsUserAcceptCallForAnyTime andalso length(Participated) =:= 1,
          case IsCanceled of
            true -> ?RTC_CALL_STATUS_CANCELED;
            false -> ?RTC_CALL_STATUS_ENDED
          end;
        false ->
          case IsUserInCallNow of
            true -> ?RTC_CALL_STATUS_SPEAKING;
            false -> ?RTC_CALL_STATUS_AVAILABLE
          end
      end;
    false ->
      case Status =:= ?STATUS_ENDED orelse Status =:= ?STATUS_TIMEOUT of
        true ->
          IsUserDeclineCall = lists:filter(fun(P) ->
            im_common:format_id(P) =:= im_common:format_id(UserId)
          end, Declined) =/= [],

          case IsUserDeclineCall of
            true -> ?RTC_CALL_STATUS_REJECTED;
            false -> ?RTC_CALL_STATUS_MISSED
          end;
        false -> ?RTC_CALL_STATUS_AVAILABLE
      end
  end.

get_roster_chat_users(RosterFeedId) ->
  case RosterFeedId of
    {<<"chat">>, UId1, UId2} ->
      [UId1, UId2];
    {<<"muc">>, FeedId} ->
      case im_roster_muc:get(im_common:parse_id(FeedId)) of
        Room = #im_grp{} -> Room#im_grp.members;
        _ -> []
      end;
    _ -> []
  end.
