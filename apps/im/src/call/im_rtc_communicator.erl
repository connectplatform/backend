-module(im_rtc_communicator).

-include("im_common.hrl").

-export([init/3, join/2, leave/2, cleanup/1, cleanup/2, receive_media_from/4, send_ice_candidate/4]).
-export([receive_message/1]).

%% received from client and send to rtc server
init(UserId, CallId, Participants) ->
  send(#'RtcMessage'{
    name = "InitCall",
    userId = im_common:format_id(UserId),
    jsonData = jsx:encode([
      {<<"callId">>, im_common:format_id(CallId)},
      {<<"participants">>, Participants}
    ])
  }).

join(UserId, CallId) ->
  send(#'RtcMessage'{
    name = "Join",
    userId = im_common:format_id(UserId),
    jsonData = jsx:encode([
      {<<"callId">>, im_common:format_id(CallId)}
    ])
  }).

leave(UserId, CallId) ->
  send(#'RtcMessage'{
    name = "Leave",
    userId = im_common:format_id(UserId),
    jsonData = jsx:encode([
      {<<"callId">>, im_common:format_id(CallId)}
    ])
  }).

cleanup(CallId) ->
  send(#'RtcMessage'{
    name = "CleanupCall",
    jsonData = jsx:encode([
      {<<"callId">>, im_common:format_id(CallId)}
    ])
  }).

cleanup(UserId, CallId) ->
  send(#'RtcMessage'{
    name = "CleanupUser",
    jsonData = jsx:encode([
      {<<"callId">>, im_common:format_id(CallId)},
      {<<"userId">>, im_common:format_id(UserId)}
    ])
  }).

receive_media_from(UserId, CallId, SdpOffer, TargetUserId) ->
  send(#'RtcMessage'{
    name = "ReceiveVideoFrom",
    userId = im_common:format_id(UserId),
    jsonData = jsx:encode([
      {<<"callId">>, im_common:format_id(CallId)},
      {<<"sdpOffer">>, SdpOffer},
      {<<"target">>, im_common:format_id(TargetUserId)}
    ])
  }).

send_ice_candidate(UserId, CallId, IceCandidateEntity = #'IceCandidateEntity7'{}, SenderUserId) ->
  send(#'RtcMessage'{
    name = "OnIceCandidate",
    userId = im_common:format_id(UserId),
    jsonData = jsx:encode([
      {<<"callId">>, im_common:format_id(CallId)},
      {<<"candidate">>, ice_candidate_from_entity_to_json(IceCandidateEntity)},
      {<<"sender">>, im_common:format_id(SenderUserId)}
    ])
  }).

%% received from client and send to rtc server
send(#'RtcMessage'{ref = Ref, name = Name, jsonData = JsonData, userId = UserId}) ->
  URL = "http://" ++ im_common:ensure_list(sm:env(im, rtc_url)) ++ "/connect",
  io:format("Send to RTC ~p~n", [URL]),
  Header = [],
  HTTPOptions = [{ssl, [{verify, 0}]}],
  Body = jsx:encode([
    {<<"name">>, im_common:format_utf8(Name)},
    {<<"userId">>, im_common:format_id(UserId)},
    {<<"data">>, jsx:decode(JsonData)}
  ]),
  Options = [],
  Response = httpc:request(post, {URL, Header, "application/json", Body}, HTTPOptions, Options),
  case Response of
    {ok, {{_, StatusCode, _}, _, _Data}} ->
      case StatusCode =:= 200 of
        true ->
          im_logger:info(undefined, "[im_rtc_communication][send] ~p: ~p", [Name, StatusCode]),
          #'RtcMessageResp'{ref = Ref};
        false ->
          im_logger:error(undefined, "[im_rtc_communication][send] ~p: ~p", [Name, StatusCode]),
          #'ErrorResp'{
            code = ?ERROR_CODE_UNKNOWN,
            ref = Ref,
            messageParams = [],
            message = im_common:format_utf8(<<"rtc server return error">>)}
      end;
    _ ->
      im_logger:error(undefined, "[im_rtc_communication][send] ~p: UnknownError", [Name]),
      #'ErrorResp'{
        code = ?ERROR_CODE_UNKNOWN,
        ref = Ref,
        messageParams = [],
        message = im_common:format_utf8(<<"rtc server not respond">>)}
  end.

%% received from rtc server and send to clients
receive_message(#'RtcMessage'{ref = Ref, name = Name, jsonData = JsonData, userId = UserId}) ->
  im_logger:info(undefined, "[im_rtc_communication][receive_message] ~p", [im_common:format_utf8(Name)]),
  handle_received_message(Name, jsx:decode(JsonData), UserId),
  #'RtcMessage'{ref = Ref}.

handle_received_message(<<"CallCreated">>, Data, _) ->
  _CallId = proplists:get_value(<<"callId">>, Data),
  ok;
handle_received_message(<<"CallDestroyed">>, Data, _) ->
  CallId = proplists:get_value(<<"callId">>, Data),
  im_call_sup:destroy(CallId),
  ok;
handle_received_message(<<"UserAdded">>, Data, _) ->
  CallId = proplists:get_value(<<"callId">>, Data),
  Who = im_common:parse_id(proplists:get_value(<<"who">>, Data)),
  im_call_sup:add_participant(CallId, Who);
handle_received_message(<<"UserRemoved">>, Data, _) ->
  CallId = proplists:get_value(<<"callId">>, Data),
  Who = im_common:parse_id(proplists:get_value(<<"who">>, Data)),
  im_call_sup:remove_participant(CallId, Who);
handle_received_message(<<"ExistingParticipants">>, Data, UserId) ->
  CallId = proplists:get_value(<<"callId">>, Data),
  Participants = proplists:get_value(<<"participants">>, Data),
  Msg = #'ExistingParticipants7'{callId = CallId, participants = Participants},
  broadcast_to_user(UserId, Msg);
handle_received_message(<<"NewParticipantArrived">>, Data, UserId) ->
  CallId = im_common:parse_id(proplists:get_value(<<"callId">>, Data)),
  Who = im_common:parse_id(proplists:get_value(<<"who">>, Data)),
  Msg = #'NewParticipantArrived7'{callId = im_common:format_id(CallId), who = im_common:format_id(Who)},
  broadcast_to_user(UserId, Msg);
handle_received_message(<<"ParticipantLeft">>, Data, UserId) ->
  CallId = im_common:parse_id(proplists:get_value(<<"callId">>, Data)),
  Who = im_common:parse_id(proplists:get_value(<<"who">>, Data)),
  Msg = #'ParticipantLeft7'{callId = im_common:format_id(CallId), who = im_common:format_id(Who)},
  broadcast_to_user(UserId, Msg);
handle_received_message(<<"IceCandidate">>, Data, UserId) ->
  CallId = proplists:get_value(<<"callId">>, Data),
  Candidate = proplists:get_value(<<"candidate">>, Data),
  CandidateEntity = ice_candidate_from_json_to_entity(Candidate),
  Sender = proplists:get_value(<<"sender">>, Data),
  Msg = #'OnIceCandidate7'{callId = CallId, candidate = CandidateEntity, sender = Sender},
  broadcast_to_user(UserId, Msg);
handle_received_message(<<"ReceiveVideoAnswer">>, Data, UserId) ->
  CallId = proplists:get_value(<<"callId">>, Data),
  SdpAnswer = proplists:get_value(<<"sdpAnswer">>, Data),
  TargetUserId = proplists:get_value(<<"target">>, Data),
  Msg = #'ReceiveMediaAnswer7'{callId = CallId, sdpAnswer = SdpAnswer, targetUserId = TargetUserId},
  broadcast_to_user(UserId, Msg);
handle_received_message(<<"InitCallResponse">>, Data, UserId) ->
  skip;
handle_received_message(<<"IncomingCall">>, Data, UserId) ->
  skip;
handle_received_message(<<"Error">>, Data, UserId) ->
  skip;
handle_received_message(MessageName, Data, _UserId) ->
  io:format("[RTC] unknown message: ~p ~p~n", [MessageName, Data]).

ice_candidate_from_entity_to_json(IceCandidateEntity = #'IceCandidateEntity7'{}) ->
  FormatString = fun(Value) ->
    case Value of
      undefined -> null;
      _ -> Value
    end
  end,
  FormatInteger = fun(V) ->
    case im_common:format_integer(V) of
      undefined -> null;
      V1 -> V1
    end
  end,
  [
    {<<"candidate">>, FormatString(IceCandidateEntity#'IceCandidateEntity7'.candidate)},
    {<<"sdpMLineIndex">>, FormatInteger(IceCandidateEntity#'IceCandidateEntity7'.sdpMLineIndex)},
    {<<"sdpMid">>, FormatString(IceCandidateEntity#'IceCandidateEntity7'.sdpMid)}
  ].

ice_candidate_from_json_to_entity(JsonData) ->
  FormatInteger = fun(V) ->
    case V of
      V when is_binary(V) -> im_common:ensure_integer(im_common:ensure_list(V));
      V when is_integer(V) -> V;
      V when is_list(V) -> im_common:ensure_integer(V)
    end
  end,
  FormatString = fun(V) ->
    V
  end,
  #'IceCandidateEntity7'{
    candidate = FormatString(proplists:get_value(<<"candidate">>, JsonData)),
    sdpMLineIndex = FormatInteger(proplists:get_value(<<"sdpMLineIndex">>, JsonData)),
    sdpMid = FormatString(proplists:get_value(<<"sdpMid">>, JsonData))
  }.

broadcast_to_user(UserId, Msg) ->
  lists:foreach(fun(Pid) ->
    Pid ! im_transport_router:format_record(Msg)
  end, im_user_state:pids(im_common:parse_id(UserId))).
