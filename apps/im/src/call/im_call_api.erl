-module(im_call_api).

-include("im_common.hrl").

-export([get/2, get_one/2, init/3, join/2, decline/2, leave/2, ice_candidate/2, receive_media_from/2]).

get(#'Calls'{ref=Ref, top=Top, stop=Stop, count=Count}, UserId) ->
  Calls = ctail_feed:get(im_call, {<<"calls">>, UserId}, im_common:parse_id(Top), im_common:parse_id(Stop), Count),
  CallsResolved = lists:map(fun(Call) -> Call#im_call{id=Call#im_call.callId} end, Calls),
  #'CallsResp'{ref=Ref, calls=[im_dto:format_call(Call) || Call <- CallsResolved]}.

get_one(#'Call7'{ref=Ref, id=CallId}, UserId) ->
  case im_call_sup:find_user_call(UserId, CallId) of
    {ok, Call} -> #'CallResp7'{ref=Ref, call = im_dto:format_call(Call)};
    _ -> #'ErrorResp'{ref=Ref, code=?ERROR_CODE_NOT_FOUND}
  end.

init(#'InitCall'{ref=Ref, feedType=FeedType, feedId=FeedId}, UserId, _TokenInfo) ->
  case im_call_sup:init_call(FeedType, im_common:parse_id(FeedId), UserId) of
    {ok, new, CallId} ->
      UserIds = im_message:target_user_ids(FeedType, im_common:parse_id(FeedId), UserId) ++ [UserId],
      im_rtc_communicator:init(UserId, CallId, [im_common:format_id(Participant) || Participant <- UserIds]),
      #'InitCallResp'{ref=Ref, callId=im_common:format_id(CallId)};
    {ok, exist, CallId} ->
      #'InitCallResp'{ref=Ref, callId=im_common:format_id(CallId)};
    _ ->
      #'ErrorResp'{ref=Ref, code=?ERROR_CODE_UNKNOWN}
  end.

join(#'JoinCall7'{ref=Ref, callId=CallId}, UserId) ->
  im_rtc_communicator:join(UserId, CallId),
  #'JoinCallResp7'{ref=Ref}.

decline(#'DeclineCall7'{ref=Ref,  callId=CallId}, UserId) ->
  im_call_sup:decline(UserId, CallId),
  #'DeclineCallResp7'{ref=Ref}.

leave(#'LeaveCall7'{ref=Ref,  callId=CallId}, UserId) ->
  im_rtc_communicator:leave(UserId, CallId),
  #'LeaveCallResp7'{ref=Ref}.

ice_candidate(#'IceCandidate7'{ref=Ref, callId=CallId, sender=Sender, candidate=Candidate}, UserId) ->
  im_rtc_communicator:send_ice_candidate(UserId, CallId, Candidate, Sender),
  #'IceCandidateResp7'{ref=Ref}.

receive_media_from(#'ReceiveMediaFrom7'{ref=Ref, callId=CallId, sdpOffer=SdpOffer, targetUserId=TargetUser}, UserId) ->
  im_rtc_communicator:receive_media_from(UserId, CallId, SdpOffer, TargetUser),
  #'ReceiveMediaFromResp7'{ref=Ref}.

