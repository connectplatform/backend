-module(im_synapse).

-include("im_common.hrl").

-export([liberate/2]).

liberate(#'LiberateSynapse'{ref=Ref, synapse=Synapse, sendToMe=SendToMe}, UserId) ->
  FeedType = Synapse#'SynapseEntity'.feedType,
  FeedId = im_common:parse_id(Synapse#'SynapseEntity'.feedId),

  TargetUserIds = im_message:target_user_ids(FeedType, FeedId, UserId),
  TargetUserIds1 = case SendToMe =:= true of
    true -> TargetUserIds ++ [UserId];
    false -> TargetUserIds
  end,

  Synapse1 = Synapse#'SynapseEntity'{sender=im_common:format_id(UserId)},

  lists:foreach(fun(TargetUserId) ->
    Synapse2 = case Synapse#'SynapseEntity'.feedType =:= ?MESSAGE_FEED_TYPE_CHAT andalso TargetUserId =/= UserId of
      true -> Synapse1#'SynapseEntity'{feedId=im_common:format_id(UserId)};
      false -> Synapse1
    end,
    im_user_state:broadcast(UserId, [TargetUserId], #'SynapseDiscovered'{synapse=Synapse2})
  end, TargetUserIds1),

  #'LiberateSynapseResp'{ref=Ref}.
