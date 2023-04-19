-module(im_roster).
-behaviour(supervisor).

-include("im_common.hrl").

-export([init/1]).
-export([feed_id/3, create_group/1, private/4, public/3, retrieve/6]).

init(WorkerSpec) ->
  {ok, {{simple_one_for_one, 5, 10}, [WorkerSpec]}}.

create_group(Group=#im_grp{}) ->
  Id = ctail:next_id(),
  Now = sm:now(),
  Group1 = Group#im_grp{id=Id, created=Now, updated=Now, deleted=false},
  case ctail:put(Group1) of
    ok              -> {ok, Group1};
    {error, Reason} -> {error, Reason}
  end.

feed_id(muc, ToId, _UserId) -> {<<"muc">>, im_common:parse_id(ToId)};
feed_id(Type, UserId1, UserId2) when Type =:= chat ->
  BinaryType = atom_to_binary(Type, utf8),
  U1 = im_common:parse_id(UserId1),
  U2 = im_common:parse_id(UserId2),
  case U1 < U2 of
    true  -> {BinaryType, U1, U2};
    false -> {BinaryType, U2, U1}
  end.

private(FeedName, ToId, FromId, Message) ->
  message(feed_id(FeedName, ToId, FromId), ToId, FromId, Message).

public(ToId, FromId, Message) ->
  message(feed_id(muc, ToId, FromId), ToId, FromId, Message).

message(FeedId, ToId, FromId, Message=#im_msg{origin=Origin, recipient=Recipient}) ->
  Origin1 = case Origin =:= undefined of
    true -> FromId;
    false -> Origin
  end,
  Recipient1 = case Recipient =:= undefined of
    true -> ToId;
    false -> Recipient
  end,
  NewMessage = Message#im_msg{id=ctail:next_id(),
    feed_id=FeedId,
    origin=Origin1,
    recipient=Recipient1},
  ctail_feed:add(NewMessage).

retrieve(FeedId, TopId, StopId, ?RETRIEVE_DIRECTION_UP, Count, FilterFun) ->
  ctail_feed:get(im_msg, FeedId, TopId, StopId, Count, FilterFun);
retrieve(FeedId, TopId, StopId, ?RETRIEVE_DIRECTION_DOWN, Count, FilterFun) ->
  ctail_feed:get_bottom(im_msg, FeedId, TopId, StopId, Count, FilterFun).

