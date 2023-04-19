-module(im_like).
-include("im_common.hrl").

-export([count/2, is_liked/3]).
-export([like/2, dislike/2]).

count(RecordType, RecordId) ->
  case ctail:get(im_like_count, id(RecordType, RecordId)) of
    {ok, LikeCount} -> LikeCount#im_like_count.count;
    {error, _}      -> 0
  end.

is_liked(RecordType, RecordId, UserId) ->
  case ctail:get(im_like, id(RecordType, RecordId, UserId)) of
    {ok, _}    -> true;
    {error, _} -> false
  end.

like(#'Like'{ref=Ref, recordType=RecordType, recordId=RecordId}, UserId) ->
  RecordId1 = im_common:parse_id(RecordId),
  Selector = id(RecordType, RecordId1, UserId),
  case ctail:get(im_like, Selector) of
    {ok, _} ->
      skip;
    {error, _} ->
      Type = case im_common:format_utf8(RecordType) =:= <<"feed_post">> of
        true ->
          {ok, FeedPost} = ctail:get(im_feed_post, RecordId1),
          FeedPost#im_feed_post.type;
        false ->
          undefined
      end,
      NewLike = #im_like{id=Selector,
        type=Type,
        userId=UserId,
        recordType=im_common:format_utf8(RecordType),
        recordId=RecordId1,
        created=sm:now()},
      ctail:put(NewLike),
      ctail_mongo:exec(update, [
        <<"im_like_count">>, {<<"_id">>, ctail_mongo:to_binary(id(RecordType, RecordId1), true)},
        {<<"$inc">>, {<<"count">>, 1}}, true, false]
      )
  end,
  #'LikeResp'{ref=Ref}.

dislike(#'Dislike'{ref=Ref, recordType=RecordType, recordId=RecordId}, UserId) ->
  RecordId1 = im_common:parse_id(RecordId),
  case ctail:get(im_like, id(RecordType, RecordId1, UserId)) of
    {ok, Like} ->
      ctail:delete(im_like, Like#im_like.id),
      ctail_mongo:exec(update, [
        <<"im_like_count">>, {<<"_id">>, ctail_mongo:to_binary(id(RecordType, RecordId1), true)},
        {<<"$inc">>, {<<"count">>, -1}}, true, false]
      );
    {error, _} ->
      skip
  end,
  #'DislikeResp'{ref=Ref}.

id(RecordType, RecordId)         -> {<<"recordType">>, im_common:ensure_binary(RecordType), <<"recordId">>, RecordId}.
id(RecordType, RecordId, UserId) -> {<<"userId">>, UserId, <<"recordType">>, im_common:ensure_binary(RecordType), <<"recordId">>, RecordId}.
