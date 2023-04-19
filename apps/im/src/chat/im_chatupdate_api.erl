-module(im_chatupdate_api).

-include("im_common.hrl").

-export([get/2, mark_as_read/2, delete/2]).

get(#'ChatUpdates'{ref=Ref, feedType=FeedType, feedId=FeedId, syncTime=SyncTime}, UserId) ->
  SyncTime1 = case SyncTime =:= undefined of
    true -> 0;
    false -> SyncTime
  end,
  User = im_roster_chat:get(UserId),
  Updates = im_chatupdate:get(User, FeedType, im_common:parse_id(FeedId)),
  UpdatesFormatted = [im_dto:format_chat_update(UserId, Update) || Update <- lists:reverse(Updates), Update#im_chat_update.timestamp > SyncTime1 - 60 * 60 * 1000],
  #'ChatUpdatesResp'{ref=Ref, serverTime=sm:now(), updates=UpdatesFormatted}.

mark_as_read(#'MarkAsRead'{ref=Ref, feedType=FeedType, feedId=FeedId}, UserId) ->
  im_chatupdate:mark_as_read(UserId, FeedType, FeedId),
  #'MarkAsReadResp'{ref=Ref}.

delete(#'DeleteChat'{ref=Ref, feedType=FeedType, feedId=FeedId}, UserId) ->
  im_chatupdate:delete(UserId, FeedType, FeedId),
  #'DeleteChatResp'{ref=Ref}.
