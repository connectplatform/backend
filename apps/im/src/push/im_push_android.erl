-module(im_push_android).

-include("im_common.hrl").

-export([send_message/2]).

send_message(UserId, Msg = #im_msg{}) ->
  {FeedId, FeedType} = case Msg#im_msg.feed_id of
    {<<"chat">>, UserId, FeedId1} -> {FeedId1, ?MESSAGE_FEED_TYPE_CHAT};
    {<<"chat">>, FeedId1, UserId} -> {FeedId1, ?MESSAGE_FEED_TYPE_CHAT};
    {<<"muc">>, FeedId1}          -> {FeedId1, ?MESSAGE_FEED_TYPE_ROOM}
  end,
  Update = #im_update{
    type=?UPDATE_TYPE_MESSAGE,
    feedType=FeedType,
    feedId=FeedId,
    messageId=Msg#im_msg.id,
    ids=[Msg#im_msg.id]
  },
  UpdateBinary = term_to_binary(#'Update'{update=im_dto:format_update(UserId, Update, Msg)}),
  UpdateEncoded = im_common:ensure_binary(base64:encode_to_string(UpdateBinary)),
  Data = [{<<"update">>, UpdateEncoded}],
  im_push:send(UserId, ?PLATFORM_ANDROID, [{<<"data">>, Data}, {<<"collapse_key">>, <<"message">>}]).
