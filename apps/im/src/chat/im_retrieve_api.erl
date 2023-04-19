-module(im_retrieve_api).

-include("im_common.hrl").

-export([retrieve/2]).

retrieve(#'Retrieve'{ref=Ref, feedType=FeedType, count=Count, feedId=FeedId, top=Top, stop=Stop, direction=Direction}, UserId) ->
  case im_retrieve:get_messages(UserId, FeedType, FeedId, Count, Top, Stop, Direction) of
    {error, not_a_member} -> #'ErrorResp'{ref=Ref, code=?ERROR_CODE_PERMISSION_DENIED};
    {ok, Messages} -> #'RetrieveResp'{ref=Ref, messages=[im_dto:format_message(UserId, M) || M <- Messages]}
  end.
