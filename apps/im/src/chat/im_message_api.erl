-module(im_message_api).

-include("im_common.hrl").

-export([get_updates/2, send/2, edit/2, delete/2, star/2, get_starred/2, typing/2, report_message/2]).

get_updates(#'Updates'{ref=Ref, top=Top, stop=Stop, count=Count}, UserId) ->
  Updates = [im_dto:format_update(UserId, Update) || Update <- im_message:get_updates(UserId, Top, Stop, Count)],
  #'UpdatesResp'{ref=Ref, updates=Updates}.

send(#'Message'{ref=Ref, feedType=FeedType, feedId=FeedId, message=Message}, UserId)->
  Result = im_message:send(UserId, FeedType, im_common:parse_id(FeedId), im_dto:parse_message(Message)),
  case Result of
    {ok, Msg}       -> #'MessageResp'{ref=Ref, message=im_dto:format_message(UserId, Msg)};
    {error, Reason} -> send_error_to_resp(Ref, Reason)
  end.

send_error_to_resp(Ref, Reason) ->
  case Reason of
    invalid_feedtype -> #'ErrorResp'{code=?ERROR_CODE_FEED_TYPE_NOT_VALID, ref=Ref, message=im_common:format_utf8(im_trans:t(<<"messaging.feedtype.not.valid">>))};
    blocked          -> #'ErrorResp'{code=?ERROR_CODE_YOU_BLOCKED, ref=Ref, message=im_common:format_utf8(im_trans:t(<<"messaging.you.blocked">>))};
    group_deleted    -> #'ErrorResp'{code=?ERROR_CODE_GROUP_DELETED, ref=Ref, message=im_common:format_utf8(im_trans:t(<<"messaging.group.deleted">>))};
    not_a_member     -> #'ErrorResp'{code=?ERROR_CODE_NOT_A_MEMBER, ref=Ref, message=im_common:format_utf8(im_trans:t(<<"messaging.not.a.member">>))};
    payload_to_big   -> #'ErrorResp'{code=?ERROR_CODE_FIELD_LENGTH_NOT_VALID, ref=Ref, message=im_common:format_utf8(im_trans:t(<<"messaging.field.length.not.valid">>))};
    _                -> #'ErrorResp'{code=?ERROR_CODE_UNKNOWN, ref=Ref, message=im_common:format_utf8(im_trans:t(<<"common.unknown.error">>))}
  end.

edit(#'EditMessage'{ref=Ref, message=Message}, UserId) ->
  Result = im_message:edit(UserId, im_dto:parse_message(Message)),
  case Result of
    {ok, EditedMessage}        -> #'EditMessageResp'{ref=Ref, message=im_dto:format_message(UserId, EditedMessage)};
    {error, not_found}         -> #'ErrorResp'{ref=Ref, code=?ERROR_CODE_NOT_FOUND};
    {error, permission_denied} -> #'ErrorResp'{ref=Ref, code=?ERROR_CODE_PERMISSION_DENIED}
  end.

delete(#'Delete'{ref=Ref, feedType=FeedType, feedId=FeedId, ids=Ids}, UserId) ->
  case im_message:delete(UserId, FeedType, FeedId, Ids) of
    {ok, Ids}  -> #'DeleteResp'{ref=Ref, ids=Ids};
    {error, _} -> #'ErrorResp'{ref=Ref, code=?ERROR_CODE_UNKNOWN}
  end.

star(#'Star'{ref=Ref, messageIds=MessageIds, star=Star}, UserId) ->
  ok = im_message:star(UserId, MessageIds, Star),
  #'StarResp'{ref=Ref}.

get_starred(#'GetStarred'{ref=Ref, skip=Skip, limit=Limit}, UserId) ->
  {Messages, Total} = im_message:get_starred(UserId, Skip, Limit),
  MessagesFormatted = [im_dto:format_message(UserId, M) || M <- Messages],
  #'GetStarredResp'{ref=Ref, messages=MessagesFormatted, total=Total}.

typing(Msg=#'Typing'{feedType=FeedType, feedId=FeedId}, UserId) ->
  Msg1 = Msg#'Typing'{userId=im_common:format_id(UserId)},

  case FeedType of
    ?MESSAGE_FEED_TYPE_CHAT ->
      im_user_state:broadcast(undefined, [im_common:parse_id(FeedId)], Msg1#'Typing'{feedId=im_common:format_id(UserId)});
    ?MESSAGE_FEED_TYPE_ROOM ->
      Fun = fun(OtherUserId) ->
        case OtherUserId =/= UserId of
          true  -> im_user_state:broadcast(undefined, [OtherUserId], Msg1#'Typing'{feedId=FeedId});
          false -> skip
        end
      end,
      lists:foreach(Fun, im_roster_muc:list(FeedId));
    _ ->
      skip
  end,
  none.

report_message(#'ReportMessage'{ref=Ref, messageId=MessageId}, _UserId) ->
  [ctail:put(R#im_report{status = <<"old">>}) || R <- ctail_mongo:find(im_report, #{<<"$query">> => #{<<"messageEntity.id">> => MessageId}}, 0)],
  im_message:wipe_message(MessageId),
  #'ReportMessageResp'{ref=Ref}.
