-module(im_pointer).

-include("im_common.hrl").

-export([update/2]).
-export([delivered/4, seen/4]).
-export([get_chatupdate/2]).

update(#'Pointer'{ref=Ref, feedType=FeedType, feedId=FeedId, delivered=Delivered, seen=Seen}, UserId) ->
  FeedId1 = im_common:parse_id(FeedId),
  delivered(FeedType, FeedId1, UserId, Delivered),
  seen(FeedType, FeedId1, UserId, Seen),
  #'PointerResp'{ref=Ref}.

delivered(_, _, _, undefined) -> skip;
delivered(_, _, _, 0) -> skip;
delivered(?MESSAGE_FEED_TYPE_ROOM, FeedId,  UserId, Delivered) ->
  UpdateId = im_roster:feed_id(muc, FeedId, UserId),
  case get_chatupdate(UserId, UpdateId) of
    {ok, Update} ->
      case Delivered > Update#im_chat_update.delivered of
        true ->
          lists:foreach(fun(TargetUserId) ->
            case get_chatupdate(TargetUserId, UpdateId) of
              {ok, TargetUpdate} ->
                NewTargetUpdate = TargetUpdate#im_chat_update{delivered=Delivered},
                put_chatupdate(TargetUserId, NewTargetUpdate),
                im_chatupdate:send_update(TargetUserId, NewTargetUpdate);
              _ -> skip
            end
          end, im_message:target_user_ids(?MESSAGE_FEED_TYPE_ROOM, FeedId, UserId));
        false -> skip
      end;
    {error, not_found} ->
      skip
  end;
delivered(?MESSAGE_FEED_TYPE_CHAT, FeedId,  UserId, Delivered) ->
  UpdateId = im_roster:feed_id(chat, UserId, FeedId),
  case get_chatupdate(FeedId, UpdateId) of
    {ok, RecipientsUpdate} ->
      case Delivered > RecipientsUpdate#im_chat_update.delivered of
        true ->
          NewRecipientsUpdate = RecipientsUpdate#im_chat_update{delivered=Delivered},
          put_chatupdate(FeedId, NewRecipientsUpdate),
          im_chatupdate:send_update(FeedId, NewRecipientsUpdate);
        false -> skip
      end;
    {error, not_found} -> skip
  end.

seen(_, _, _, undefined) -> skip;
seen(_, _, _, 0) -> skip;
seen(?MESSAGE_FEED_TYPE_CHAT, FeedId, UserId, Seen) ->
  UpdateId = im_roster:feed_id(chat, UserId, FeedId),
  seen_inner(UpdateId, FeedId, UserId, Seen);
seen(?MESSAGE_FEED_TYPE_ROOM, FeedId, UserId, Seen) ->
  UpdateId = im_roster:feed_id(muc, FeedId, UserId),
  case get_chatupdate(UserId, UpdateId) of
    {ok, MyUpdate} ->
      Seen1 = case Seen =:= -1 of
        true -> MyUpdate#im_chat_update.lastSendSeen;
        false -> Seen
      end,
      case Seen1 >= MyUpdate#im_chat_update.lastSendSeen of
        true ->
          %% Update MyUpdate unread count
          Unread = calc_unread_count(UserId, Seen1, ctail:get(im_msg, MyUpdate#im_chat_update.top), 0),
          MyUpdate1 = MyUpdate#im_chat_update{unread=Unread, lastSendSeen=Seen1},
          put_chatupdate(UserId, MyUpdate1),
          im_chatupdate:send_update(UserId, MyUpdate1),

          %% Update room users delivered seen pointers.
          TargetUserIds = im_message:target_user_ids(?MESSAGE_FEED_TYPE_ROOM, FeedId, UserId),
          lists:foreach(fun(TargetUserId) ->
            case get_chatupdate(TargetUserId, UpdateId) of
              {ok, TargetUpdate} ->
                TargetSeen = case Seen1 > TargetUpdate#im_chat_update.seen of
                  true  -> Seen1;
                  false -> TargetUpdate#im_chat_update.seen
                end,
                TargetDelivered = case Seen1 > TargetUpdate#im_chat_update.delivered of
                  true  -> Seen1;
                  false -> TargetUpdate#im_chat_update.delivered
                end,
                TargetUpdate1 = TargetUpdate#im_chat_update{seen=TargetSeen, delivered=TargetDelivered},
                put_chatupdate(TargetUserId, TargetUpdate1),
                im_chatupdate:send_update(TargetUserId, TargetUpdate1);
              _ -> skip
            end
          end, TargetUserIds);
        false ->
          skip
      end;
    {error, not_found} ->
      skip
  end.

seen_inner(UpdateId, ?SYS_USER_ID, UserId, Seen) ->
  case get_chatupdate(UserId, UpdateId) of
    {ok, MyUpdate} ->
      TopId = MyUpdate#im_chat_update.top,
      Top = ctail:get(im_msg, TopId),
      Unread = calc_unread_count(UserId, Seen, Top, 0),
      Delivered = case Seen > MyUpdate#im_chat_update.delivered of
        true -> Seen;
        false -> MyUpdate#im_chat_update.delivered
      end,
      MyNewUpdate = MyUpdate#im_chat_update{unread=Unread, seen=Seen, delivered=Delivered},
      put_chatupdate(UserId, MyNewUpdate);
    {error, not_found} ->
      skip
  end;
seen_inner(UpdateId, FeedId, UserId, Seen) ->
  case get_chatupdate(FeedId, UpdateId) of
    {ok, RecipientsUpdate} ->
      case Seen >= RecipientsUpdate#im_chat_update.seen orelse Seen =:= -1 of
        true  ->
          case get_chatupdate(UserId, UpdateId) of
            {ok, MyUpdate} ->
              TopId = MyUpdate#im_chat_update.top,
              Top = ctail:get(im_msg, TopId),
              Seen1 = case Seen =:= -1 of
                true -> RecipientsUpdate#im_chat_update.seen;
                false -> Seen
              end,
              Delivered = case Seen1 > RecipientsUpdate#im_chat_update.delivered of
                true -> Seen1;
                false -> RecipientsUpdate#im_chat_update.delivered
              end,
              Unread = calc_unread_count(UserId, Seen1, Top, 0),
              case Unread =/= MyUpdate#im_chat_update.unread of
                true ->
                  RecipientsUpdate1 = RecipientsUpdate#im_chat_update{seen=Seen1, delivered=Delivered},
                  MyUpdate1 = MyUpdate#im_chat_update{unread=Unread, lastSendSeen=Seen1},
                  put_chatupdate(FeedId, RecipientsUpdate1),
                  put_chatupdate(UserId, MyUpdate1),
                  im_chatupdate:send_update(FeedId, RecipientsUpdate1),
                  im_chatupdate:send_update(UserId, MyUpdate1, true);
                false ->
                  skip
              end;
            {error, not_found} ->
                skip
          end;
        false ->
          skip
      end;
    {error, not_found} ->
      skip
  end.

get_chatupdate(UserId, UpdateId) ->
  User = im_roster_chat:get(UserId),
  User1 = im_chatupdate:parse(User),
  im_chatupdate:find(UpdateId, User1). %%  Return {ok, Update} or {error, not_found}

put_chatupdate(UserId, Update=#im_chat_update{id=FeedId}) ->
  {FeedType, FeedId1} = im_chatupdate:roster_to_msg_feed(FeedId, UserId),
  im_chatupdate:quick_update(UserId, FeedType, FeedId1, fun(_, _) -> {none, Update} end).

calc_unread_count(_UserId,      _,    {error, _},    Unread) ->
  % im_logger:debug(UserId, "RETURN_UNREAD: ~p", [Unread]),
  Unread;
calc_unread_count(UserId, Seen, {ok, Message}, Unread) ->
  % im_logger:debug(UserId, "SCAN: ~p", [Message]),
  calc_unread_count(UserId, Seen, Message, Unread);
calc_unread_count(UserId, Seen, Message,       Unread) ->
  Prev = case Message#im_msg.prev of
    undefined -> {error, end_of_feed};
    _         -> ctail:get(im_msg, Message#im_msg.prev)
  end,

  IsMyMessage = Message#im_msg.origin =:= UserId,
  IsVisible = im_message:is_visible(UserId, Message),
  DoesntCount = IsMyMessage orelse not IsVisible,

  % im_logger:debug(UserId, "SUMMARY. MY: ~p, VISIBLE: ~p, SKIP: ~p, UNREAD: ~p", [IsMyMessage, IsVisible, DoesntCount, Message#im_msg.created > Seen]),

  case DoesntCount of
    true ->
      % im_logger:debug(UserId, "SKIP", []),
      calc_unread_count(UserId, Seen, Prev, Unread);
    false ->
      IsUnread = Message#im_msg.created > Seen,
      case IsUnread of
        true ->
          % im_logger:debug(UserId, "INCREMENT. CURRENT_UNREAD: ~p", [Unread + 1]),
          calc_unread_count(UserId, Seen, Prev, Unread + 1);
        false ->
          % im_logger:debug(UserId, "RETURN_UNREAD: ~p", [Unread]),
          Unread
      end
  end.
