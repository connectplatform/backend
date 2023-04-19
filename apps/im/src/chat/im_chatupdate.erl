-module(im_chatupdate).

-include("im_common.hrl").

-export([get/1, get/3, mark_as_read/3, delete/3]).
-export([parse/1, format/1, upsert/2, find/2, ensure/2, remove/1]).
-export([quick_update/4, quick_find/3, send_update/2, send_update/3, wait_update/2]).
-export([msg_feed_to_roster/3, roster_to_msg_feed/2]).
-export([calculate_total_unread/1]).

get(User) -> get(User, undefined, undefined).
get(FormattedUser=#im_usr{id=UserId}, FeedType, FeedId) ->
  Updates = [Update || {_, Update} <- (parse(FormattedUser))#im_usr.chatUpdates],

  lists:filter(fun(#im_chat_update{id=UpdateId}) ->
    {FeedType1, FeedId1} = im_chatupdate:roster_to_msg_feed(UpdateId, UserId),
    (FeedType =:= 0 orelse FeedType =:= undefined orelse (FeedType =/= 0 andalso FeedType =/= undefined andalso FeedType =:= FeedType1)) andalso
    (FeedId =:= undefined orelse (FeedId =/= undefined andalso FeedId =:= FeedId1))
  end, Updates).

mark_as_read(UserId, FeedType, FeedId) ->
  FeedId1 = im_common:parse_id(FeedId),

  Fun = fun (User) ->
    Now = sm:now(),
    case FeedId1 =:= undefined of
      true ->
        ChatUpdates = lists:map(fun({UpdateId, Update}) ->
          Update1 = Update#im_chat_update{delivered=Now, seen=Now, unread=0},
          {UpdateId, Update1}
        end, User#im_usr.chatUpdates),
        {ok, User#im_usr{chatUpdates=ChatUpdates}};
      false ->
        MsgFeedId = im_chatupdate:msg_feed_to_roster(FeedType, FeedId1, UserId),
        case im_chatupdate:find(MsgFeedId, User) of
          {ok, Update} ->
            Update1 = Update#im_chat_update{delivered=Now, seen=Now, unread=0},
            {ok, im_chatupdate:upsert(Update1, User)};
          {error, not_found} ->
            {none, User}
        end
    end
  end,
  im_roster_chat:execute(UserId, Fun).

calculate_total_unread(#im_usr{chatUpdates=ChatUpdates}) ->
  Fun = fun({_, #im_chat_update{unread=Unread, deleted=Deleted}}, Acc) ->
    case Deleted of
      true -> Acc;
      false -> Acc + Unread
    end
  end,
  lists:foldl(Fun, 0, ChatUpdates).

parse(User=#im_usr{chatUpdates=undefined}) -> User#im_usr{chatUpdates=[]};
parse(User=#im_usr{chatUpdates=ChatUpdates}) ->
  case ChatUpdates of
    [] -> User;
    _ ->
      ChatUpdates1 = lists:map(fun
        ({update_id_here, PropList})               -> parse_chatupdate(PropList);
        ({<<"update_id_here">>, PropList})         -> parse_chatupdate(PropList);
        ({UpdateId, ChatUpdate=#im_chat_update{}}) -> {UpdateId, ChatUpdate}
      end, ChatUpdates),
      User#im_usr{chatUpdates=ChatUpdates1}
  end.

format(User=#im_usr{chatUpdates=undefined}) -> User=#im_usr{chatUpdates=[]};
format(User=#im_usr{chatUpdates=ChatUpdates}) ->
  ChatUpdates1 = lists:map(fun
    ({update_id_here, PropList}) -> {update_id_here, PropList};
    ({<<"update_id_here">>, PropList}) -> {update_id_here, PropList};
    ({_, Update}) ->
      {update_id_here, [
        {id, tuple_to_list(Update#im_chat_update.id)},
        {timestamp, Update#im_chat_update.timestamp},
        {top, Update#im_chat_update.top},
        {delivered, Update#im_chat_update.delivered},
        {seen, Update#im_chat_update.seen},
        {lastSendSeen, Update#im_chat_update.lastSendSeen},
        {unread, Update#im_chat_update.unread},
        {hide, Update#im_chat_update.hide},
        {name, Update#im_chat_update.name},
        {thumbnail, Update#im_chat_update.thumbnail},
        {deleted, Update#im_chat_update.deleted},
        {replyKeyboardMarkup, Update#im_chat_update.replyKeyboardMarkup},
        {removeKeyboardMarkup, Update#im_chat_update.removeKeyboardMarkup},
        {callId, Update#im_chat_update.callId}
      ]}
  end, ChatUpdates),
  User#im_usr{chatUpdates=ChatUpdates1}.

upsert(Update, User=#im_usr{chatUpdates=ChatUpdates}) ->
  FeedId = Update#im_chat_update.id,
  {ChatUpdates1, IsUnreadChanged} = case proplists:lookup(FeedId, ChatUpdates) of
    none ->
      NewUpdates = [{FeedId, Update}|ChatUpdates],
      {NewUpdates, Update#im_chat_update.unread > 0};
    {_, OldUpdate} ->
      IsChanged = Update#im_chat_update.unread =/= OldUpdate#im_chat_update.unread,
      NewUpdates = lists:keyreplace(FeedId, 1, ChatUpdates, {FeedId, Update}),
      {NewUpdates, IsChanged}
  end,
  User1 = User#im_usr{chatUpdates=ChatUpdates1},
  case IsUnreadChanged of
    true ->
      % send_update(User1, Update),
      im_push_ios:send_badge(User1, Update#im_chat_update.unread);
    false -> skip
  end,
  User1.

find(FeedId, #im_usr{chatUpdates=ChatUpdates}) ->
  case proplists:lookup(FeedId, ChatUpdates) of
    none           -> {error, not_found};
    {_, OldUpdate} -> {ok, OldUpdate}
  end.

ensure(FeedId, #im_usr{chatUpdates=ChatUpdates}) ->
  case proplists:lookup(FeedId, ChatUpdates) of
    none           -> #im_chat_update{id=FeedId};
    {_, OldUpdate} -> OldUpdate
  end.

parse_chatupdate(PropList) ->
  UpdateId = list_to_tuple(get_val(id, PropList)),
  {UpdateId, #im_chat_update{
    id=UpdateId,
    timestamp=get_val(timestamp, PropList),
    top=get_val(top, PropList),
    delivered=get_val(delivered, PropList),
    seen=get_val(seen, PropList),
    lastSendSeen=get_val(lastSendSeen, PropList),
    unread=get_val(unread, PropList),
    hide=get_val(hide, PropList),
    name=get_val(name, PropList),
    thumbnail=get_val(thumbnail, PropList),
    callId=get_val(callId, PropList),
    deleted=case get_val(deleted, PropList) of
      undefined -> false;
      Deleted   -> Deleted
    end,
    replyKeyboardMarkup=get_val(replyKeyboardMarkup, PropList),
    removeKeyboardMarkup=get_val(removeKeyboardMarkup, PropList)
  }}.

remove(Update=#im_chat_update{}) ->
  Update#im_chat_update{
    deleted=true,
    delivered=0,
    seen=0,
    lastSendSeen=0,
    unread=0
  }.

get_val(Val, Proplist) ->
  case proplists:is_defined(Val, Proplist) of
    true  -> proplists:get_value(Val, Proplist);
    false -> proplists:get_value(atom_to_binary(Val, utf8), Proplist)
  end.

delete(UserId, FeedType, FeedId) ->
  {Update, User} = quick_update(UserId, FeedType, im_common:parse_id(FeedId), fun(Update, User) ->
    Update1 = remove(Update),
    {{Update1, User}, Update1}
  end),
  send_update(User, Update).

msg_feed_to_roster(FeedType, FeedId, UserId) ->
  case FeedType of
    ?MESSAGE_FEED_TYPE_CHAT -> im_roster:feed_id(chat, FeedId, UserId);
    ?MESSAGE_FEED_TYPE_ROOM -> im_roster:feed_id(muc, FeedId, UserId)
  end.

roster_to_msg_feed([A,B,C], UserId)      -> roster_to_msg_feed({A,B,C}, UserId);
roster_to_msg_feed([A,B], UserId)        -> roster_to_msg_feed({A,B}, UserId);
roster_to_msg_feed(RosterFeedId, UserId) ->
  case RosterFeedId of
    {<<"chat">>, UserId, FeedId1} -> {?MESSAGE_FEED_TYPE_CHAT, FeedId1};
    {<<"chat">>, FeedId1, UserId} -> {?MESSAGE_FEED_TYPE_CHAT, FeedId1};
    {<<"chat">>, _, _}            -> {undefined, undefined};
    {<<"muc">>, FeedId1}          -> {?MESSAGE_FEED_TYPE_ROOM, FeedId1}
  end.

quick_update(UserId, FeedType, FeedId, Fun) ->
  UpdateId = msg_feed_to_roster(FeedType, FeedId, UserId),
  InnerFun = fun(User) ->
    Update = ensure(UpdateId, User),
    {Result, Update1} = Fun(Update, User),
    {Result, upsert(Update1#im_chat_update{timestamp=sm:now()}, User)}
  end,
  im_roster_chat:execute(UserId, InnerFun).

quick_find(FeedType, FeedId, UserId) ->
  UpdateId = msg_feed_to_roster(FeedType, FeedId, UserId),
  Fun = fun(User) ->
    Result = find(UpdateId, User),
    {Result, User}
  end,
  im_roster_chat:execute(UserId, Fun).

wait_update(UserId, FeedId)          -> wait_update(UserId, FeedId, 0).
wait_update(_,      _,      20)      -> undefined;
wait_update(UserId, FeedId, Retries) ->
  case quick_find(?MESSAGE_FEED_TYPE_ROOM, FeedId, UserId) of
    {ok, FoundUpdate} -> FoundUpdate;
    _                 -> timer:sleep(100), wait_update(UserId, FeedId, Retries + 1)
  end.

send_update(User, Update) ->
  send_update(User, Update, true).
send_update(#im_usr{id=UserId}, Update, SendMe) ->
  UpdateEntity = im_dto:format_chat_update(UserId, Update),
  % im_logger:debug(UserId, "[ChatUpdate] Send update: ~p", [UpdateEntity]),
  im_user_state:broadcast(UserId, [UserId], #'ChatUpdate'{update=UpdateEntity}, SendMe);
send_update(UserId, Update, SendMe) ->
  send_update(im_roster_chat:get(UserId), Update, SendMe).
