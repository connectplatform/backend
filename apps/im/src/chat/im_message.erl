-module(im_message).

-include("im_common.hrl").

-define(MAX_PAYLOAD_SIZE, 4096).
-define(CHAT_PREFIX, <<"chat_notif_">>).
-define(MUC_PREFIX, <<"muc_notif_">>).

-export([get_updates/4]).
-export([send/4, send/5]).
-export([send_from_sys_user/3, send_sys_msg/5, send_sys_msg/7]).
-export([edit/2, delete/4, star/3, get_starred/3]).
-export([is_visible/2, wipe_message/1]).
-export([get_top_message/4, target_user_ids/3]).
-export([create_update/2, create_update/3]).
-export([markup_user/1, parse_msg_meta/1]).

get_updates(UserId, Top, Stop, Count) ->
  ctail_feed:get(im_update, {<<"update">>, UserId}, Top, Stop, Count).

send(UserId, FeedType, FeedIdFormatted, Message) ->
  send(UserId, FeedType, FeedIdFormatted, Message, true).
send(UserId, FeedType, FeedIdFormatted, Message, SendAsUser) ->
  FeedId = im_common:parse_id(FeedIdFormatted),

  Message1 = case SendAsUser =:= true of
    true -> Message#im_msg{type=?MESSAGE_TYPE_USER_MESSAGE, inlineKeyboardMarkup=undefined};
    false -> Message
  end,

  case im_common:get_binary_length(Message#im_msg.payload) =< ?MAX_PAYLOAD_SIZE of
    true -> send_inner(UserId, FeedType, FeedId, Message1);
    false -> {error, payload_to_big}
  end.

send_from_sys_user(UserId, SystemMessageType, SystemMessageParams) ->
  send_sys_msg(?SYS_USER_ID, ?MESSAGE_FEED_TYPE_CHAT, UserId, SystemMessageType, SystemMessageParams).

send_sys_msg(UserId, FeedType, FeedId, SystemMessageType, SystemMessageParams) ->
  send_sys_msg(UserId, FeedType, FeedId, SystemMessageType, SystemMessageParams, false, undefined).
send_sys_msg(UserId, FeedType, FeedId, SystemMessageType, SystemMessageParams, RecipientOnly, Recipient) ->
  MsgItem = #im_msg{
    kind=?MESSAGE_KIND_TEXT,
    type=?MESSAGE_TYPE_SYSTEM_NOTIFICATION,
    recipientOnly=RecipientOnly,
    systemMessageType=SystemMessageType,
    systemMessageParams = SystemMessageParams,
    payload=im_trans:t(SystemMessageType, SystemMessageParams)
  },
  MsgItem1 = case RecipientOnly =:= true of
    true -> MsgItem#im_msg{origin=?SYS_USER_ID, recipient=Recipient};
    false -> MsgItem
  end,
  send_inner(UserId, FeedType, FeedId, MsgItem1).

send_inner(UserId, FeedType, FeedId, Msg) ->
  Msg1 = Msg#im_msg{id=ctail:next_id(),
    userTime=Msg#im_msg.created,
    created=sm:now(),
    updated=sm:now()},

  case FeedType of
    ?MESSAGE_FEED_TYPE_CHAT -> send_to_chat(UserId, FeedId, Msg1);
    ?MESSAGE_FEED_TYPE_ROOM -> send_to_room(UserId, FeedId, Msg1)
  end.

send_to_chat(UserId, FeedId, MsgItem) ->
  TargetUser = im_roster_chat:get(FeedId),
  case not is_blocked(UserId, TargetUser#im_usr.blocks, MsgItem#im_msg.type) of
    true ->
      {ok, MsgItem1} = im_roster_chat:message(UserId, FeedId, MsgItem),

      lists:foreach(fun(TargetUserId) ->
        new_update(UserId, TargetUserId, ?MESSAGE_FEED_TYPE_CHAT, FeedId, ?UPDATE_TYPE_MESSAGE, MsgItem1, [])
      end, target_user_ids(?MESSAGE_FEED_TYPE_CHAT, FeedId, UserId) ++ [UserId]),

      case im_bot:lookup_by_user_id(TargetUser#im_usr.id) of
        {ok, BotId} -> im_bot_update:send_message_update(BotId, TargetUser#im_usr.id, UserId, ?MESSAGE_FEED_TYPE_CHAT, UserId, MsgItem1);
        _           -> skip
      end,

      {ok, MsgItem1};
    false ->
      {error, blocked}
  end.

send_to_room(UserId, FeedId, MsgItem) ->
  Room = im_roster_muc:get(FeedId),
  case not Room#im_grp.deleted of
    true ->
      case lists:member(UserId, Room#im_grp.members) orelse UserId =:= ?SYS_USER_ID of
        true ->
          {ok, MsgItem1} = im_roster_muc:message(FeedId, UserId, MsgItem),

          lists:foreach(fun(TargetUserId) ->
            new_update(UserId, TargetUserId, ?MESSAGE_FEED_TYPE_ROOM, FeedId, ?UPDATE_TYPE_MESSAGE, MsgItem1, []),
            case TargetUserId =/= UserId of
              true ->
                case im_bot:lookup_by_user_id(TargetUserId) of
                  {ok, BotId} ->
                    IsCircle = Room#im_grp.circleParams =/= undefined,
                    DoSend = case IsCircle of
                      true -> true;
                      false ->
                        #'MessageMetaEntity7'{mentions=Mentions} = parse_msg_meta(MsgItem1),
                        BotMentions = lists:filter(fun(#'MessageMetaMentionEntity7'{userId=MentionedUserId}) ->
                          im_common:parse_id(MentionedUserId) =:= TargetUserId
                        end, Mentions),
                        length(BotMentions) > 0
                    end,
                    case DoSend of
                      true -> im_bot_update:send_message_update(BotId, TargetUserId, UserId, ?MESSAGE_FEED_TYPE_ROOM, FeedId, MsgItem1);
                      false -> skip
                    end;
                  _ -> skip
                end;
              false -> skip
            end
          end, target_user_ids(?MESSAGE_FEED_TYPE_ROOM, FeedId, UserId) ++ [UserId]),

          {ok, MsgItem1};
        false -> {error, not_a_member}
      end;
    false ->
      {error, group_deleted}
  end.

new_update(UserId, TargetUserId, FeedType, FeedId, Type, Message, Ids) ->
  RecipientOnlyRestriction = Message =/= undefined andalso
    Message#im_msg.recipientOnly =:= true andalso
    Message#im_msg.recipient =/= TargetUserId,

  case RecipientOnlyRestriction of
    true -> skip;
    false ->
      UpdateFeedId = case FeedType of
        ?MESSAGE_FEED_TYPE_ROOM -> FeedId;
        ?MESSAGE_FEED_TYPE_CHAT ->
          case TargetUserId of
            UserId -> FeedId;
            _      -> UserId
          end
      end,

      MessageId = case Message =:= undefined of
        true -> undefined;
        false -> Message#im_msg.id
      end,
      Update = #im_update{type=Type,
        feedType=FeedType,
        feedId=UpdateFeedId,
        messageId=MessageId,
        ids=Ids},
      create_update(TargetUserId, Update, Message),

      case TargetUserId =/= UserId andalso Type =:= ?UPDATE_TYPE_MESSAGE of
        true -> send_push(TargetUserId, FeedType, UpdateFeedId, Message);
        false -> skip
      end,

      case Message =:= undefined of
        true -> skip;
        false ->
          case Message#im_msg.next =:= undefined of
            true -> update_feed(UserId, TargetUserId, FeedType, UpdateFeedId, Type, Message);
            false -> skip
          end
      end
  end.

update_feed(UserId, TargetUserId, FeedType, FeedId, Type, MsgItem) ->
  Fun = fun (Update, User) ->
    FeedTypeName = case FeedType of
      ?MESSAGE_FEED_TYPE_CHAT -> chat;
      ?MESSAGE_FEED_TYPE_ROOM -> muc
    end,
    Created  = MsgItem#im_msg.created,

    Update1 = case FeedTypeName of
      chat -> Update#im_chat_update{deleted=false};
      _    -> Update
    end,

    Update2 = case {FeedTypeName, TargetUserId} of
      {chat, UserId} ->
        Update1;
      {_, _} ->
        DoIncrementUnread = (Created > im_common:ensure_timestamp(Update1#im_chat_update.seen)) and
          (UserId =/= TargetUserId) and
          (Type =/= ?UPDATE_TYPE_UPDATE_MESSAGE) and
          (Update1#im_chat_update.deleted =:= false) and
          is_visible(UserId, MsgItem),
        case DoIncrementUnread of
          true ->
            Update1#im_chat_update{timestamp=sm:now(), unread=Update1#im_chat_update.unread+1};
          false -> Update1
        end
    end,

    CurrentCreated = case ctail:get(im_msg, Update1#im_chat_update.top) of
      {ok, Top1} -> im_common:ensure_timestamp(Top1#im_msg.created);
      {error, _} -> 0
    end,

    % Update3 = update_contact_info(FeedType, Update2, UserId, TargetUserId, MsgItem),
    % Update4 = case CurrentCreated =:= 0 orelse Created > CurrentCreated of
    Update3 = Update2,
    Update4 = case CurrentCreated =:= 0 orelse Created > CurrentCreated of
      true  -> Update3#im_chat_update{timestamp=sm:now(), top=MsgItem#im_msg.id};
      false -> Update3
    end,

    {{Update4, User}, Update4}
  end,
  {Update, User} = im_chatupdate:quick_update(TargetUserId, FeedType, FeedId, Fun),
  im_chatupdate:send_update(User, Update).

create_update(UserId, Update) -> create_update(UserId, Update, undefined).
create_update(UserId, Update, Message) ->
  Update1 = Update#im_update{
    id=ctail:next_id(),
    feed_id={<<"update">>, UserId},
    messageId=im_common:parse_id(Update#im_update.messageId),
    taskId=im_common:parse_id(Update#im_update.taskId),
    ids=[im_common:parse_id(Id) || Id <- Update#im_update.ids],
    created=sm:now()},
  ctail_feed:add(Update1),

  UpdateMsg = #'Update'{update=im_dto:format_update(UserId, Update1, Message)},
  im_user_state:broadcast(UserId, [UserId], UpdateMsg).

is_blocked(_, _, ?MESSAGE_TYPE_SYSTEM_NOTIFICATION)     -> false;
is_blocked(_, undefined, _)                             -> false;
is_blocked(UserId, BlockIds,  _) when is_list(BlockIds) -> lists:member(UserId, BlockIds).

edit(UserId, #im_msg{id=Id, payload=Payload, media=Media, inlineKeyboardMarkup=InlineKeyboardMarkup, geo=Geo}) ->
  case ctail:get(im_msg, im_common:parse_id(Id)) of
    {ok, OriginalMessage} ->
      case OriginalMessage#im_msg.origin =/= UserId of
        true ->
          {error, permission_denied};
        false ->
          EditedMessage = OriginalMessage#im_msg{
            payload=Payload,
            media=Media,
            inlineKeyboardMarkup=InlineKeyboardMarkup,
            geo=Geo,
            updated=sm:now(),
            isEdited=true},
          ok = ctail:put(EditedMessage),

          {FeedType, FeedId} = im_chatupdate:roster_to_msg_feed(OriginalMessage#im_msg.feed_id, UserId),

          lists:foreach(fun(TargetUserId) ->
            new_update(UserId, TargetUserId, FeedType, FeedId, ?UPDATE_TYPE_UPDATE_MESSAGE, EditedMessage, [])
          end, target_user_ids(FeedType, FeedId, UserId) ++ [UserId]),

          {ok, EditedMessage}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

delete(UserId, FeedType, FormattedFeedId, Ids) ->
  FeedId = im_common:parse_id(FormattedFeedId),

  {NotifyMeIds, NotifyAllIds} = lists:foldl(fun(Id, {NotifyMeIds, NotifyAllIds}) ->
    case ctail:get(im_msg, im_common:parse_id(Id)) of
      {ok, MsgItem} ->
        IsMyMessage = MsgItem#im_msg.origin =:= UserId,
        case IsMyMessage of
          true ->
            ctail:put(MsgItem#im_msg{deletedByOwner=true}),
            {[MsgItem#im_msg.id|NotifyMeIds], [MsgItem#im_msg.id|NotifyAllIds]};
          false ->
            DeletedBy = case MsgItem#im_msg.deletedBy of
              undefined  -> [];
              DeletedBy1 -> DeletedBy1
            end,
            UpdatedMsg = MsgItem#im_msg{deleted=sm:now(),
              deletedBy=lists:usort([UserId|DeletedBy])},
            ctail:put(UpdatedMsg),
            {[MsgItem#im_msg.id|NotifyMeIds], NotifyAllIds}
        end;
      {error, _} ->
        {NotifyMeIds, NotifyAllIds}
    end
  end, {[],[]}, Ids),

  lists:foreach(fun(TargetUserId) ->
    UpdateFeedId = case FeedType of
      ?MESSAGE_FEED_TYPE_ROOM -> FeedId;
      ?MESSAGE_FEED_TYPE_CHAT ->
        case TargetUserId of
          UserId -> FeedId;
          _      -> UserId
        end
    end,
    MessageIds = case TargetUserId of
      UserId -> NotifyMeIds;
      _      -> NotifyAllIds
    end,
    case MessageIds =/= [] of
      true -> new_update(UserId, TargetUserId, FeedType, UpdateFeedId, ?UPDATE_TYPE_DELETE_MESSAGES, undefined, MessageIds);
      false -> skip
    end,
    im_pointer:seen(FeedType, UpdateFeedId, TargetUserId, -1) %% update unread count
  end, target_user_ids(FeedType, FeedId, UserId) ++ [UserId]),

  {ok, Ids}.

star(UserId, MessageIds, Star) ->
  MessageIds1 = [im_common:parse_id(MessageId) || MessageId <- MessageIds],
  Starred = case ctail:get(im_msg_star, UserId) of
    {ok, Record} -> Record;
    {error, _} -> #im_msg_star{id=UserId, messageIds=[]}
  end,
  NewStarred = case Star =:= true of
    true ->
      NewMessageIds = Starred#im_msg_star.messageIds ++ MessageIds1,
      Starred#im_msg_star{messageIds=im_common:list_unique(NewMessageIds)};
    false ->
      Starred#im_msg_star{messageIds=Starred#im_msg_star.messageIds -- MessageIds1}
  end,
  ctail:put(NewStarred).

get_starred(UserId, Skip, Limit) ->
  Skip1 = case Skip =:= undefined orelse Skip < 0 of true -> 0; false -> Skip end,
  Limit1 = case Limit =:= undefined orelse Limit =:= 0 of true -> 50; _ -> Limit end,

  case ctail:get(im_msg_star, UserId) of
    {ok, Star} ->
      case length(Star#im_msg_star.messageIds) > 0 of
        true ->
          Limit2 = case Limit1 > length(Star#im_msg_star.messageIds) of
            true -> length(Star#im_msg_star.messageIds);
            false -> Limit1
          end,
          MessageIds = lists:sublist(lists:reverse(Star#im_msg_star.messageIds), Skip1 + 1, Limit2),
          Messages = lists:foldl(fun(MessageId, Acc) ->
            case ctail:get(im_msg, MessageId) of
              {ok, Message} -> [Message|Acc];
              {error, _} -> Acc
            end
          end, [], MessageIds),
          {Messages, length(Star#im_msg_star.messageIds)};
        false ->
          {[], 0}
      end;
    {error, _} ->
      {[], 0}
  end.

is_visible(UserId, Message) ->
  DeletedBy = case Message#im_msg.deletedBy =:= undefined of
    true -> [];
    false -> Message#im_msg.deletedBy
  end,
  IsDeleted = lists:member(UserId, DeletedBy) orelse Message#im_msg.deletedByOwner =:= true,
  IAmRecipient = (Message#im_msg.recipientOnly =/= true orelse Message#im_msg.recipient =:= im_common:parse_id(UserId)),

  IsDeleted =/= true andalso IAmRecipient.

markup_user(UserId) ->
  UId1 = im_common:format_id(UserId),
  <<"<user:", UId1/binary, ">">>.

parse_msg_meta(#im_msg{payload=undefined}) -> #'MessageMetaEntity7'{mentions=[]};
parse_msg_meta(#im_msg{payload=Payload}) -> parse_msg_meta(Payload);
parse_msg_meta(Payload) ->
  Mentions = case re:run(Payload, "<user:[^>]+>", [global]) of
    nomatch -> [];
    {match, Matches} ->
      lists:map(fun([{Pos, _}]) ->
        UserId = erlang:binary_part(Payload, Pos + 6, 24),
        #'MessageMetaMentionEntity7'{position=Pos, userId=UserId}
      end, Matches)
  end,
  #'MessageMetaEntity7'{mentions=Mentions}.

% update_contact_info(muc, Update=#im_chat_update{}, _UserId, _UpdateUserId, _MsgItem) -> Update;
% update_contact_info(_, Update = #im_chat_update{name=Name}, UserId, UpdateUserId, MsgItem) ->
%   case Name of
%     undefined ->
%       {ContactUserId, UId} = case UserId =:= UpdateUserId of
%         true  -> {MsgItem#im_msg.recipient, UserId};
%         false -> {MsgItem#im_msg.origin, UpdateUserId}
%       end,
%       Contact1 = case Update#im_chat_update.id of
%         {_, UId, UId2} -> im_contact:find(UId, UId2);
%         {_, UId1, UId} -> im_contact:find(UId, UId1);
%         {_, _}         -> skip
%       end,
%       case Contact1 of
%         skip -> Update;
%         {ok, Contact} ->
%           Update#im_chat_update{name=Contact#im_contact.name, thumbnail=Contact#im_contact.thumbnail};
%         _ ->
%           {UserName, UserThumbnail} = get_user_info(ContactUserId),
%           Update#im_chat_update{name=UserName, thumbnail=UserThumbnail}
%       end;
%     _ -> Update
%   end.

send_push(_, _, _, undefined) -> skip;
send_push(TargetUserId, FeedType, FeedId, Message) ->
  DoSend = case Message#im_msg.kind =:= ?MESSAGE_KIND_CALL of
    true -> im_call_sup:is_missed(Message#im_msg.callId, TargetUserId);
    false -> true
  end,

  case DoSend of
    true ->
      Prefix = case FeedType of
        ?MESSAGE_FEED_TYPE_CHAT -> ?CHAT_PREFIX;
        ?MESSAGE_FEED_TYPE_ROOM -> ?MUC_PREFIX
      end,
      TargetUserIdBin = list_to_binary(sm:bin_to_hex(element(1, TargetUserId))),
      SettingKey = <<Prefix/binary, TargetUserIdBin/binary>>,

      NotificationsEnabled = im_user_settings:get_one(FeedId, SettingKey, <<"true">>) =/= <<"false">>,
      case NotificationsEnabled of
        true ->
          im_push_ios:send_message(TargetUserId, Message),
          im_push_android:send_message(TargetUserId, Message),
          im_push_web:send_message(TargetUserId, Message);
        false -> skip
      end;
    false -> skip
  end.

get_user_info(UId) ->
  case im_roster_chat:get(UId) of
    undefined -> {undefined, undefined};
    User -> {User#im_usr.name, User#im_usr.thumbnail}
  end.

wipe_message(MessageIdRaw) ->
  MessageId = im_common:parse_id(MessageIdRaw),
  case ctail:get(im_msg, MessageId) of
    {ok, M} ->
      M1 = M#im_msg{
        type=?MESSAGE_TYPE_SYSTEM_NOTIFICATION, kind=?MESSAGE_KIND_TEXT, media=[], geo=[],
        systemMessageType= <<"system.message.message.has.been.deleted">>,
        systemMessageParams=[],
        payload=im_common:format_utf8(im_trans:t(<<"system.message.message.has.been.deleted">>))
      },
      ctail:put(M1);
    _ -> skip
  end,
  ok.

get_top_message(MsgId, Update=#im_chat_update{}, User=#im_usr{id=UserId}, ExcludeMine) ->
  MsgId1 = im_common:parse_id(MsgId),

  case ctail:get(im_msg, MsgId1) of
    {ok, Msg} ->
      DeletedBy = Msg#im_msg.deletedBy,
      case lists:member(Msg#im_msg.origin, DeletedBy) orelse
        lists:member(im_common:parse_id(UserId), DeletedBy) orelse
        Msg#im_msg.deletedByOwner =:= true
      of
        true ->
          get_top_message(Msg#im_msg.prev, Update, User, ExcludeMine);
        false ->
          case Msg#im_msg.recipientOnly of
            true ->
              case Msg#im_msg.recipient of
                UserId ->
                  case ExcludeMine =:= true andalso Msg#im_msg.origin =:= UserId of
                    true -> get_top_message(Msg#im_msg.prev, Update, User, ExcludeMine);
                    false -> Msg
                  end;
                _ ->
                  get_top_message(Msg#im_msg.prev, Update, User, ExcludeMine)
              end;
            _ -> Msg
          end
      end;
    _ -> undefined
  end.

target_user_ids(FeedType, FeedId, UserId) ->
  case FeedType of
    ?MESSAGE_FEED_TYPE_CHAT -> [im_common:parse_id(FeedId)];
    ?MESSAGE_FEED_TYPE_ROOM ->
      Room = im_roster_muc:get(im_common:parse_id(FeedId)),
      Room#im_grp.members -- [UserId]
  end.
