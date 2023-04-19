-module(im_dto).

-include("im_common.hrl").

-export([format_user/1, parse_user/1, format_user_role/1, user_to_json/1]).
-export([format_room/1, parse_room/1]).
-export([format_message/2, parse_message/1]).
-export([format_update/2, format_update/3]).
-export([format_chat_update/2]).
-export([format_media/1, parse_media/1]).
-export([format_geo/1, parse_geo/1]).
-export([format_csr/1, parse_csr/1]).
-export([format_call/1]).
-export([format_task/2, parse_task/1]).
-export([format_bot/1]).
-export([format_inline_keyboard_markup/1, parse_inline_keyboard_markup/1]).
-export([format_reply_keyboard_markup/1, parse_reply_keyboard_markup/1]).
-export([format_remove_keyboard_markup/1, parse_remove_keyboard_markup/1]).
-export([format_setting/1, format_settings/1, parse_setting/1]).
-export([format_feed_post_category/1, parse_feed_post_category/1]).
-export([format_feed_post/1, parse_feed_post/1]).
-export([format_order/1]).
-export([format_localized_store/1, parse_localized_store/1]).
-export([get_localized_value/2]).
-export([format_page/1]).
-export([format_channel/1, parse_channel/1]).
-export([format_location/1, parse_location/1]).
-export([format_channel_category/1, parse_channel_category/1]).
-export([format_ogdata/1]).
-export([format_device/1]).

format_user(#im_usr{id=Id, name=Name, username=Username, photo=Photo, thumbnail=Thumbnail, phone=Phone, departmentId=DepartmentId,
  facebookId=FacebookId, data=Data, roles=Roles, isBot=IsBot, isVendor=IsVendor, vendorId=VendorId, excludeMe=ExcludeMe,
  active=Active, bio=Bio, sendBioToNewContacts=SendBioToNewContacts}) ->
  #'UserEntity'{
    id           = im_common:format_id(Id),
    name         = im_common:format_utf8(Name),
    username     = im_common:format_utf8(Username),
    photo        = im_common:format_utf8(Photo),
    thumbnail    = im_common:format_utf8(Thumbnail),
    phone        = im_common:format_utf8(Phone),
    departmentId = im_common:format_id(DepartmentId),
    facebookId   = im_common:format_id(FacebookId),
    data         = im_common:format_utf8(im_common:mongo_to_json(Data)),
    roles        = format_roles(Roles),
    isBot        = IsBot,
    isVendor     = im_common:ensure_boolean(IsVendor),
    vendorId     = im_common:format_id(VendorId),
    excludeMe    = im_common:ensure_boolean(ExcludeMe),
    bio          = im_common:format_utf8(Bio),
    active       = Active,
    sendBioToNewContacts = SendBioToNewContacts
  }.

parse_user(#'UserEntity'{name=Name, photo=Photo, thumbnail=Thumbnail, phone=Phone, excludeMe=ExcludeMe, bio=Bio, sendBioToNewContacts=SendBioToNewContacts}) ->
  #im_usr{
    name=im_common:format_utf8(Name),
    photo=im_common:format_utf8(Photo),
    thumbnail=im_common:format_utf8(Thumbnail),
    phone=im_common:format_utf8(Phone),
    excludeMe=im_common:ensure_boolean(ExcludeMe),
    bio=im_common:format_utf8(Bio),
    sendBioToNewContacts=SendBioToNewContacts
  }.

format_roles(undefined) -> [];
format_roles(Roles) ->
  [im_common:format_utf8(Role) || Role <- Roles].

format_user_role(#im_usr_role{id=Id, name=Name, permissions=Permissions}) ->
  Perms = lists:foldl(fun(Perm, Acc) ->
    case ctail:get(im_usr_perm, Perm) of
      {ok, Perm1} -> [format_user_role_perm(Perm1)|Acc];
      {error, _}  -> Acc
    end
  end, [], Permissions),
  #'UserRoleEntity'{id=im_common:format_utf8(Id), name=im_common:format_utf8(Name), perms=Perms}.

format_user_role_perm(#im_usr_perm{id=Id, name=Name}) ->
  #'UserRolePermEntity'{id=im_common:format_utf8(Id), name=im_common:format_utf8(Name)}.

format_room(undefined) -> undefined;
format_room(#im_grp{id=Id, created=Created, updated=Updated, members=Members, admins=Admins, topic=Topic, picture=Picture, thumbnail=Thumbnail, deleted=Deleted, circleParams=CircleParams}) ->
  Circle = case CircleParams =:= undefined orelse CircleParams =:= [] orelse CircleParams =:= <<>> of
    true -> undefined;
    false ->
      CircleParamsList = cow_qs:parse_qs(CircleParams),
      #'CircleEntity'{supervisor=proplists:get_value(<<"supervisor">>, CircleParamsList),
        params=cow_qs:qs(proplists:delete(<<"supervisor">>, CircleParamsList))}
  end,
  #'RoomEntity'{
    id=im_common:format_id(Id),
    created=Created,
    updated=Updated,
    members=[im_common:format_id(Member) || Member <- Members],
    admins=[im_common:format_id(Admin) || Admin <- Admins],
    picture=im_common:format_utf8(Picture),
    thumbnail=im_common:format_utf8(Thumbnail),
    topic=im_common:format_utf8(Topic),
    deleted=Deleted,
    circle=Circle
  }.

parse_room(#'RoomEntity'{id=Id, created=Created, updated=Updated, members=Members, admins=Admins, topic=Topic, picture=Picture, thumbnail=Thumbnail, deleted=Deleted}) ->
  #im_grp{
    id=im_common:parse_id(Id),
    created=im_common:ensure_timestamp(Created),
    updated=im_common:ensure_timestamp(Updated),
    members=lists:map(fun (X) -> im_common:parse_id(X) end, Members),
    admins=lists:map(fun (X) -> im_common:parse_id(X) end, Admins),
    picture=im_common:format_utf8(Picture),
    thumbnail=im_common:format_utf8(Thumbnail),
    topic=im_common:format_utf8(Topic),
    deleted=Deleted
  }.

format_message(UserId, Msg) -> format_message(UserId, Msg, 0).
format_message(UserId, #im_msg{id=Id, type=Type, created=Created, updated=Updated, recipient=Recipient, origin=Origin, isEdited=IsEdited,
  payload=Payload, media=Media, starred=Starred, replyId=ReplyId, forwardId=ForwardId, geo=Geo, kind=Kind, callId=CallId,
  systemMessageType=SystemMessageType, systemMessageParams=SystemMessageParams, inlineKeyboardMarkup=InlineKeyboardMarkup, userTime=UserTime}, Level) ->

  FormatInnerMessageEntityById = fun(MsgId) ->
    case MsgId =/= undefined andalso Level =:= 0 of
      true ->
        case ctail:get(im_msg, im_common:parse_id(MsgId)) of
          {ok, Msg} ->
            case im_message:is_visible(UserId, Msg) =:= true of
              true -> format_message(UserId, Msg, Level + 1);
              false -> undefined
            end;
          {error, not_found} -> undefined
        end;
      false -> undefined
    end
  end,

  Call = case im_call_sup:find_user_call(UserId, CallId) of
    {ok, C = #im_call{}} -> C;
    _ -> undefined
  end,

  #'MessageEntity'{id=im_common:format_id(Id),
    type=Type,
    created=im_common:ensure_timestamp(Created),
    updated=im_common:ensure_timestamp(Updated),
    recipient=im_common:format_id(Recipient),
    origin=im_common:format_id(Origin),
    reply=FormatInnerMessageEntityById(ReplyId),
    forward=FormatInnerMessageEntityById(ForwardId),
    systemMessageType=SystemMessageType,
    systemMessageParams=SystemMessageParams,
    payload=im_common:format_utf8(Payload),
    media=format_media_list(Media),
    inlineKeyboardMarkup=format_inline_keyboard_markup(InlineKeyboardMarkup),
    starred=im_common:ensure_boolean(Starred),
    geo=format_geo(Geo),
    kind=Kind,
    call=format_call(Call),
    isEdited=im_common:ensure_boolean(IsEdited),
    userTime=im_common:ensure_timestamp(UserTime)};
format_message(_, _, _) -> undefined.

parse_message(undefined) -> undefined;
parse_message(#'MessageEntity'{id=Id, created=Created, payload=Payload, media=Media, reply=Reply, forward=Forward, geo=Geo, kind=Kind, inlineKeyboardMarkup=InlineKeyboardMarkup}) ->
  Kind1 = case Kind =:= 0 of
    true -> ?MESSAGE_KIND_TEXT;
    false -> Kind
  end,

  ReplyId = case Reply of
    #'MessageEntity'{id=ReplyId1} -> ReplyId1;
    _                             -> undefined
  end,
  ForwardId = case Forward of
    #'MessageEntity'{id=ForwardId1} -> ForwardId1;
    _                               -> undefined
  end,

  #im_msg{id=im_common:parse_id(Id),
    type=?MESSAGE_TYPE_USER_MESSAGE,
    kind=Kind1,
    created=Created,
    replyId=im_common:parse_id(ReplyId),
    forwardId=im_common:parse_id(ForwardId),
    payload=im_common:format_utf8(Payload),
    media=[parse_media(M) || M <- Media],
    inlineKeyboardMarkup=parse_inline_keyboard_markup(InlineKeyboardMarkup),
    geo=parse_geo(Geo)}.

format_update(UserId, Update) ->
  format_update(UserId, Update, undefined).
format_update(UserId, #im_update{id=Id, type=Type, created=Created, feedType=FeedType, feedId=FeedId, messageId=MessageId, taskId=TaskId, ids=Ids}, Msg) ->
  Message = case Msg =/= undefined of
    true -> Msg;
    false ->
      case MessageId =:= undefined of
        true -> undefined;
        false ->
          case ctail:get(im_msg, MessageId) of
            {ok, Message1} -> Message1;
            {error, _}     -> undefined
          end
      end
  end,
  Task = case TaskId =/= undefined of
    true ->
      case ctail:get(im_task, TaskId) of
        {ok, Task1} -> Task1;
        {error, _}  -> undefined
      end;
    false -> undefined
  end,
  #'UpdateEntity'{id=im_common:format_id(Id),
    type=Type,
    created=Created,
    feedType=FeedType,
    feedId=im_common:format_id(FeedId),
    message=format_message(UserId, Message),
    task=format_task(Task, UserId),
    ids=[im_common:format_id(MsgId) || MsgId <- Ids]}.

format_chat_update(_, undefined) -> undefined;
format_chat_update(UserId, ChatUpdate=#im_chat_update{id=UpdateId, top=TopId, delivered=Delivered, seen=Seen, unread=Unread, hide=Hide, name=Name, thumbnail=Thumbnail, deleted=Deleted, replyKeyboardMarkup=ReplyKeyboardMarkup, removeKeyboardMarkup=RemoveKeyboardMarkup, callId=CallId}) ->
  {FeedType, FeedId1} = im_chatupdate:roster_to_msg_feed(UpdateId, UserId),
  TopEntity = case TopId =:= undefined of
    true -> undefined;
    false ->
      Message = im_message:get_top_message(TopId, ChatUpdate, im_roster_chat:get(UserId), false),
      format_message(UserId, Message)
  end,
  Room1 = case FeedType of
    ?MESSAGE_FEED_TYPE_ROOM -> try im_roster_muc:get(FeedId1) catch _:_ -> undefined end;
    _                       -> undefined
  end,
  #'ChatUpdateEntity'{feedType=FeedType,
    feedId=im_common:format_id(FeedId1),
    top=TopEntity,
    room=format_room(Room1),
    delivered=im_common:ensure_timestamp(Delivered),
    seen=im_common:ensure_timestamp(Seen),
    unread=Unread,
    hide=Hide,
    name=im_common:format_utf8(Name),
    thumbnail=im_common:format_utf8(Thumbnail),
    deleted=Deleted,
    replyKeyboardMarkup=format_reply_keyboard_markup(ReplyKeyboardMarkup),
    removeKeyboardMarkup=format_remove_keyboard_markup(RemoveKeyboardMarkup),
    callId=im_common:format_id(CallId)}.

format_media_list(undefined) -> undefined;
format_media_list(List)      -> [format_media(M) || M <- List].

format_media({<<"media">>, MediaProps}) ->
  Type = proplists:get_value(<<"type">>, MediaProps),
  Link = proplists:get_value(<<"link">>, MediaProps),

  OgData = case Type =:= ?MEDIA_TYPE_LINK of
    true ->
      case im_og:fetch_url(Link, true, false, undefined) of
        {ok, OgData1} -> OgData1;
        {error, _} -> undefined
      end;
    false -> undefined
  end,

  #'MediaEntity'{
    type=Type,
    link=Link,
    caption=proplists:get_value(<<"caption">>, MediaProps),
    thumbnail=proplists:get_value(<<"thumbnail">>, MediaProps),
    width=im_common:format_integer(proplists:get_value(<<"width">>, MediaProps)),
    height=im_common:format_integer(proplists:get_value(<<"height">>, MediaProps)),
    duration=im_common:format_integer(proplists:get_value(<<"duration">>, MediaProps)),
    name=proplists:get_value(<<"name">>, MediaProps),
    size=im_common:format_integer(proplists:get_value(<<"size">>, MediaProps)),
    ogData=format_ogdata(OgData),
    callId=im_common:format_id(proplists:get_value(<<"callId">>, MediaProps)),
    taskId=im_common:format_id(proplists:get_value(<<"taskId">>, MediaProps)),
    contactId=im_common:format_id(proplists:get_value(<<"contactId">>, MediaProps)),
    messageId=im_common:format_id(proplists:get_value(<<"messageId">>, MediaProps)),
    geo=im_common:format_id(proplists:get_value(<<"geo">>, MediaProps))
  }.

parse_media(#'MediaEntity'{type=Type, link=Link, thumbnail=Thumbnail, width=Width, height=Height, size=Size, duration=Duration, name=Name, caption=Caption, callId=CallId, taskId=TaskId, contactId=ContactId, messageId=MessageId}) ->
  Thumbnail1 = case Thumbnail =:= undefined of
    true -> Link;
    false -> Thumbnail
  end,
  {<<"media">>, [
    {<<"type">>, Type},
    {<<"link">>, Link},
    {<<"caption">>, Caption},
    {<<"thumbnail">>, Thumbnail1},
    {<<"width">>, Width},
    {<<"height">>, Height},
    {<<"duration">>, Duration},
    {<<"name">>, Name},
    {<<"size">>, Size},
    {<<"callId">>, im_common:parse_id(CallId)},
    {<<"taskId">>, im_common:parse_id(TaskId)},
    {<<"contactId">>, im_common:parse_id(ContactId)},
    {<<"messageId">>, im_common:parse_id(MessageId)}
  ]}.

format_geo(undefined)                                               -> [];
format_geo([])                                                      -> [];
format_geo({<<"type">>, <<"Point">>, <<"coordinates">>, [0,0]})     -> [];
format_geo({<<"type">>, <<"Point">>, <<"coordinates">>, [Lng,Lat]}) -> [float(Lat), float(Lng)];
format_geo({<<"coordinates">>, [Lng,Lat], <<"type">>, <<"Point">>}) -> [float(Lat), float(Lng)].

parse_geo(undefined)  -> {<<"type">>, <<"Point">>, <<"coordinates">>, [0, 0]};
parse_geo([])         -> {<<"type">>, <<"Point">>, <<"coordinates">>, [0, 0]};
parse_geo([Lat, Lng]) -> {<<"type">>, <<"Point">>, <<"coordinates">>, [Lng, Lat]}.

format_csr(#im_csr{id=Id, status=Status, userId=UserId, vendorId=VendorId, roomId=RoomId, descr=Descr, created=Created, updated=Updated, activity=Activity, staff=Staff, tags=Tags}) ->
  #'CsrEntity'{id=im_common:format_id(Id),
    status=Status,
    userId=im_common:format_id(UserId),
    vendorId=im_common:format_id(VendorId),
    roomId=im_common:format_id(RoomId),
    descr=im_common:format_utf8(Descr),
    created=im_common:ensure_timestamp(Created),
    updated=im_common:ensure_timestamp(Updated),
    activity=format_csr_activity(Activity, []),
    staff=[im_common:format_id(UId) || UId <- Staff],
    tags=im_common:format_utf8_array(Tags)}.

format_csr_activity(undefined, Acc) -> Acc;
format_csr_activity([], Acc) -> Acc;
format_csr_activity([{<<"activity">>, ActivityProps}|T], Acc) ->
  NewItem = #'CsrActivityEntity'{userId=im_common:format_id(proplists:get_value(<<"userId">>, ActivityProps)),
    action=proplists:get_value(<<"action">>, ActivityProps),
    date=proplists:get_value(<<"date">>, ActivityProps)},
  format_csr_activity(T, Acc ++ [NewItem]).

parse_csr(#'CsrEntity'{userId=UserId, vendorId=VendorId, roomId=RoomId, descr=Descr, tags=Tags}) ->
  #im_csr{id=ctail:next_id(),
    userId=im_common:parse_id(UserId),
    vendorId=im_common:parse_id(VendorId),
    roomId=im_common:parse_id(RoomId),
    descr=im_common:format_utf8(Descr),
    tags=im_common:format_utf8_array(Tags)}.

format_call(#im_call{callId=CallId, feedType=FeedType, feedId=FeedId, initiatorId=InitiatorId, created=Created,
  duration=Duration, direction=Direction, status=Status}) ->
  #'CallEntity'{
    id=im_common:format_id(CallId),
    feedType=FeedType,
    feedId=im_common:format_id(FeedId),
    initiatorId=im_common:format_id(InitiatorId),
    created=Created,
    duration=erlang:trunc(Duration),
    direction=Direction,
    status=Status
  };
format_call(_) -> undefined.

format_task(undefined, _UserId) -> undefined;
format_task(#im_task{id=Id, created=Created, payload=Payload, media=Media, reporter=Reporter, deadline=Deadline,
  assignee=Assignee, status=Status, sourceMessageIds=SourceMessageIds, feedId=FeedId}, UserId) ->
  SourceMessageIds1 = case SourceMessageIds of
    undefined -> [];
    _         -> [im_common:format_id(SourceId) || SourceId <- SourceMessageIds]
  end,
  {FeedType, FeedId1} = case FeedId of
    undefined -> {undefined, undefined};
    _         -> im_chatupdate:roster_to_msg_feed(list_to_tuple(FeedId), UserId)
  end,
  #'TaskEntity'{id=im_common:format_id(Id),
    feedType=FeedType,
    feedId=im_common:format_id(FeedId1),
    created=im_common:ensure_timestamp(Created),
    payload=im_common:format_utf8(Payload),
    media=format_media_list(Media),
    reporter=im_common:format_id(Reporter),
    assignee=im_common:format_id(Assignee),
    status=im_common:format_utf8(Status),
    sourceMessageIds=SourceMessageIds1,
    deadline=im_common:ensure_timestamp(Deadline)}.

parse_task(#'TaskEntity'{created=Created, payload=Payload, media=Media, status=Status, assignee=Assignee,
  sourceMessageIds=SourceMessageIds, deadline=Deadline}) ->
  Created1 = case Created =:= undefined of
    true -> sm:now();
    false -> im_common:ensure_timestamp(Created)
  end,
  SourceMessageIds1 = case SourceMessageIds =:= undefined of
    true -> [];
    false -> [im_common:parse_id(SourceId) || SourceId <- SourceMessageIds]
  end,
  #im_task{created=Created1,
    payload=im_common:format_utf8(Payload),
    assignee=im_common:parse_id(Assignee),
    status=im_common:format_utf8(Status),
    media=[parse_media(M) || M <- Media],
    sourceMessageIds=SourceMessageIds1,
    deadline=im_common:ensure_timestamp(Deadline)}.

format_bot(#im_bot{id=Id, type=Type, userId=UserId, username=Username, descr=Descr, commands=Commands, isInline=IsInline}) ->
  {MainUrl, ChatUrl} = case Type == ?BOT_TYPE_WIZARD of
    true ->
      BaseUrl = im_common:base_url() ++ "/bot/" ++ im_common:ensure_list(Username) ++ "/webview",
      {BaseUrl ++ "/main", BaseUrl ++ "/chat"};
    false ->
      {undefined, undefined}
  end,
  #'BotEntity'{
    id=im_common:format_id(Id),
    type=Type,
    userId=im_common:format_id(UserId),
    username=im_common:format_utf8(Username),
    descr=im_common:format_utf8(Descr),
    commands=format_bot_commands(Commands),
    isInline=IsInline,
    webviewUrlMain=MainUrl,
    webviewUrlChat=ChatUrl
  }.

format_bot_commands(undefined) -> undefined;
format_bot_commands([]) -> [];
format_bot_commands({<<"commands">>, Props}) ->
  [format_bot_command_set(Set) || Set <- Props].

format_bot_command_set({<<"commandSet">>, Props}) ->
  #'BotCommandSetEntity'{
    userRole=im_common:format_utf8(proplists:get_value(<<"userRole">>, Props)),
    commands=[format_bot_command(Command) || Command <- proplists:get_value(<<"commands">>, Props)]
  }.

format_bot_command({<<"command">>, Props}) ->
  #'BotCommandEntity'{
    name=im_common:format_utf8(proplists:get_value(<<"name">>, Props)),
    descr=im_common:format_utf8(proplists:get_value(<<"descr">>, Props))
  }.

format_inline_keyboard_markup(undefined) -> undefined;
format_inline_keyboard_markup({<<"inlineKeyboardMarkup">>, Props}) ->
  #'InlineKeyboardMarkupEntity'{
    buttonRows=[format_inline_keyboard_row(Row) || Row <- proplists:get_value(<<"buttonRows">>, Props)]
  }.

format_inline_keyboard_row({<<"row">>, Props}) ->
  #'InlineKeyboardButtonRowEntity'{
    buttons=[format_inline_keyboard_button(Button) || Button <- proplists:get_value(<<"buttons">>, Props)]
  }.

format_inline_keyboard_button({<<"button">>, Props}) ->
  #'InlineKeyboardButtonEntity'{
    text=im_common:format_utf8(proplists:get_value(<<"text">>, Props)),
    url=im_common:format_utf8(proplists:get_value(<<"url">>, Props)),
    callbackData=im_common:format_utf8(proplists:get_value(<<"callbackData">>, Props)),
    switchInlineQueryCurrentChat=im_common:format_utf8(proplists:get_value(<<"switchInlineQueryCurrentChat">>, Props))
  }.

parse_inline_keyboard_markup(undefined) -> undefined;
parse_inline_keyboard_markup(#'InlineKeyboardMarkupEntity'{buttonRows=ButtonRows}) ->
  {<<"inlineKeyboardMarkup">>, [
    {<<"buttonRows">>, [parse_inline_keyboard_row(Row) || Row <- ButtonRows]}
  ]}.

parse_inline_keyboard_row(#'InlineKeyboardButtonRowEntity'{buttons=Buttons}) ->
  {<<"row">>, [
    {<<"buttons">>, [parse_inline_keyboard_button(Button) || Button <- Buttons]}
  ]}.

parse_inline_keyboard_button(#'InlineKeyboardButtonEntity'{text=Text, url=Url, callbackData=CallbackData,
  switchInlineQueryCurrentChat=SwitchInlineQueryCurrentChat}) ->
  {<<"button">>, [
    {<<"text">>, im_common:format_utf8(Text)},
    {<<"url">>, im_common:format_utf8(Url)},
    {<<"callbackData">>, im_common:format_utf8(CallbackData)},
    {<<"switchInlineQueryCurrentChat">>, im_common:format_utf8(SwitchInlineQueryCurrentChat)}
  ]}.

format_reply_keyboard_markup(undefined) -> undefined;
format_reply_keyboard_markup({<<"replyKeyboardMarkup">>, Props}) ->
  #'ReplyKeyboardMarkupEntity'{
    buttonRows=[format_reply_keyboard_row(Row) || Row <- proplists:get_value(<<"buttonRows">>, Props)],
    resizeKeyboard=proplists:get_value(<<"resizeKeyboard">>, Props),
    oneTimeKeyboard=im_common:ensure_boolean(proplists:get_value(<<"oneTimeKeyboard">>, Props)),
    selective=im_common:ensure_boolean(proplists:get_value(<<"selective">>, Props))
  }.

format_reply_keyboard_row({<<"row">>, Props}) ->
  #'KeyboardButtonRowEntity'{
    buttons=[format_reply_keyboard_button(Button) || Button <- proplists:get_value(<<"buttons">>, Props)]
  }.

format_reply_keyboard_button({<<"button">>, Props}) ->
  #'KeyboardButtonEntity'{
    text=im_common:format_utf8(proplists:get_value(<<"text">>, Props)),
    payload=proplists:get_value(<<"payload">>, Props),
    requestContact=proplists:get_value(<<"requestContact">>, Props),
    requestLocation=proplists:get_value(<<"requestLocation">>, Props)
  }.

parse_reply_keyboard_markup(undefined) -> undefined;
parse_reply_keyboard_markup(#'ReplyKeyboardMarkupEntity'{buttonRows=ButtonRows, resizeKeyboard=ResizeKeyboard, oneTimeKeyboard=OneTimeKeyboard, selective=Selective}) ->
  {<<"replyKeyboardMarkup">>, [
    {<<"buttonRows">>, [parse_reply_keyboard_row(Row) || Row <- ButtonRows]},
    {<<"resizeKeyboard">>, ResizeKeyboard},
    {<<"oneTimeKeyboard">>, OneTimeKeyboard},
    {<<"selective">>, Selective}
  ]}.

parse_reply_keyboard_row(#'KeyboardButtonRowEntity'{buttons=Buttons}) ->
  {<<"row">>, [
    {<<"buttons">>, [parse_reply_keyboard_button(Button) || Button <- Buttons]}
  ]}.

parse_reply_keyboard_button(#'KeyboardButtonEntity'{text=Text, payload=Payload, requestContact=RequestContact, requestLocation=RequestLocation}) ->
  {<<"button">>, [
    {<<"text">>, im_common:format_utf8(Text)},
    {<<"payload">>, im_common:format_utf8(Payload)},
    {<<"requestContact">>, RequestContact},
    {<<"requestLocation">>, RequestLocation}
  ]}.

format_remove_keyboard_markup(undefined) -> undefined;
format_remove_keyboard_markup({<<"removeKeyboardMarkup">>, Props}) ->
  #'RemoveKeyboardMarkupEntity'{
    selective=proplists:get_value(<<"selective">>, Props)
  }.

parse_remove_keyboard_markup(undefined) -> undefined;
parse_remove_keyboard_markup(#'RemoveKeyboardMarkupEntity'{selective=Selective}) ->
  {<<"removeKeyboardMarkup">>, [
    {<<"selective">>, Selective}
  ]}.

format_setting({K,V}) ->
  #'UserSettingEntity'{key=im_common:ensure_binary(K), value=im_common:ensure_binary(V)}.

format_settings(Setting=#im_setting{}) ->
  lists:map(fun format_setting/1, Setting#im_setting.settings).

parse_setting(#'UserSettingEntity'{key=K,value=V})
  when is_list(K) andalso is_list(V) ->
  {list_to_binary(K), list_to_binary(V)};
parse_setting(#'UserSettingEntity'{key=K,value=V}) -> {K, V}.

format_feed_post_category(#im_feed_post_category{id=Id, tag=Tag, name=Name, parentId=ParentId, thumbnail=Thumbnail, created=Created, isFeatured=IsFeatured}) ->
  #'FeedPostCategoryEntity'{id=im_common:format_id(Id),
    name=im_common:format_utf8(Name),
    tag=im_common:format_utf8(Tag),
    parentId=im_common:format_id(ParentId),
    thumbnail=im_common:format_utf8(Thumbnail),
    created=im_common:ensure_timestamp(Created),
    isFeatured=im_common:ensure_boolean(IsFeatured)}.

parse_feed_post_category(#'FeedPostCategoryEntity'{id=Id, tag=Tag, name=Name, parentId=ParentId, thumbnail=Thumbnail, isFeatured=IsFeatured}) ->
  #im_feed_post_category{id=im_common:parse_id(Id),
    tag=im_common:format_utf8(Tag),
    name=im_common:format_utf8(Name),
    parentId=im_common:parse_id(ParentId),
    thumbnail=im_common:format_utf8(Thumbnail),
    isFeatured=IsFeatured}.

format_feed_post(#im_feed_post{id=Id, type=Type, created=Created, title=Title, payload=Payload, media=Media, categories=Categories,
  author=Author, thumbnail=Thumbnail, buttonCaption=ButtonCaption, targetLink=TargetLink, location=Location, tags=Tags, date=Date, vendorId=VendorId,
  parentId=ParentId, workHours=WorkHours, userId=UserId, address=Address, descr=Descr, phone=Phone, paymentInfo=PaymentInfo}) ->
  #'FeedPostEntity'{
    id=im_common:format_id(Id),
    type=im_common:format_utf8(Type),
    categories=format_feed_post_categories(Categories),
    tags=format_feed_post_tags(Tags),
    parentId=im_common:format_id(ParentId),
    created=im_common:ensure_timestamp(Created),
    title=im_common:format_utf8(Title),
    descr=im_common:format_utf8(Descr),
    payload=im_common:format_utf8(Payload),
    author=im_common:format_utf8(Author),
    thumbnail=im_common:format_utf8(Thumbnail),
    buttonCaption=im_common:format_utf8(ButtonCaption),
    targetLink=im_common:format_utf8(TargetLink),
    workHours=im_common:format_utf8(WorkHours),
    userId=im_common:format_id(UserId),
    address=im_common:format_utf8(Address),
    phone=im_common:format_utf8(Phone),
    location=format_geo(Location),
    date=Date,
    vendorId=im_common:format_id(VendorId),
    media=format_media_list(Media),
    paymentInfo=format_payment_info(PaymentInfo)
  };
format_feed_post(_) -> undefined.

format_feed_post_tags(undefined) -> undefined;
format_feed_post_tags(Tags) -> [im_common:format_utf8(Tag) || Tag <- Tags].

format_feed_post_categories(undefined) -> undefined;
format_feed_post_categories(Categories) ->
  [im_common:format_id(Category) || Category <- Categories].

format_payment_info(undefined) -> undefined;
format_payment_info({<<"paymentInfo">>, PaymentInfoProps}) ->
  #'FeedPostPaymentInfoEntity'{
    type=proplists:get_value(<<"type">>, PaymentInfoProps),
    price=im_common:ensure_integer(proplists:get_value(<<"price">>, PaymentInfoProps)),
    useQty=im_common:ensure_integer(proplists:get_value(<<"useQty">>, PaymentInfoProps)),
    discount=proplists:get_value(<<"discount">>, PaymentInfoProps),
    currency=proplists:get_value(<<"currency">>, PaymentInfoProps),
    availableDates=format_payment_info_dates(proplists:get_value(<<"availableDates">>, PaymentInfoProps)),
    durationInMinutes=proplists:get_value(<<"durationInMinutes">>, PaymentInfoProps),
    doRequestDate=proplists:get_value(<<"doRequestDate">>, PaymentInfoProps),
    startingDate=proplists:get_value(<<"startingDate">>, PaymentInfoProps)};
format_payment_info(_) -> undefined.

format_payment_info_dates(undefined) -> undefined;
format_payment_info_dates(AvailableDates) ->
  [format_payment_info_date(Date) || Date <- AvailableDates].

format_payment_info_date(undefined) -> undefined;
format_payment_info_date({<<"paymentInfoDate">>, DateProps}) ->
  #'FeedPostPaymentInfoDateEntity'{
    date=proplists:get_value(<<"date">>, DateProps),
    availableQty=proplists:get_value(<<"availableQty">>, DateProps),
    reservedQty=proplists:get_value(<<"reservedQty">>, DateProps),
    availableToBuyQty=proplists:get_value(<<"availableToBuyQty">>, DateProps)}.

parse_feed_post(#'FeedPostEntity'{created=Created, type=Type, title=Title, payload=Payload, media=Media, categories=Categories,
  author=Author, thumbnail=Thumbnail, buttonCaption=ButtonCaption, targetLink=TargetLink, parentId=ParentId, date=Date, vendorId=VendorId,
  tags=Tags, location=Location, workHours=WorkHours, userId=UserId, address=Address, descr=Descr, phone=Phone, paymentInfo=PaymentInfo}) ->
  Created1 = case Created =:= undefined of
    true -> sm:now();
    false -> im_common:ensure_timestamp(Created)
  end,
  #im_feed_post{created=Created1,
    type=im_common:format_utf8(Type),
    categories=[im_common:parse_id(Category) || Category <- Categories],
    tags=[im_common:format_utf8(Tag) || Tag <- Tags],
    parentId=im_common:parse_id(ParentId),
    descr=im_common:format_utf8(Descr),
    title=im_common:format_utf8(Title),
    payload=im_common:format_utf8(Payload),
    author=im_common:format_utf8(Author),
    thumbnail=im_common:format_utf8(Thumbnail),
    buttonCaption=im_common:format_utf8(ButtonCaption),
    targetLink=im_common:format_utf8(TargetLink),
    location=parse_geo(Location),
    workHours=im_common:format_utf8(WorkHours),
    userId=im_common:parse_id(UserId),
    address=im_common:format_utf8(Address),
    phone=im_common:format_utf8(Phone),
    date=Date,
    vendorId=im_common:parse_id(VendorId),
    media=[parse_media(M) || M <- Media],
    paymentInfo=parse_payment_info(PaymentInfo)}.

parse_payment_info(undefined) -> undefined;
parse_payment_info(#'FeedPostPaymentInfoEntity'{type=Type, price=Price, discount=Discount, currency=Currency, useQty=UseQty,
  availableDates=AvailableDates, durationInMinutes=DurationInMinutes, doRequestDate=DoRequestDate, startingDate=StartingDate}) ->
  UseQty1 = case UseQty of
    undefined -> 1;
    _ -> UseQty
  end,
  {<<"paymentInfo">>, [
    {<<"type">>, Type},
    {<<"price">>, Price},
    {<<"useQty">>, UseQty1},
    {<<"discount">>, Discount},
    {<<"currency">>, im_common:format_utf8(Currency)},
    {<<"availableDates">>, [parse_payment_info_available_date(Date) || Date <- AvailableDates]},
    {<<"durationInMinutes">>, DurationInMinutes},
    {<<"doRequestDate">>, DoRequestDate},
    {<<"startingDate">>, StartingDate}
  ]};
parse_payment_info(_) -> undefined.

parse_payment_info_available_date(#'FeedPostPaymentInfoDateEntity'{date=Date, availableQty=AvailableQty}) ->
  {<<"paymentInfoDate">>, [
    {<<"date">>, Date},
    {<<"availableQty">>, AvailableQty},
    {<<"reservedQty">>, 0},
    {<<"availableToBuyQty">>, AvailableQty}
  ]}.

format_order(#im_order{id=Id, productId=ProductId, vendorId=VendorId, qty=Qty, availableUseQty=AvailableUseQty, status=Status,
  expires=Expires, serial=Serial, date=Date, refundValidTill=RefundValidTill, price=Price, total=Total}) ->
  #'OrderEntity'{id=im_common:format_id(Id),
    productId=im_common:format_id(ProductId),
    vendorId=im_common:format_id(VendorId),
    qty=Qty,
    price=im_common:ensure_integer(Price),
    total=im_common:ensure_integer(Total),
    serial=Serial,
    status=Status,
    date=Date,
    expires=Expires,
    refundValidTill=RefundValidTill,
    availableUseQty=AvailableUseQty}.

format_localized_store(#im_localized_store{id=Id, name=Name, location=Location}) ->
  #'LocalizedStoreEntity'{id=im_common:format_id(Id),
    name=im_common:format_utf8(Name),
    location=format_geo(Location)}.

parse_localized_store(#'LocalizedStoreEntity'{id=Id, name=Name, location=Location}) ->
  #im_localized_store{id=im_common:parse_id(Id),
    name=im_common:format_utf8(Name),
    location=parse_geo(Location)}.

get_localized_value(Data, Locale) when is_list(Data) andalso length(Data) > 0 ->
  Hd = hd(Data),
  case is_tuple(Hd) of
    true ->
      {_,FirstVal} = Hd,
      proplists:get_value(im_common:ensure_binary(Locale), Data, FirstVal);
    false -> Data
  end;
get_localized_value(_, _) -> "".

format_page(#im_page{id=Id, title=Title, content=Content}) ->
  #'PageEntity'{id=im_common:format_utf8(atom_to_list(Id)),
    title=im_common:format_utf8(Title),
    content=im_common:format_utf8(Content)}.

format_channel(#channel{id=Id, name=Name, descr=Descr, location_id=LocationId, category_id=CategoryId, thumb=Thumb, user_count=UserCount}) ->
  #'ChannelEntity'{
    id=im_common:format_id(Id),
    name=im_common:format_utf8(Name),
    descr=im_common:format_utf8(Descr),
    thumb=im_common:format_utf8(Thumb),
    locationId=im_common:format_id(LocationId),
    categoryId=im_common:format_id(CategoryId),
    userCount=UserCount
  }.

parse_channel(#'ChannelEntity'{id=Id, name=Name, thumb=Thumb}) ->
  #channel{id=Id, name=Name, thumb=Thumb}.

format_location(#channel_location{id=Id, location_tag=LocationTag, name=Name, coordinates=[Lat,Lng], radius=Radius, thumb=Thumb}) ->
  #'LocationEntity'{id=Id,
    locationTag=LocationTag,
    name=Name,
    coordinates=[Lat,Lng],
    radius=Radius,
    thumb=Thumb}.

parse_location(#'LocationEntity'{id=Id, locationTag=LocationTag, name=Name, coordinates=[Lat,Lng], radius=Radius, thumb=Thumb}) ->
  #channel_location{id=Id,
    location_tag=LocationTag,
    name=Name,
    coordinates=[Lat,Lng],
    radius=Radius,
    thumb=Thumb}.

format_channel_category(#channel_category{id=Id, name=Name}) ->
  #'ChannelCategoryEntity'{id=Id, name=Name}.

parse_channel_category(#'ChannelCategoryEntity'{id=Id, name=Name}) ->
  #channel_category{id=Id, name=Name}.

format_ogdata(undefined) -> undefined;
format_ogdata(#im_ogdata{id=Url, domain=Domain, favicon=Favicon, title=Title, description=Description, image=Image, fetched=Fetched, context=Context, media=Media}) ->
  #'OgData'{url=im_common:format_utf8(Url),
    domain=im_common:format_utf8(Domain),
    favicon=im_common:format_utf8(Favicon),
    title=im_common:format_utf8(Title),
    description=im_common:format_utf8(Description),
    context=im_common:format_utf8(Context),
    image=im_common:format_utf8(Image),
    media=format_ogdata_media_list(Media),
    fetched=im_common:ensure_timestamp(Fetched)}.

format_ogdata_media_list(undefined) -> [];
format_ogdata_media_list(Media) ->
  [format_ogdata_media(M) || M <- Media].

format_ogdata_media({<<"ogmedia">>, Props}) ->
  #'OgMedia'{image=proplists:get_value(<<"image">>, Props),
    thumbnail=proplists:get_value(<<"thumbnail">>, Props),
    description=proplists:get_value(<<"description">>, Props)}.

user_to_json(#im_usr{id=UserId, phone=Phone, name=Name, username=Username, photo=Photo, thumbnail=Thumbnail,
  bio=Bio, data=Data, isNew=IsNew, isBot=IsBot, isVendor=IsVendor, vendorId=VendorId}) ->
  jsx:encode([
    {<<"userId">>, im_common:format_id(UserId)},
    {<<"vendorId">>, im_common:format_id(VendorId)},
    {<<"name">>, im_common:format_utf8(Name)},
    {<<"username">>, im_common:format_utf8(Username)},
    {<<"photo">>, im_common:format_utf8(Photo)},
    {<<"thumbnail">>, im_common:format_utf8(Thumbnail)},
    {<<"phone">>, im_common:format_utf8(Phone)},
    {<<"bio">>, im_common:format_utf8(Bio)},
    {<<"data">>, im_common:format_utf8(im_common:mongo_to_json(Data))},
    {<<"isNew">>, im_common:ensure_boolean(IsNew)},
    {<<"isBot">>, im_common:ensure_boolean(IsBot)},
    {<<"isVendor">>, im_common:ensure_boolean(IsVendor)}
  ]).

format_device(#im_device{id=DeviceId, name=DeviceName, os=OS}) ->
  #'DeviceEntity'{
    os=OS,
    deviceId=DeviceId,
    deviceName=DeviceName
  }.
