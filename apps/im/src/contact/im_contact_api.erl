-module(im_contact_api).

-include("im_common.hrl").
-include_lib("smoothie/include/sm.hrl").

-export([upload/3, add/2, delete/2, get/2, sync_contacts/2, update/2, block/2, unblock/2, add_to_spam/2]).

upload(#'UploadContacts'{ref = Ref, contacts = Contacts}, #im_usr{id = UserId}, #im_usr_token{deviceId = DeviceId}) ->
  im_contact_queue:add(#im_add_contacts_task{userId = UserId, deviceId = DeviceId, data = {<<"binary">>, term_to_binary(Contacts)}}),
  {ok, #'UploadContactsResp'{ref = Ref}}.

add(#'AddContact'{ref = Ref, contact = ContactEntity}, #im_usr{id = UserId}) ->
  ContactUserId = ContactEntity#'ContactEntity'.userId,

  {ok, P1}=im_contact_fsm:start_link([UserId, ContactUserId]),
  {ok, P2}=im_contact_fsm:start_link([ContactUserId, UserId]),

  case im_contact_fsm:add(P1, P2) of
    Contact=#im_contact{} ->
      % im_message:send_sys_msg(ContactUserId, ?MESSAGE_FEED_TYPE_CHAT, UserId, <<"system.you.added.user.to.contacts">>, [im_message:markup_user(ContactUserId)], true, UserId),
      im_message:send_sys_msg(UserId, ?MESSAGE_FEED_TYPE_CHAT, ContactUserId, <<"system.user.added.you.to.contacts">>, [im_message:markup_user(UserId)], true, ContactUserId),

      {ok, #'AddContactResp'{ref = Ref, contact = im_contact:format(Contact)}};
    {error, denied} ->
      {ok, #'ErrorResp'{
        code = ?ERROR_CODE_PERMISSION_DENIED,
        ref = Ref,
        messageParams = [],
        messageType = <<"contact.you.cannot.add.this.user">>,
        message = im_common:format_utf8(im_trans:t(<<"contact.you.cannot.add.this.user">>))}}
  end.

delete(#'DeleteContact'{ref = Ref, userId = TargetUserId}, #im_usr{id = UserId}) ->
  {ok, P1}=im_contact_fsm:start_link([UserId, TargetUserId]),
  {ok, P2}=im_contact_fsm:start_link([TargetUserId, UserId]),

  case im_contact_fsm:delete(P1, P2) of
    Contact=#im_contact{} ->
      {ok, #'DeleteContactResp'{ref = Ref, contact = im_contact:format(Contact)}};
    {error, denied} ->
      {ok, #'ErrorResp'{
        code = ?ERROR_CODE_PERMISSION_DENIED,
        ref = Ref,
        messageParams = [],
        messageType = <<"contact.cannot.delete.this.user">>,
        message = im_common:format_utf8(im_trans:t(<<"contact.cannot.delete.this.user">>))}}
  end.

block(#'BlockContact'{ref = Ref, userId = TargetUserId}, #im_usr{id = UserId}) ->
  {ok, P1}=im_contact_fsm:start_link([UserId, TargetUserId]),
  {ok, P2}=im_contact_fsm:start_link([TargetUserId, UserId]),

  case im_contact_fsm:block(P1, P2) of
    Contact=#im_contact{} ->
      {ok, #'BlockContactResp'{ref = Ref, contact = im_contact:format(Contact)}};
    {error, denied} ->
      {ok, #'ErrorResp'{
        code = ?ERROR_CODE_PERMISSION_DENIED,
        ref = Ref,
        messageParams = [],
        messageType = <<"contact.you.cannot.block.this.user">>,
        message = im_common:format_utf8(im_trans:t(<<"contact.you.cannot.block.this.user">>))}}
  end.

unblock(#'UnBlockContact'{ref = Ref, userId = TargetUserId}, #im_usr{id = UserId}) ->
  {ok, P1}=im_contact_fsm:start_link([UserId, TargetUserId]),
  {ok, P2}=im_contact_fsm:start_link([TargetUserId, UserId]),

  case im_contact_fsm:unblock(P1, P2) of
    Contact=#im_contact{} ->
      {ok, #'UnBlockContactResp'{ref = Ref, contact = im_contact:format(Contact)}};
    {error, denied} ->
      {ok, #'ErrorResp'{
        code = ?ERROR_CODE_PERMISSION_DENIED,
        ref = Ref,
        messageParams = [],
        messageType = <<"contact.you.cannot.unblock.this.user">>,
        message = im_common:format_utf8(im_trans:t(<<"contact.you.cannot.unblock.this.user">>))}}
  end.

add_to_spam(#'AddToSpam'{ref = Ref, userId = TargetUserId}, #im_usr{id = UserId}) ->
  {ok, P1}=im_contact_fsm:start_link([UserId, TargetUserId]),
  {ok, P2}=im_contact_fsm:start_link([TargetUserId, UserId]),

  case im_contact_fsm:add_to_spam(P1, P2) of
    Contact=#im_contact{} ->
      {ok, #'AddToSpamResp'{ref = Ref, contact = im_contact:format(Contact)}};
    {error, denied} ->
      {ok, #'ErrorResp'{
        code = ?ERROR_CODE_PERMISSION_DENIED,
        ref = Ref,
        messageParams = [],
        messageType = <<"contact.cannot.add.to.spam.this.user">>,
        message = im_common:format_utf8(im_trans:t(<<"contact.cannot.add.to.spam.this.user">>))}}
  end.

update(#'UpdateContact'{ref = Ref, contact = ContactEntity}, #im_usr{id = UserId}) ->
  case im_contact:update(UserId, ContactEntity#'ContactEntity'.userId, ContactEntity#'ContactEntity'.name, ContactEntity#'ContactEntity'.labels) of
    {ok, UpdatedContact} ->
      {ok, #'UpdateContactResp'{ref = Ref, contact = im_contact:format(UpdatedContact)}};
    {error, name_required} ->
      {ok, #'ErrorResp'{
        code = ?ERROR_CODE_CONTACT_NAME_REQUIRED,
        ref = Ref,
        messageParams = [],
        messageType = <<"contact.contact.name.is.required">>,
        message = im_common:format_utf8(im_trans:t(<<"contact.contact.name.is.required">>))}};
    {error, not_found} ->
      {ok, #'ErrorResp'{
        code = ?ERROR_CODE_NOT_FOUND,
        ref = Ref,
        messageParams = [],
        messageType = <<"contact.contact.not.found">>,
        message = im_common:format_utf8(im_trans:t(<<"contact.contact.not.found">>))}}
  end.

get(#'Contact'{ref = Ref, userId = TargetUserId}, User=#im_usr{id = UserId}) ->
  case im_contact:find(UserId, TargetUserId) of
    {ok, C} ->
      {ok, #'ContactResp'{ref = Ref, contact = im_contact:format(C)}};
    {error, not_found} ->
      TargetUser = case TargetUserId =:= undefined of
        true -> undefined;
        false -> try im_roster_chat:get(TargetUserId) catch _:_ -> undefined end
      end,
      case TargetUser =:= undefined of
        true ->
          {ok, #'ErrorResp'{ref = Ref,
            code = ?ERROR_CODE_NOT_FOUND,
            messageParams = [],
            message = im_common:format_utf8(im_trans:t(<<"auth.user.not.found">>))}};
        false ->
          {ok, #'ContactResp'{ref = Ref, contact = im_contact:format(im_contact:generate_new_contact_from_user(User, TargetUser))}}
      end
  end.

sync_contacts(#'SyncContacts'{ref = Ref, syncTime = SyncTime}, #im_usr{id = UserId}) ->
  Contacts=case ctail_feed:get(im_contact, {<<"contacts">>, im_common:parse_id(UserId)}, -1) of
    undefined -> [];
    Result -> [C||C <- Result, C#im_contact.updatedAt >= SyncTime]
  end,
  {ok, #'SyncContactsResp'{ref = Ref, serverTime = sm:now(), contacts = [im_contact:format(C)||C <- Contacts]}}.
