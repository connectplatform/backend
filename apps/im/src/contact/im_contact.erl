-module(im_contact).

-include("im_common.hrl").
-include_lib("smoothie/include/sm.hrl").

-export([
  find/2,
  format/1,
  update/4,
  upsert/1,
  generate_new_contact_from_user/2,
  get_contact_ids_by_phone/1,
  update_contacts_when_user_changed/1,
  get_friends_ids/1,
  add_contacts_after_user_registered/1,
  get_system_labels/1
]).

-define(NAME_CHANGE_STRATEGY_FROM_USER, 'strategy_from_user').
-define(NAME_CHANGE_STRATEGY_FROM_CONTACT, 'strategy_from_contact').

update_contacts_when_user_changed(#im_usr{id=_UserId, phone=undefined}) ->
  [];
update_contacts_when_user_changed(User=#im_usr{id=UserId, phone=Phone}) ->
  Lookup = ensure_lookup(Phone),
  case element(#im_lookup.userId, Lookup) of
    undefined -> ctail:put(setelement(#im_lookup.userId, Lookup, UserId));
    _         -> skip
  end,
  update_contacts_when_user_changed(element(#im_lookup.contactIds, Lookup), User, []).

update_contacts_when_user_changed([], _, Acc) -> Acc;
update_contacts_when_user_changed([ContactId|ContactsIds], User, Acc) ->
  case ctail:get(im_contact, ContactId) of
    {ok, Contact} ->
      ContactName = case get_name_change_strategy() of
        ?NAME_CHANGE_STRATEGY_FROM_USER -> User#im_usr.name;
        ?NAME_CHANGE_STRATEGY_FROM_CONTACT ->
          case Contact#im_contact.name of
            undefined -> User#im_usr.name;
            _         -> Contact#im_contact.name
          end
      end,

      UpdatedContact = im_contact:upsert(Contact#im_contact{
        name = ContactName,
        photo = User#im_usr.photo,
        thumbnail = User#im_usr.thumbnail,
        userId = User#im_usr.id,
        departmentId = User#im_usr.departmentId,
        updatedAt = sm:now(),
        bio = User#im_usr.bio
      }),
      {<<"contacts">>, ToUserId} = UpdatedContact#im_contact.feed_id,
      UpdatedAcc = case lists:keyfind(ToUserId, 1, Acc) of
        false -> [UpdatedContact|Acc];
        _     -> Acc
      end,
      update_contacts_when_user_changed(ContactsIds, User, UpdatedAcc);
    _ ->
      im_logger:error(User#im_usr.id, "[Contact] Contact not found in 'update_contacts_when_user_changed'. ContactId: ~p", [ContactId]),
      update_contacts_when_user_changed(ContactsIds, User, Acc)
  end.

add_contacts_after_user_registered(User) ->
  case im_app_conf_watcher:get("contacts") of
    {ok, {struct, List}} when is_list(List) ->
      case proplists:get_value("add", List) of
        {array, Rules} when is_list(Rules) ->
          FindResults = lists:map(fun({struct, Rule}) ->
            Conditions=case proplists:get_value("conditions", Rule) of
              {array, Conditions1} when is_list(Conditions1) ->
                lists:map(fun({struct, [{"field", Field}, {"values", {array, Values}}]}) ->
                  {im_common:ensure_binary(Field), {<<"$in">>, lists:map(fun({struct, V}) ->
                    im_common:ensure_binary(proplists:get_value("value", V))
                  end, Values)}}
                end, Conditions1);
              _ -> skip
            end,
            Operator = case proplists:get_value("operator", Rule) of
              "and" -> "$and";
              "or"  -> "$or";
              Op    -> Op
            end,
            Selector={im_common:ensure_binary(Operator), Conditions},
            ctail_mongo:find(im_usr, Selector, 0)
          end, Rules),

          %% Merge results
          UsersToAdd = lists:foldl(fun(FindResult, Acc) ->
            lists:foldl(fun(U=#im_usr{id=UserId}, Acc1) ->
              case lists:filter(fun(#im_usr{id=AccUserId}) -> AccUserId =:= UserId end, Acc1) of
                [] -> [U|Acc1];
                _  -> Acc1
              end
            end, Acc, FindResult)
          end, [], FindResults),

          %% Add contacts
          lists:foreach(fun(#im_usr{id=UId}) ->
            im_contact_api:add(#'AddContact'{contact=#'ContactEntity'{userId=UId}}, User)
          end, UsersToAdd),
          im_logger:debug(User#im_usr.id, "[Contact] Add contacts when user registered ~p", [[im_common:format_id(C#im_usr.id) || C <- UsersToAdd]]),
          ok;
        _ ->
          skip
      end;
    _ -> skip
  end.

find(UserId, TargetId) ->
  ParsedUserId=im_common:parse_id(UserId),
  ParsedTargetId=im_common:parse_id(TargetId),
  Contact=lists:foldl(fun(Contact, Acc) ->
    case im_common:parse_id(Contact#im_contact.userId) =:= ParsedTargetId of
      true -> Contact;
      false -> Acc
    end
  end, [], ctail_feed:get(im_contact, {<<"contacts">>, ParsedUserId}, -1)),

  case Contact of
    [] -> {error, not_found};
    _ -> {ok, Contact}
  end.

get_friends_ids(UserId) ->
  Friends = lists:foldl(fun(Contact, Acc) ->
    case Contact#im_contact.status =:= ?CONTACT_STATUS_FRIEND of
      true -> [Contact | Acc];
      false -> Acc
    end
  end, [], ctail_feed:get(im_contact, {<<"contacts">>, UserId}, -1)),

  lists:map(fun(#im_contact{userId = UId}) -> im_common:parse_id(UId) end, Friends).

generate_new_contact_from_user(User=#im_usr{}, TargetUser=#im_usr{}) ->
  #im_contact{
    feed_id = {<<"contacts">>, im_common:parse_id(User#im_usr.id)},
    thumbnail = TargetUser#im_usr.thumbnail,
    photo = TargetUser#im_usr.photo,
    name = TargetUser#im_usr.name,
    username = TargetUser#im_usr.username,
    isBot = TargetUser#im_usr.isBot,
    isVendor = TargetUser#im_usr.isVendor,
    userId = im_common:format_id(TargetUser#im_usr.id),
    phone = TargetUser#im_usr.phone,
    originalPhone = TargetUser#im_usr.phone,
    status = ?CONTACT_STATUS_UNKNOWN,
    friend = false,
    blockedBySystem = not im_common:ensure_boolean(TargetUser#im_usr.active)
  }.

format(User=#im_usr{}) ->
  UserEntity = im_dto:format_user(User),
  #'ContactEntity'{
    userId        = UserEntity#'UserEntity'.id,
    name          = UserEntity#'UserEntity'.name,
    username      = UserEntity#'UserEntity'.username,
    phone         = UserEntity#'UserEntity'.phone,
    originalPhone = UserEntity#'UserEntity'.phone,
    email         = UserEntity#'UserEntity'.email,
    skype         = UserEntity#'UserEntity'.skype,
    isBot         = UserEntity#'UserEntity'.isBot,
    photo         = UserEntity#'UserEntity'.photo,
    thumbnail     = UserEntity#'UserEntity'.thumbnail,
    isVendor      = UserEntity#'UserEntity'.isVendor,
    bio           = UserEntity#'UserEntity'.bio
  };
format(Contact=#im_contact{}) ->
  ContactEntity = #'ContactEntity'{
    id = im_common:format_id(Contact#im_contact.id),
    userId = im_common:format_id(Contact#im_contact.userId),
    name = im_common:format_utf8(Contact#im_contact.name),
    username = im_common:format_utf8(Contact#im_contact.username),
    thumbnail = Contact#im_contact.thumbnail,
    photo = Contact#im_contact.photo,
    blockedBySystem = Contact#im_contact.blockedBySystem,
    departmentId = im_common:format_id(Contact#im_contact.departmentId),
    created = Contact#im_contact.createdAt,
    updated = Contact#im_contact.updatedAt,
    status = Contact#im_contact.status,
    isBot = Contact#im_contact.isBot,
    friend = Contact#im_contact.friend,
    labels = Contact#im_contact.labels,
    bio = Contact#im_contact.bio
  },

  {<<"contacts">>, UserId} = Contact#im_contact.feed_id,
  case im_acl:has_role("super_admin", UserId) of
    true ->
      ContactEntity#'ContactEntity'{
        phone = im_common:format_utf8(Contact#im_contact.phone),
        originalPhone = im_common:format_utf8(Contact#im_contact.originalPhone),
        email = im_common:format_utf8(Contact#im_contact.email)
      };
    false ->
      ContactEntity
  end.

update(UserId, TargetUserId, NewContactName, NewCustomContactLabels) ->
  SystemLabels = get_system_labels(TargetUserId),
  Labels = case NewCustomContactLabels of
    ExistsLabels when is_list(ExistsLabels) -> sets:to_list(sets:from_list(ExistsLabels ++ SystemLabels));
    _ -> SystemLabels
  end,
  case im_contact:find(UserId, TargetUserId) of
    {ok, Contact=#im_contact{friend = true}} ->
      case NewContactName =/= undefind of
        true ->
          UpdatedContact = im_contact:upsert(Contact#im_contact{name = NewContactName, labels=Labels}),
          {ok, UpdatedContact};
        false -> {error, name_required}
      end;
    _ -> {error, not_found}
  end.

upsert(Contact=#im_contact{}) ->
  Now=sm:now(),
  {<<"contacts">>, UserId}=Contact#im_contact.feed_id,
  IsNewContact=Contact#im_contact.id =:= undefined,

  ResultContact=case IsNewContact of
    true ->
      SystemLabels = get_system_labels(Contact#im_contact.userId),
      NewContact=Contact#im_contact{id = ctail:next_id(), labels=SystemLabels, createdAt = Now, updatedAt = Now},
      %% update lookup
      case NewContact#im_contact.phone of
        undefined ->
          skip;
        _ ->
          Lookup=case ctail:get(im_lookup_phone, im_common:format_lookup_value(NewContact#im_contact.phone)) of
            {ok, PhoneLookup} -> PhoneLookup;
            {error, not_found} -> #im_lookup_phone{id = im_common:format_lookup_value(NewContact#im_contact.phone)}
          end,
          ContactIds=element(#im_lookup.contactIds, Lookup),
          case lists:member(NewContact#im_contact.id, ContactIds) of
            true -> skip;
            false -> ctail:put(setelement(#im_lookup.contactIds, Lookup, [NewContact#im_contact.id|ContactIds]))
          end
      end,
      ctail_feed:add(NewContact),
      update_chat_updates(NewContact),
      NewContact;
    false ->
      {ok, ExistContact}=ctail:get(im_contact, Contact#im_contact.id),

      UpdatedContact=Contact#im_contact{updatedAt = Now},
      ctail:put(UpdatedContact),
      %% Update ChatUpdates if name photo or thumbnail changed
      case (ExistContact#im_contact.name =/= UpdatedContact#im_contact.name)or
        (ExistContact#im_contact.photo =/= UpdatedContact#im_contact.photo)or
        (ExistContact#im_contact.thumbnail =/= UpdatedContact#im_contact.thumbnail)
      of
        true -> update_chat_updates(UpdatedContact);
        false -> skip
      end,
      UpdatedContact
  end,

  im_user_state:broadcast(undefined, [UserId], #'ContactChanged'{contact = format(ResultContact)}),
  ResultContact.

update_chat_updates(Contact=#im_contact{feed_id = {<<"contacts">>, UserId}, userId = TargetUserId}) ->
  Func = fun(User) ->
    UpdateId = im_chatupdate:msg_feed_to_roster(?MESSAGE_FEED_TYPE_CHAT, im_common:parse_id(TargetUserId), UserId),
    case im_chatupdate:find(UpdateId, User) of
      {ok, Update} ->
        UpdatedUpdate = Update#im_chat_update{
          name = Contact#im_contact.name,
          thumbnail = Contact#im_contact.thumbnail},
        User1 = im_chatupdate:upsert(UpdatedUpdate, User),
        {{UpdatedUpdate, User1}, User1};
      {error, _} ->
        {undefined, User}
    end
  end,
  case im_roster_chat:execute(UserId, Func) of
    undefined -> skip;
    {Update, User} -> im_chatupdate:send_update(User, Update)
  end.

get_contact_ids_by_phone(undefined) -> [];
get_contact_ids_by_phone(Phone) ->
  element(#im_lookup.contactIds, ensure_lookup(Phone)).

ensure_lookup(undefined) ->
  {error, invalid_phone};
ensure_lookup(Phone) ->
  case ctail:get(im_lookup_phone, im_common:format_lookup_value(Phone)) of
    {ok, PhoneLookup} -> PhoneLookup;
    {error, not_found} -> #im_lookup_phone{id = im_common:format_lookup_value(Phone)}
  end.

get_name_change_strategy() ->
  case im_directory:is_turnedon() of
    true  -> ?NAME_CHANGE_STRATEGY_FROM_USER;
    false -> ?NAME_CHANGE_STRATEGY_FROM_CONTACT
  end.

get_system_labels(UserId) ->
  User = im_roster_chat:get(UserId),
  Proplist = lists:zip(record_info(fields, im_usr), tl(tuple_to_list(User))),

  IsExist = fun(SourceValue, ConfigValue, T) ->
    case T of
      "array" ->
        case  is_list(SourceValue) of
          true ->
            case lists:filter(fun(SourceValue1) -> im_common:ensure_list(SourceValue1) =:= im_common:ensure_list(ConfigValue) end, SourceValue) of
              [] -> false;
              _  -> true
            end;
          false -> false
        end;
      _ ->
        im_common:ensure_list(SourceValue) =:= im_common:ensure_list(ConfigValue)
    end
  end,

  Result = case im_app_conf_watcher:get("contacts") of
    {ok, {struct, List}} when is_list(List) ->
      case proplists:get_value("labels", List) of
        {struct, LabelsConf} when is_list(LabelsConf) ->
          lists:foldl(fun({FieldName, {struct, Conf}}, Acc1) ->
            case proplists:get_value("values", Conf) of
              {array, Values} ->
                lists:foldl(fun(V, Acc2) ->
                  case V of
                    {struct, V1} ->
                      Value = proplists:get_value("value", V1, invalid),
                      Type = proplists:get_value("type", Conf, invalid),
                      LabelName = proplists:get_value("label", V1, invalid),
                      case is_list(FieldName)
                        andalso LabelName =/= invalid
                        andalso Type =/= invalid
                        andalso proplists:get_value(list_to_atom(FieldName), Proplist, invalid) =/= invalid
                        andalso IsExist(proplists:get_value(list_to_atom(FieldName), Proplist), Value, Type) =:= true
                      of
                        true -> [LabelName|Acc2];
                        false -> Acc2
                      end;
                    _ ->
                      Acc2
                  end
                end, Acc1, Values);
              _ -> Acc1
            end
          end, [], LabelsConf);
        _ ->
          []
      end;
    _ -> []
  end,

  sets:to_list(sets:from_list(Result)).
