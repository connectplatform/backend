-module(im_contact_merge).

-include("im_common.hrl").

-export([process/2]).

process(UserId, Contacts) ->
  DbContacts  = ctail_feed:get(im_contact, {<<"contacts">>, UserId}, -1),
  MergedContacts = merge(Contacts, DbContacts, []),
  User = im_roster_chat:get(UserId),

  MyPhone = User#im_usr.phone,
  EnsureWithoutMeContacts = [NewContact || NewContact=#im_contact{phone=P} <- MergedContacts, P =/= MyPhone],

  MergedContacts2 = lists:foldl(fun(Contact=#im_contact{phone=P}, Acc) ->
    Contact1 = case Contact#im_contact.id of
                 undefined -> persist_contact(UserId, Contact);
                 _         -> Contact
               end,

    Phone = im_common:format_lookup_value(P),
    Lookup = case ctail:get(im_lookup_phone, Phone) of
               {ok, ExistLookup} ->
                 case lists:member(Contact1#im_contact.id, ExistLookup#im_lookup_phone.contactIds) of
                   true ->
                     ExistLookup;
                   false ->
                     ExistLookup1 = ExistLookup#im_lookup_phone{contactIds=[Contact1#im_contact.id|ExistLookup#im_lookup_phone.contactIds]},
                     ctail:put(ExistLookup1),
                     ExistLookup1
                 end;
               {error, not_found} ->
                 NewLookup = #im_lookup_phone{id=Phone, contactIds=[Contact1#im_contact.id]},
                 ctail:put(NewLookup),
                 NewLookup
             end,

    Contact2 = case Lookup#im_lookup_phone.userId of
      undefined -> Contact1;
      LookupUserId ->
        ContactUser = im_roster_chat:get(LookupUserId),
        ContactName = case Contact1#im_contact.name of
        undefined    -> ContactUser#im_usr.name;
        ContactName1 -> ContactName1
      end,

      Contact1#im_contact{
        userId = ContactUser#im_usr.id,
        name = ContactName,
        thumbnail = ContactUser#im_usr.thumbnail,
        photo = ContactUser#im_usr.photo
      }
    end,
    case Contact#im_contact.id of
      undefined -> ctail_feed:add(Contact2);
      _         -> ctail:put(Contact2)
    end,
    [Contact2|Acc]
  end, [], EnsureWithoutMeContacts),
  MergedContacts2.

merge([], _, Acc) -> Acc;
merge([Contact|Contacts], DbContacts, Acc) ->
  case find_contact(Contact, DbContacts, []) of
    []         -> merge(Contacts, DbContacts, [Contact|Acc]);
    DbContact  ->
      case DbContact#im_contact.status of
        ?CONTACT_STATUS_PENDING ->
          Name = case Contact#im_contact.name of
                   undefined -> DbContact#im_contact.name;
                   Name1     -> Name1
                 end,
          Contact1 = DbContact#im_contact{status=?CONTACT_STATUS_FRIEND, updatedAt=sm:now(), name=Name},
          merge(Contacts, DbContacts, [Contact1|Acc]);
        _ ->
          merge(Contacts, DbContacts, Acc)
      end
  end.

find_contact(_, [], Acc) -> Acc;
find_contact(Contact=#im_contact{phone=Phone1}, [DbContact=#im_contact{phone=Phone2}|DbContacts], Acc) ->
  case Phone1 =:= Phone2 of
    true  -> DbContact;
    false -> find_contact(Contact, DbContacts, Acc)
  end.

persist_contact(UserId, Contact = #im_contact{}) ->
  case Contact#im_contact.id of
    undefined ->
      NowTime = sm:now(),
      Contact#im_contact{
        id = ctail:next_id(),
        feed_id = {<<"contacts">>, UserId},
        createdAt = NowTime,
        updatedAt = NowTime
      };
    _ ->
      Contact#im_contact{updatedAt = sm:now()}
  end.
