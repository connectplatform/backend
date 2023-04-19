-module(im_contact_queue).

-include("im_common.hrl").

-export([spec/0, handle/3]).
-export([add/1]).

-define(TABLE, im_add_contacts_task).
-define(MAX_RETRIES, 5).
-record(state, {feed=#feed{id=add_contacts_queue} :: any()}).

spec() -> im_std_worker:spec(contact_queue, [], fun ?MODULE:handle/3).

add(Task) -> im_std_worker:call(contact_queue, {add, Task}).

handle(init, _, _) ->
  State = case ctail:get(feed, add_contacts_queue) of
    {ok, Feed} -> #state{feed=Feed};
    {error, _} -> #state{feed=#feed{id=add_contacts_queue}}
  end,
  timer:send_after(3000, self(), process),
  {ok, State};

handle(cast, process, State=#state{feed=Feed}) ->
  Result = case Feed#feed.bottom of
    undefined -> {error, no_tasks};
    BottomId  -> ctail:get(?TABLE, BottomId)
  end,

  State1 = case Result of
    {ok, Task} ->
      case process(Task) of
        ok ->
          {ok, NewFeed} = remove(Task#?TABLE.id),
          gen_server:cast(?MODULE, process),
          State#state{feed=NewFeed};
        {error, retrie} ->
          Retries = Task#im_add_contacts_task.retries,
          State2 = case Retries of
            ?MAX_RETRIES ->
              {ok, NewFeed} = remove(Task#?TABLE.id),
              State#state{feed=NewFeed};
            _ ->
              put(Task#im_add_contacts_task{retries=Retries + 1}),
              State
          end,
          gen_server:cast(?MODULE, process),
          State2
      end;
    {error, _} ->
      timer:send_after(100, self(), process),
      State
  end,
  {noreply, State1};

handle(call, {add, Task}, State) ->
  ctail_feed:add(Task#im_add_contacts_task{id=ctail:next_id(), feed_id=add_contacts_queue, retries=0}),
  {ok, Feed} = ctail:get(feed, add_contacts_queue),
  {reply, ok, State#state{feed=Feed}};

handle(info, process, State) ->
  gen_server:cast(?MODULE, process),
  {noreply, State};

handle(_, _, _) -> ok.

put(Task) ->
  ctail:put(Task).

remove(TaskId) ->
  ctail_feed:remove(?TABLE, TaskId),
  ctail:get(feed, add_contacts_queue).

process(Task) ->
  try process_contacts(Task) of
    _ -> ok
  catch
    error:_ -> {error, retrie}
  end.

process_contacts(#im_add_contacts_task{userId=UserId, data=Data, deviceId=DeviceId}) ->
  {<<"binary">>, BinData} = Data,
  ContactEntities = binary_to_term(BinData),

  Records = parse_upload_contacts(ContactEntities, UserId),
  NewContacts = im_contact_merge:process(UserId, Records),

  UserIds = [element(#im_contact.userId, Contact) ||  Contact <- NewContacts, element(#im_contact.userId, Contact) =/= undefined],
  contact_resolved_event({userId=UserId, deviceId=DeviceId, resolvedUserIds=UserIds}),
  ok.

parse_upload_contacts(ContactEntities, UserId) ->
  Contacts = lists:foldl(fun(ContactEntity, Acc) ->
                           ParsedContacts = apportion_contact(ContactEntity, UserId),
                           Acc ++ ParsedContacts
                         end, [], ContactEntities),
  UniquePhoneList = get_unique_phone_list(Contacts, []),
  FilteredContacts = remove_duplicate(UniquePhoneList, Contacts, []),

  FilteredContacts.

get_unique_phone_list([], Acc)                                  -> lists:usort(Acc);
get_unique_phone_list([#im_contact{phone=Phone}|Contacts], Acc) -> get_unique_phone_list(Contacts, [Phone|Acc]).

remove_duplicate(_, [], Acc) -> Acc;
remove_duplicate(PhoneList, [Contact=#im_contact{phone=Phone}|Contacts], Acc) ->
  case lists:member(Phone, PhoneList) of
    true ->
      NewPhoneList = lists:delete(Phone, PhoneList),
      remove_duplicate(NewPhoneList, Contacts, [Contact|Acc]);
    false ->
      remove_duplicate(PhoneList, Contacts, Acc)
  end.

apportion_contact(ContactEntity=#'ContactEntity'{phone=Phones}, UserId) ->
  Fun = fun(Phone, Acc) ->
    StrippedPhone = re:replace(Phone, "[^0-9]", "", [global, {return, list}]),
    FormatedPhone = im_common:format_phone(StrippedPhone),
    case FormatedPhone of
      "" -> Acc;
      _  ->
        FormatedPhone1 = im_common:format_phone_with_code(UserId, StrippedPhone),
        case FormatedPhone1 of
          {ok, FormatedPhone2} ->
            Contact = #im_contact{
              userId        = im_common:parse_id(ContactEntity#'ContactEntity'.userId),
              name          = ContactEntity#'ContactEntity'.name,
              phone         = FormatedPhone2,
              originalPhone = Phone,
              thumbnail     = ContactEntity#'ContactEntity'.thumbnail,
              photo         = ContactEntity#'ContactEntity'.photo,
              status        = ?CONTACT_STATUS_FRIEND,
              email         = ContactEntity#'ContactEntity'.email,
              skype         = ContactEntity#'ContactEntity'.skype,
              facebookId    = ContactEntity#'ContactEntity'.facebookId,
              vkontakteId   = ContactEntity#'ContactEntity'.vkontakteId,
              departmentId  = ContactEntity#'ContactEntity'.departmentId,
              bio           = ContactEntity#'ContactEntity'.bio
            },

            [Contact|Acc];
          _ -> Acc
        end
    end
  end,

  lists:foldl(Fun, [], Phones).

contact_resolved_event({userId=UserId, deviceId=DeviceId, resolvedUserIds=ResolvedUserIds}) ->
  ExistingUserIds = roster_chat:list(UserId),
  lists:foreach(fun (ResolvedUserId) ->
    case lists:member(ResolvedUserId, ExistingUserIds) of
      false ->
        im_roster_chat:worker(UserId),
        im_roster_chat:worker(ResolvedUserId),
        roster_chat:add(UserId, ResolvedUserId);
      true ->
        skip
    end
  end, ResolvedUserIds),

  %% TODO remove this event
  Msg = #'ContactResolvedEvent'{userId=im_common:format_id(UserId), deviceId=DeviceId},

  im_user_state:broadcast(undefined, [UserId], Msg).
