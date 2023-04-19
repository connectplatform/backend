-module(im_contact_fsm).
-behaviour(gen_statem).

-include("im_common.hrl").
-include_lib("smoothie/include/sm.hrl").

%% Events
-define(EVENT_ADD, add).
-define(EVENT_USER_ADDED_ME, user_added_me).
-define(EVENT_DELETE, delete).
-define(EVENT_USER_DELETED_ME, user_deleted_me).
-define(EVENT_BLOCK, block).
-define(EVENT_USER_BLOCKED_ME, user_blocked_me).
-define(EVENT_UNBLOCK, unblock).
-define(EVENT_USER_UNBLOCKED_ME, user_unblocked_me).
-define(EVENT_ADD_TO_SPAM, add_to_spam).
-define(EVENT_USER_ADDED_TO_SPAM_ME, user_added_to_spam_me).

%% initialize
-export([start_link/1]).
%% public API
-export([add/2, delete/2, block/2, unblock/2, add_to_spam/2]).
%% custom state names
-export([unknown_contact/3, pending_invitation/3, friend/3,
  blocked/3, i_am_blocked/3, added_to_spam/3, i_am_added_to_spam/3]).
%% gen_fsm callbacks
-export([init/1, terminate/3, callback_mode/0]).

-record(data, {
  user,
  targetUser,
  contact
}).

%% gen_statem callbacks

init([UserId, TargetUserId]) ->
  User = im_roster_chat:get(UserId),
  TargetUser = im_roster_chat:get(TargetUserId),
  Contact = case im_contact:find(User#im_usr.id, TargetUser#im_usr.id) of
    {ok, C}            -> C;
    {error, not_found} -> im_contact:generate_new_contact_from_user(User, TargetUser)
  end,
  Data = #data{user = User, targetUser = TargetUser, contact = Contact},
  State = get_state(Data),
  {ok, State, Data}.

start_link([UserId, TargetUserId]) ->
  gen_statem:start_link(?MODULE, [UserId, TargetUserId], []).

callback_mode() ->
  state_functions.

terminate(_Reason, _StateName, _StateData) ->
  ok.

%%% Public API

add(OwnPid, OtherPid) ->
  Result = gen_statem:call(OwnPid, ?EVENT_ADD),
  gen_statem:cast(OtherPid, ?EVENT_USER_ADDED_ME),
  Result.

delete(OwnPid, OtherPid) ->
  Result = gen_statem:call(OwnPid, ?EVENT_DELETE),
  gen_statem:cast(OtherPid, ?EVENT_USER_DELETED_ME),
  Result.

block(OwnPid, OtherPid) ->
  Result = gen_statem:call(OwnPid, ?EVENT_BLOCK),
  gen_statem:cast(OtherPid, ?EVENT_USER_BLOCKED_ME),
  Result.

unblock(OwnPid, OtherPid) ->
  Result = gen_statem:call(OwnPid, ?EVENT_UNBLOCK),
  gen_statem:cast(OtherPid, ?EVENT_USER_UNBLOCKED_ME),
  Result.

add_to_spam(OwnPid, OtherPid) ->
  Result = gen_statem:call(OwnPid, ?EVENT_ADD_TO_SPAM),
  gen_statem:cast(OtherPid, ?EVENT_USER_ADDED_TO_SPAM_ME),
  Result.

%% States

unknown_contact({call, From}, ?EVENT_ADD, Data) -> action_add_to_contact(From, Data);
unknown_contact({call, From}, ?EVENT_BLOCK, Data) -> action_block(From, Data);
unknown_contact({call, From}, ?EVENT_ADD_TO_SPAM, Data) -> action_add_to_spam(From, Data);
unknown_contact(cast, ?EVENT_USER_ADDED_ME, Data) -> action_user_add_to_contact_you(Data);
unknown_contact(cast, ?EVENT_USER_BLOCKED_ME, Data) -> action_user_blocked_me(Data);
unknown_contact(cast, ?EVENT_USER_ADDED_TO_SPAM_ME, Data) -> action_user_added_to_spam_me(Data);
unknown_contact(Type, Event, Data) -> handle_event(Type, Event, Data).

pending_invitation({call, From}, ?EVENT_ADD, Data) -> action_add_to_contact(From, Data);
pending_invitation({call, From}, ?EVENT_BLOCK, Data) -> action_block(From, Data);
pending_invitation({call, From}, ?EVENT_ADD_TO_SPAM, Data) -> action_add_to_spam(From, Data);
pending_invitation(cast, ?EVENT_USER_DELETED_ME, Data) -> action_user_deleted_me(Data);
pending_invitation(cast, ?EVENT_USER_BLOCKED_ME, Data) -> action_user_blocked_me(Data);
pending_invitation(cast, ?EVENT_USER_ADDED_TO_SPAM_ME, Data) -> action_user_added_to_spam_me(Data);
pending_invitation(Type, Event, Data) -> handle_event(Type, Event, Data).

friend({call, From}, ?EVENT_DELETE, Data) -> action_delete_contact(From, Data);
friend({call, From}, ?EVENT_BLOCK, Data) -> action_block(From, Data);
friend({call, From}, ?EVENT_ADD_TO_SPAM, Data) -> action_add_to_spam(From, Data);
friend(cast, ?EVENT_USER_BLOCKED_ME, Data) -> action_user_blocked_me(Data);
friend(cast, ?EVENT_USER_ADDED_TO_SPAM_ME, Data) -> action_user_added_to_spam_me(Data);
friend(Type, Event, Data) -> handle_event(Type, Event, Data).

blocked({call, From}, ?EVENT_DELETE, Data) -> action_delete_contact(From, Data);
blocked({call, From}, ?EVENT_UNBLOCK, Data) -> action_unblock(From, Data);
blocked({call, From}, ?EVENT_ADD_TO_SPAM, Data) -> action_add_to_spam(From, Data);
blocked(cast, ?EVENT_USER_BLOCKED_ME, Data) -> action_user_blocked_me(Data);
blocked(cast, ?EVENT_USER_UNBLOCKED_ME, Data) -> action_user_unblocked_me(Data);
blocked(cast, ?EVENT_USER_ADDED_TO_SPAM_ME, Data) -> action_user_added_to_spam_me(Data);
blocked(Type, Event, Data) -> handle_event(Type, Event, Data).

i_am_blocked({call, From}, ?EVENT_DELETE, Data) -> action_delete_contact(From, Data);
i_am_blocked({call, From}, ?EVENT_BLOCK, Data) -> action_block(From, Data);
i_am_blocked({call, From}, ?EVENT_ADD_TO_SPAM, Data) -> action_add_to_spam(From, Data);
i_am_blocked(cast, ?EVENT_USER_UNBLOCKED_ME, Data) -> action_user_unblocked_me(Data);
i_am_blocked(cast, ?EVENT_USER_ADDED_TO_SPAM_ME, Data) -> action_user_added_to_spam_me(Data);
i_am_blocked(Type, Event, Data) -> handle_event(Type, Event, Data).

added_to_spam({call, From}, ?EVENT_DELETE, Data) -> action_delete_contact(From, Data);
added_to_spam(Type, Event, Data) -> handle_event(Type, Event, Data).

i_am_added_to_spam({call, From}, ?EVENT_DELETE, Data) -> action_delete_contact(From, Data);
i_am_added_to_spam({call, From}, ?EVENT_ADD_TO_SPAM, Data) -> action_add_to_spam(From, Data);
i_am_added_to_spam(Type, Event, Data) -> handle_event(Type, Event, Data).

handle_event(Type, Event, Data) ->
  io:format("[~p] received unknown event ~p", [get_state(Data), Event]),
  case Type of
    {call, From} ->
      {keep_state, Data, [{reply, From, {error, denied}}]};
    cast ->
      {keep_state, Data}
  end.

%%% Actions

action_add_to_contact(From, Data = #data{user = User, contact = Contact}) ->
  NewContact = im_contact:upsert(Contact#im_contact{status = ?CONTACT_STATUS_FRIEND, friend=true}),
  Data1 = Data#data{contact = NewContact},

  UserId = User#im_usr.id,
  ContactUserId = im_common:parse_id(Contact#im_contact.userId),

  ExistingUserIds = im_roster_chat:list(UserId),
  case lists:member(ContactUserId, ExistingUserIds) of
    false ->
      im_roster_chat:worker(UserId),
      im_roster_chat:worker(ContactUserId),
      im_roster_chat:add(UserId, ContactUserId);
    true ->
      skip
  end,

  ContactUser = im_roster_chat:get(ContactUserId),

  case ContactUser#im_usr.sendBioToNewContacts =:= true andalso ContactUser#im_usr.bio =/= undefined of
    true ->
      MsgItem = #im_msg{
        type=?MESSAGE_TYPE_USER_MESSAGE,
        kind=?MESSAGE_KIND_TEXT,
        payload=ContactUser#im_usr.bio,
        origin=ContactUserId,
        recipient=UserId
      },
      im_message:send(ContactUserId, ?MESSAGE_FEED_TYPE_CHAT, UserId, MsgItem, true);
    false -> skip
  end,

  {next_state, get_state(Data1), Data1, [{reply, From, Data1#data.contact}]}.

action_user_add_to_contact_you(Data = #data{contact = Contact}) ->
  Contact1 = case Contact of
    #im_contact{status=?CONTACT_STATUS_UNKNOWN, friend=false} ->
      im_contact:upsert(Contact#im_contact{status = ?CONTACT_STATUS_PENDING});
    _ ->
      Contact
  end,
  Data1 = Data#data{contact = Contact1},

  {next_state, get_state(Data1), Data1}.

action_delete_contact(From, Data  = #data{user = User, targetUser = TargetUser, contact = Contact}) ->
  case Contact of
    #im_contact{friend = true} ->
      UpdatedContactStatus = case im_contact:find(TargetUser#im_usr.id, User#im_usr.id) of
        {ok, #im_contact{friend = true}} ->
          case Contact of
            #im_contact{status = ?CONTACT_STATUS_FRIEND} -> Contact#im_contact{status = ?CONTACT_STATUS_PENDING};
            _                                            -> Contact
          end;
        _ ->
          case Contact of
            #im_contact{status = ?CONTACT_STATUS_FRIEND} -> Contact#im_contact{status = ?CONTACT_STATUS_UNKNOWN};
            _                                            -> Contact
          end
      end,
      UpdatedContact = UpdatedContactStatus#im_contact{friend = false},
      Data1 = Data#data{contact = im_contact:upsert(UpdatedContact)},
      {next_state, get_state(Data1), Data1, [{reply, From, Data1#data.contact}]};
    _  ->
      {next_state, get_state(Data), Data, [{reply, From, {error, denied}}]}
  end.


action_user_deleted_me(Data = #data{contact = Contact}) ->
  Contact1 = case Contact of
    #im_contact{status = ?CONTACT_STATUS_PENDING, friend = false} ->
      im_contact:upsert(Contact#im_contact{status = ?CONTACT_STATUS_UNKNOWN});
    _ ->
      Contact
  end,
  Data1 = Data#data{contact = Contact1},

  {next_state, get_state(Data1), Data1}.

action_block(From, Data = #data{user = User, targetUser = TargetUser, contact = Contact}) ->
  Data1 = Data#data{contact = im_contact:upsert(Contact#im_contact{status = ?CONTACT_STATUS_BLOCKED})},

  im_message:send_sys_msg(TargetUser#im_usr.id, ?MESSAGE_FEED_TYPE_CHAT, User#im_usr.id, <<"system.you.block.user">>, [], true, User#im_usr.id),
  im_message:send_sys_msg(User#im_usr.id, ?MESSAGE_FEED_TYPE_CHAT, TargetUser#im_usr.id, <<"system.you.blocked.by.user">>, [im_message:markup_user(User#im_usr.id)], true, TargetUser#im_usr.id),
  user_block(User#im_usr.id, TargetUser#im_usr.id),

  {next_state, get_state(Data1), Data1, [{reply, From, Data1#data.contact}]}.

action_user_blocked_me(Data = #data{contact = Contact}) ->
  Contact1 = case Contact#im_contact.status of
    ?CONTACT_STATUS_UNKNOWN   -> im_contact:upsert(Contact#im_contact{status = ?CONTACT_STATUS_I_AM_BLOCKED});
    ?CONTACT_STATUS_PENDING   -> im_contact:upsert(Contact#im_contact{status = ?CONTACT_STATUS_I_AM_BLOCKED});
    ?CONTACT_STATUS_FRIEND    -> im_contact:upsert(Contact#im_contact{status = ?CONTACT_STATUS_I_AM_BLOCKED});
    _                         -> Contact
  end,
  Data1 = Data#data{contact = Contact1},

  {next_state, get_state(Data1), Data1}.

action_unblock(From, Data = #data{user = User, targetUser = TargetUser, contact = Contact}) ->
  Contact1 = case im_contact:find(TargetUser#im_usr.id, User#im_usr.id) of
    {ok, #im_contact{status = ?CONTACT_STATUS_BLOCKED}} ->
      Contact#im_contact{status = ?CONTACT_STATUS_I_AM_BLOCKED};
    {ok, #im_contact{friend = true}} ->
      user_unblock(User#im_usr.id, TargetUser#im_usr.id),
      case Contact#im_contact.friend =:= true of
        true -> Contact#im_contact{status = ?CONTACT_STATUS_FRIEND};
        false -> Contact#im_contact{status = ?CONTACT_STATUS_PENDING}
      end;
    {ok, #im_contact{friend = false}} ->
      im_message:send_sys_msg(User#im_usr.id, ?MESSAGE_FEED_TYPE_CHAT, TargetUser#im_usr.id, <<"system.user.unblock.you">>, [im_message:markup_user(User#im_usr.id)], true, TargetUser#im_usr.id),
      user_unblock(User#im_usr.id, TargetUser#im_usr.id),
      case Contact#im_contact.friend =:= true of
        true -> Contact#im_contact{status = ?CONTACT_STATUS_FRIEND};
        false -> Contact#im_contact{status = ?CONTACT_STATUS_UNKNOWN}
      end
  end,
  im_message:send_sys_msg(TargetUser#im_usr.id, ?MESSAGE_FEED_TYPE_CHAT, User#im_usr.id, <<"system.you.unblock.user">>, [im_message:markup_user(TargetUser#im_usr.id)], true, User#im_usr.id),
  Data1 = Data#data{contact = im_contact:upsert(Contact1)},
  {next_state, get_state(Data1), Data1, [{reply, From, Data1#data.contact}]}.

action_user_unblocked_me(Data = #data{user = User, targetUser = TargetUser, contact = Contact}) ->
  Contact1 = case Contact#im_contact.status of
    ?CONTACT_STATUS_BLOCKED ->
      Contact;
    _ ->
      UpdatedContact = case Contact#im_contact.friend of
        false ->
          case im_contact:find(TargetUser#im_usr.id, User#im_usr.id) of
            {ok, #im_contact{status = ?CONTACT_STATUS_FRIEND}}  -> Contact#im_contact{status = ?CONTACT_STATUS_PENDING};
            _                                                   -> Contact#im_contact{status = ?CONTACT_STATUS_UNKNOWN}
          end;
        true  ->
          Contact#im_contact{status = ?CONTACT_STATUS_FRIEND}
      end,
      im_message:send_sys_msg(TargetUser#im_usr.id, ?MESSAGE_FEED_TYPE_CHAT, User#im_usr.id, <<"system.user.unblock.you">>, [im_message:markup_user(TargetUser#im_usr.id)], true, User#im_usr.id),
      UpdatedContact
  end,
  im_contact:upsert(Contact1),
  Data1 = Data#data{contact = Contact1},

  {next_state, get_state(Data1), Data1}.

action_add_to_spam(From, Data = #data{user = User,  targetUser = TargetUser, contact = Contact}) ->
  Contact1 = case Contact#im_contact.status =/= ?CONTACT_STATUS_SPAMMER of
    true ->
      UpdatedContact = im_contact:upsert(Contact#im_contact{status = ?CONTACT_STATUS_SPAMMER}),
      UpdatedTargetUser = case TargetUser of
        #im_usr{spamCount = undefined} -> TargetUser#im_usr{spamCount = 1};
        _                               -> TargetUser#im_usr{spamCount=TargetUser#im_usr.spamCount+1}
      end,
      ctail:put(UpdatedTargetUser),
      im_message:send_sys_msg(User#im_usr.id, ?MESSAGE_FEED_TYPE_CHAT, TargetUser#im_usr.id, <<"system.you.cannot.write.to.user.anymore">>, [im_message:markup_user(User#im_usr.id)], true, TargetUser#im_usr.id),
      case im_chatupdate:find(User#im_usr.id, im_roster_chat:get(TargetUser#im_usr.id)) of
        {ok, Update1} -> im_chatupdate:remove(Update1);
        {error, not_found} -> skip
      end,
      case im_chatupdate:find(TargetUser#im_usr.id, im_roster_chat:get(User#im_usr.id)) of
        {ok, Update2} -> im_chatupdate:remove(Update2);
        {error, not_found} -> skip
      end,

      user_block(User#im_usr.id, TargetUser#im_usr.id),

      UpdatedContact;
    false ->
      Contact
  end,
  Data1 = Data#data{contact = Contact1},

  {next_state, get_state(Data1), Data1, [{reply, From, Data1#data.contact}]}.

action_user_added_to_spam_me(Data = #data{contact = Contact}) ->
  Contact1 = case Contact#im_contact.status =/= ?CONTACT_STATUS_SPAMMER of
    true  -> im_contact:upsert(Contact#im_contact{status = ?CONTACT_STATUS_I_AM_SPAMMER});
    false -> Contact
  end,
  Data1 = Data#data{contact = Contact1},

  {next_state, get_state(Data1), Data1}.

%%% PRIVATE FUNCTIONS

get_state(#data{contact = #im_contact{status = Status}}) ->
  case Status of
    ?CONTACT_STATUS_UNKNOWN      -> unknown_contact;
    ?CONTACT_STATUS_PENDING      -> pending_invitation;
    ?CONTACT_STATUS_FRIEND       -> friend;
    ?CONTACT_STATUS_BLOCKED      -> blocked;
    ?CONTACT_STATUS_I_AM_BLOCKED -> i_am_blocked;
    ?CONTACT_STATUS_SPAMMER      -> added_to_spam;
    ?CONTACT_STATUS_I_AM_SPAMMER -> i_am_added_to_spam
  end.

user_block(UserId, TargetUserId) ->
  Fun = fun(User=#im_usr{blocks=BlockedIds}) ->
    BlockedIds1 = case BlockedIds =:= undefined of
      true -> [];
      false -> BlockedIds
    end,
    BlockUserId = case User#im_usr.id of
      UserId       -> TargetUserId;
      TargetUserId -> UserId
    end,
    case lists:member(BlockUserId, BlockedIds1) of
      true -> {ok, User};
      false -> {ok, User#im_usr{blocks=im_common:list_unique([BlockUserId|BlockedIds1])}}
    end
  end,

  im_roster_chat:execute(TargetUserId, Fun),
  im_roster_chat:execute(UserId, Fun).

user_unblock(UserId, TargetUserId) ->
  Fun = fun(User=#im_usr{blocks=BlockedIds}) ->
    BlockedIds1 = case BlockedIds =:= undefined of
      true -> [];
      false -> BlockedIds
    end,

    UnblockUserId = case User#im_usr.id of
      UserId       -> TargetUserId;
      TargetUserId -> UserId
    end,

    case lists:member(UnblockUserId, BlockedIds1) of
      true -> {ok, User#im_usr{blocks=lists:filter(fun(UId) -> UId =/= UnblockUserId end, BlockedIds1)}};
      false -> {ok, User}
    end
  end,

  im_roster_chat:execute(TargetUserId, Fun),
  im_roster_chat:execute(UserId, Fun).
