-module(im_usr).

-include("im_common.hrl").

-export([get/3, update/3, validate_user/1]).
-export([status/2, statuses/2, get_status/1, get_last_seen/1]).
-export([block_user_by_system/1, unblock_user_by_system/1]).
-export([get_vendor_users/2]).

get(#'User'{ref=Ref}, User, #im_usr_token{os=OS, deviceName=DeviceName, deviceId=DeviceId})->
  Resp = #'UserResp'{
    ref=Ref,
    os=OS,
    user=im_dto:format_user(User),
    deviceId=DeviceId,
    deviceName=DeviceName
  },
  {ok, Resp}.

update(#'UpdateUser'{ref=Ref, user=UserEntity}, User, #im_usr_token{deviceName=DeviceName, deviceId=DeviceId}) ->
  case validate_user(UserEntity) of
    {error, name_empty} ->
      {ok, #'ErrorResp'{
        code=?ERROR_CODE_USER_NAME_REQUIRED,
        ref=Ref, messageParams=[],
        messageType= <<"user.name.is.required">>,
        message=im_common:format_utf8(im_trans:t(<<"user.name.is.required">>))}};
    {error, name_length} ->
      {ok, #'ErrorResp'{
        code=?ERROR_CODE_USER_NAME_NOT_VALID,
        ref=0, messageParams=[],
        messageType= <<"user.length.of.name.is.not.valid">>,
        message=im_common:format_utf8(im_trans:t(<<"user.length.of.name.is.not.valid">>))}};
    {ok, valid} ->
      UserToChange = case UserEntity#'UserEntity'.id of
        undefined -> User;
        UserId -> im_roster_chat:get(UserId)
      end,

      Roles1 = case im_acl:has_role("super_admin", UserToChange#im_usr.id) of
        true  -> [<<"super_admin">>];
        false ->
          case is_list(UserEntity#'UserEntity'.roles) of
            true ->
              lists:filter(fun(Role) ->
                im_acl:has_role("super_admin", User#im_usr.id) orelse
                im_acl:has_perm("assign_role_" ++ im_common:ensure_list(Role), User#im_usr.id)
              end, UserEntity#'UserEntity'.roles);
            false -> []
          end
      end,
      Roles2 = [im_common:format_utf8(Role) || Role <- Roles1],

      NotMyProfile = UserToChange#im_usr.id =/= User#im_usr.id,
      case NotMyProfile andalso im_acl:has_perm("manage_users", User#im_usr.id) =:= false of
        true ->
          case Roles1 =:= [] orelse Roles1 =:= [<<"super_admin">>] of
            true ->
              {ok, #'ErrorResp'{ref=Ref, code=?ERROR_CODE_PERMISSION_DENIED}};
            false ->
              UpdatedUser = im_roster_chat:execute(User#im_usr.id, fun(User1) ->
                User2 = User1#im_usr{roles=im_common:list_unique(UserToChange#im_usr.roles ++ Roles2)},
                {User2, User2}
              end),
              {ok, #'UserResp'{
                ref=Ref,
                user=im_dto:format_user(UpdatedUser),
                deviceId=DeviceId,
                deviceName=DeviceName
              }}
          end;
        false ->
          UpdatedUser = im_roster_chat:execute(User#im_usr.id, fun(User1) ->
            User2 = User1#im_usr{
              name      = im_common:format_utf8(UserEntity#'UserEntity'.name),
              photo     = im_common:format_utf8(UserEntity#'UserEntity'.photo),
              thumbnail = im_common:format_utf8(UserEntity#'UserEntity'.thumbnail),
              data      = im_common:json_to_mongo(UserEntity#'UserEntity'.data),
              excludeMe = im_common:ensure_boolean(UserEntity#'UserEntity'.excludeMe),
              bio       = im_common:format_utf8(UserEntity#'UserEntity'.bio),
              sendBioToNewContacts = UserEntity#'UserEntity'.sendBioToNewContacts,
              updatedAt = sm:now()
            },
            User3 = case NotMyProfile of
              true ->
                User2#im_usr{
                  roles     = Roles2,
                  active    = UserEntity#'UserEntity'.active,
                  isVendor  = UserEntity#'UserEntity'.isVendor,
                  vendorId  = im_common:parse_id(UserEntity#'UserEntity'.vendorId)
                };
              false ->
                User2
            end,
            {User3, User3}
          end),

          im_event:fire(?USER_UPDATED_EVENT, UpdatedUser),

          case im_directory:is_turnedon() of
            true  -> im_directory:update_directory_avatar(UpdatedUser);
            false -> skip
          end,

          case UpdatedUser#im_usr.isNew == true of
            false -> im_contact:update_contacts_when_user_changed(UpdatedUser);
            _     -> skip
          end,

          Resp = #'UserResp'{
            ref=Ref,
            user=im_dto:format_user(UpdatedUser),
            deviceId=DeviceId,
            deviceName=DeviceName
          },

          ToUserIds = case NotMyProfile =:= true of
            true -> [User#im_usr.id, UserToChange#im_usr.id];
            false -> [User#im_usr.id]
          end,
          im_user_state:broadcast(undefined, ToUserIds, #'ProfileChanged'{deviceId=DeviceId, user=im_dto:format_user(UpdatedUser)}),

          im_bot_update:send_system_message({user_updated, UpdatedUser}, UpdatedUser#im_usr.id),

          lists:foreach(fun (Pid) ->
            Pid ! {user_changed, UpdatedUser}
          end, im_user_state:pids(UpdatedUser#im_usr.id)),

          {ok, Resp}
      end
  end.

validate_user(UserEntity=#'UserEntity'{}) ->
  case UserEntity#'UserEntity'.name =/= undefined of
    true ->
      NameLength = im_common:length_utf8(UserEntity#'UserEntity'.name),
      case (NameLength =< 40) and (NameLength > 0) of
        true ->
          {ok, valid};
        false ->
          {error, name_length}
      end;
    false ->
      {error, name_empty}
  end.

status(#'Status'{ref=Ref, userId=UserId}, _UserId) ->
  UserId1 = im_common:parse_id(UserId),
  StatusEntity = #'UserStatusEntity'{userId=UserId, status=get_status(UserId1), lastSeen=get_last_seen(UserId1)},
  #'StatusResp'{ref=Ref, status=StatusEntity}.

statuses(#'Statuses'{ref=Ref}, UserId) ->
  Statuses = lists:map(fun(FriendUserId) ->
    #'UserStatusEntity'{userId=im_common:format_id(FriendUserId), status=get_status(FriendUserId), lastSeen=get_last_seen(FriendUserId)}
  end, im_contact:get_friends_ids(UserId)),
  #'StatusesResp'{ref=Ref, statuses=Statuses}.

get_status(UserId) ->
  case im_user_state:pids(UserId) of
    [] -> ?USER_STATUS_OFFLINE;
    _  -> ?USER_STATUS_ONLINE
  end.

get_last_seen(UserId) ->
  case im_roster_chat:get(UserId) of
    undefined -> 0;
    User -> User#im_usr.lastSeen
  end.

block_user_by_system(#'BlockUserBySystem'{ref=Ref, userId=UserId}) ->
  case im_roster_chat:get(UserId) of
    {ok, User} ->
      ctail:put(im_user_transform:serialize(User#im_usr{active=false})),
      lists:foreach(fun(ContactId) ->
        case ctail:get(im_contact, ContactId) of
          {ok, Contact}      -> im_contact:upsert(Contact#im_contact{blockedBySystem=true});
          {error, not_found} -> skip
        end
      end, im_contact:get_contact_ids_by_phone(User#im_usr.phone)),

      im_device:remove_all(UserId),
      im_user_state:broadcast(undefined, [UserId], #'UserBlockedBySystem'{}),

      {ok, {}};
    {error, not_found} ->
      {ok, #'ErrorResp'{
        code=?ERROR_CODE_NOT_FOUND,
        ref=Ref, messageParams=[],
        messageType= <<"auth.user.not.found">>,
        message=im_common:format_utf8(im_trans:t(<<"auth.user.not.found">>))}}
  end.

unblock_user_by_system(#'UnBlockUserBySystem'{ref=Ref, userId=UserId}) ->
  case im_roster_chat:get(UserId) of
    undefined ->
      {ok, #'ErrorResp'{
        code=?ERROR_CODE_NOT_FOUND,
        ref=Ref, messageParams=[],
        messageType= <<"auth.user.not.found">>,
        message=im_common:format_utf8(im_trans:t(<<"auth.user.not.found">>))}};
    User ->
      ctail:put(im_user_transform:serialize(User#im_usr{active=true})),
      lists:foreach(fun(ContactId) ->
        case ctail:get(im_contact, ContactId) of
          {ok, Contact}      -> im_contact:upsert(Contact#im_contact{blockedBySystem=false});
          {error, not_found} -> skip
        end
      end, im_contact:get_contact_ids_by_phone(User#im_usr.phone)),
      {ok, {}}
  end.

get_vendor_users(#'VendorUsers'{ref=Ref}, User=#im_usr{}) ->
  Users = case im_acl:has_role("super_admin", User#im_usr.id)
    andalso im_common:parse_id(User#im_usr.vendorId) =:= undefined
    andalso User#im_usr.isVendor =/= true of
    true ->
      Contacts = case ctail_feed:get(im_contact, {<<"contacts">>, im_common:parse_id(User#im_usr.id)}, -1) of
        undefined -> [];
        Result -> Result
      end,
      lists:foldl(fun(Contact=#im_contact{}, Acc) ->
        case ctail:get(im_usr, im_common:parse_id(Contact#im_contact.userId)) of
          {ok, ContactUser} -> [ContactUser|Acc];
          _ -> Acc
        end
      end, [], Contacts);
    false ->
      case im_common:parse_id(User#im_usr.vendorId) =/= undefined of
        true -> ctail_mongo:find(im_usr, {<<"vendorId">>, im_common:parse_id(User#im_usr.vendorId)}, 0, 999);
        _ -> []
      end
  end,
  #'VendorUsersResp'{ref=Ref, users=[im_dto:format_user(User) || User <- Users]}.

