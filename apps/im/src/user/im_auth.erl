-module(im_auth).

-include("im_common.hrl").

-export([social/1, request_verification/1, confirm_verification/1, logout/3]).
-export([ensure_user_by_phone/1, create_token/5, find_token_by_id/1]).

social(#'SocialAuth'{ref=Ref, network=Network, accessToken=AccessToken, os=OS, deviceId=DeviceId, deviceName=DeviceName}) ->
  case Network of
    ?SOCIAL_NETWORK_FACEBOOK ->
      Response = case im:is_debug() of
        true ->
          {ok, {{undefined, 200, undefined}, undefined, "{\"id\": \"facebook_user_id\"}"}};
        false ->
          URL = "https://graph.facebook.com/me?fields=id,name,email&access_token=" ++ AccessToken,
          Header = [],
          HTTPOptions = [],
          Body = "{}",
          Options = [],
          httpc:request(post, {URL, Header, "application/json", Body}, HTTPOptions, Options)
      end,
      case Response of
        {ok, {{_, StatusCode, _}, _, Data}} ->
          case StatusCode =:= 200 of
            true ->
              {ok, {struct, Props}} = yaws_json2:decode_string(im_common:ensure_list(Data)),
              FacebookId = im_common:format_utf8(proplists:get_value("id", Props)),
              FacebookPhotoUrl = "https://graph.facebook.com/" ++ im_common:ensure_list(FacebookId) ++ "/picture",
              User = case ctail:get(im_lookup_facebook, im_common:ensure_binary(FacebookId)) of
                {ok, FbLookup} ->
                  im_roster_chat:get(FbLookup#im_lookup_facebook.userId);
                {error, _} ->
                  NowTime = sm:now(),
                  NewUser = #im_usr{id=ctail:next_id(),
                    name=im_common:ensure_binary(proplists:get_value("name", Props)),
                    email=im_common:ensure_binary(proplists:get_value("email", Props)),
                    thumbnail=FacebookPhotoUrl ++ "?type=large",
                    photo=FacebookPhotoUrl ++ "?type=large",
                    facebookId=FacebookId,
                    roles=[<<"customer">>],
                    createdAt=NowTime,
                    updatedAt=NowTime,
                    active=true,
                    isNew=false},
                  NewUserFbLookup = #im_lookup_facebook{id=FacebookId, userId=NewUser#im_usr.id},
                  ok = ctail:put(NewUser),
                  ok = ctail:put(NewUserFbLookup),
                  NewUser
              end,
              TokenRecord = create_or_find_token(OS, DeviceId, DeviceName, User),
              % im_logger:debug(User#im_usr.id, "[Auth] Using token record: ~p", [TokenRecord]),
              im_event:fire(?USER_UPDATED_EVENT, User),
              finish_registration(User),
              im_user_state:broadcast(User#im_usr.id, [User#im_usr.id], #'AuthResp'{}, true, DeviceId), %% AuthResp
              {ok, #'SocialAuthResp'{ref=Ref, token=TokenRecord#im_usr_token.id}};
            false ->
              {ok, {struct, Props}} = yaws_json2:decode_string(im_common:ensure_list(Data)),
              {struct, ErrorProps} = proplists:get_value("error", Props),
              Message = proplists:get_value("message", ErrorProps),
              {ok, #'ErrorResp'{ref=Ref, code=?ERROR_CODE_AUTH_FAILED, message=Message}}
          end;
        _ ->
            {ok, #'ErrorResp'{ref=Ref, code=?ERROR_CODE_AUTH_FAILED, message="Got error response from social network API"}}
      end;
    _ ->
      {ok, #'ErrorResp'{ref=Ref, code=?ERROR_CODE_AUTH_FAILED, message="Unknown social network"}}
  end.

request_verification(#'RequestVerification'{phone=Phone, deviceType=DeviceType}) ->
  % im_logger:debug(undefined, "[Auth] Got request for ~p", [Phone]),

  FormatedPhone = im_common:format_phone_with_code(undefined, Phone),
  FormatedPhone2 = case FormatedPhone of
    {ok, FormatedPhone1} -> FormatedPhone1;
    _                    -> Phone
  end,

  case im_directory:is_turnedon() of
    false -> process_request_verification(Phone, FormatedPhone2, DeviceType);
    true  ->
      % im_logger:debug(undefined, "[Auth] Directory is enabled, checking if directory entry for ~p exists", [Phone]),
      DirectoryEntryExists = case im_directory:r({FormatedPhone2}) of
        #'DirectoryEntity'{} -> true;
        {err, _}             -> false;
        _                    -> false
      end,
      im_logger:debug(undefined, "[Auth] Request verification. Phone: ~p, apple user phone: ~p", [FormatedPhone2, ?APPLE_USER_PHONE]),
      case DirectoryEntryExists orelse (FormatedPhone2 =:= ?APPLE_USER_PHONE) of
        true  -> process_request_verification(Phone, FormatedPhone2, DeviceType);
        false ->
          % im_logger:debug(undefined, "[Auth] Returing error, directory entry for ~p doesn't exist", [Phone]),
          {ok, #'ErrorResp'{
            code=?ERROR_CODE_USER_NOT_REGISTERED,
            ref=0, messageParams=[],
            messageType= <<"auth.user.not.registered">>,
            message=im_common:format_utf8(im_trans:t(<<"auth.user.not.registered">>))}
          }
      end
  end.

process_request_verification(Phone, FormatedPhone2, _DeviceType) ->
  % im_logger:debug(undefined, "[Auth] Processing request verification message for ~p", [Phone]),

  UserRes = case ctail:get(im_usr_phone, im_common:format_lookup_value(FormatedPhone2)) of
    {ok, LookupPhone} ->
      case LookupPhone#im_usr_phone.userId of
        {UserId} when is_binary(UserId) ->
          case im_roster_chat:get({UserId}) of
            undefined -> {error, not_found};
            FoundUser -> {ok, FoundUser}
          end;
        _ ->
          {error, not_found}
      end;
    {error, not_found} -> {error, not_found}
  end,
  case UserRes of
    {ok, User} ->
      % im_logger:debug(undefined, "[Auth] User with phone ~p exists, checking ability to request verification", [Phone]),
      case User#im_usr.active =:= false of
        true ->
          % im_logger:debug(undefined, "[Auth] User with phone ~p is not active, sorry", [Phone]),
          {ok, #'ErrorResp'{
            code=?ERROR_CODE_BLOCKED_BY_SYSTEM,
            ref=0, messageParams=[],
            messageType= <<"auth.user.not.active">>,
            message=im_common:format_utf8(im_trans:t(<<"auth.user.not.active">>))}};
        false ->
          do_send(Phone)
      end;
    {error, not_found} ->
      % im_logger:debug(undefined, "[Auth] User with phone ~p is new", [Phone]),
      do_send(Phone)
  end.

can_send_sms(Phone) ->
  case ctail:get(im_code, Phone) of
    {ok, Record} ->
      sm:now() > Record#im_code.expired;
    {error, _} ->
      true
  end.

do_send(Phone) ->
  SaveTokenFun = fun(P, C) ->
    Record=#im_code{
      id   = P,
      code = C,
      expired = sm:now() + 1000 * 60 * 2 %% expire after 2 minutes
    },
    ctail:put(Record)
  end,

  case can_send_sms(Phone) of
    true ->
      case Phone =:= ?APPLE_USER_PHONE of
        true ->
          SaveTokenFun(Phone, ?APPLE_USER_SMS_CODE),
          {ok, #'RequestVerificationResp'{}};
        false ->
          case im:is_debug() of
            true  ->
              {ok, CountryCode} = im_common:get_county_code(Phone),
              Phone1 = list_to_binary("+" ++ integer_to_list(CountryCode) ++ binary_to_list(im_common:format_phone(Phone))),
              SaveTokenFun(Phone1, "111"),
              {ok, #'RequestVerificationResp'{}};
            false ->
              case im_common:get_county_code(Phone) of
                {error, invalid} ->
                  {ok, #'ErrorResp'{
                    code=?ERROR_CODE_INVALID_PHONE_NUMBER,
                    ref=0, messageParams=[],
                    messageType= <<"auth.invalid.phone.number">>,
                    message=im_common:format_utf8(im_trans:t(<<"auth.invalid.phone.number">>))}};
                {ok, CountryCode} ->
                  SmsCode = im_common:random_string(6, numbers),
                  Phone1 = list_to_binary("+" ++ integer_to_list(CountryCode) ++ binary_to_list(im_common:format_phone(Phone))),
                  im_sms:send(CountryCode, binary_to_list(Phone1), "Your Activation Code: " ++ SmsCode),
                  SaveTokenFun(Phone1, SmsCode),
                  {ok, #'RequestVerificationResp'{}}
              end
          end
      end;
    false ->
      % im_logger:debug(undefined, "[Auth] Wait before request verification again for ~p", [Phone]),

      {ok, #'ErrorResp'{code=?ERROR_CODE_SMS_SEND_FAILED,
        ref=0, messageParams=[],
        messageType= <<"auth.you.must.wait.before.send.sms.again">>,
        message=im_common:format_utf8(im_trans:t(<<"auth.you.must.wait.before.send.sms.again">>))}}
  end.

confirm_verification(#'ConfirmVerification'{phone=Phone, smsCode=SmsCode, os=OS, deviceId=DeviceId, deviceName=DeviceName}) ->
  IsValid = not lists:member(DeviceId, [undefined, <<>>]) andalso not lists:member(DeviceName, [undefined, <<>>])
    andalso (OS >= ?PLATFORM_IOS andalso OS =< ?PLATFORM_WEB) andalso is_integer(OS),
  case IsValid of
    false ->
      {ok, #'ErrorResp'{code=?ERROR_CODE_INVALID_MESSAGE}};
    true ->
      case im_common:get_county_code(Phone) of
        {error, invalid} ->
          {ok, #'ErrorResp'{code=?ERROR_CODE_INVALID_PHONE_NUMBER, messageParams=[],
            messageType= <<"auth.invalid.phone.number">>,
            message=im_common:format_utf8(im_trans:t(<<"auth.invalid.phone.number">>))}};
        {ok, CountryCode} ->
          Phone1 = list_to_binary("+" ++ integer_to_list(CountryCode) ++ im_common:format_phone(Phone)),
          case sms_conformity(Phone1, SmsCode) of
            true ->
              case ensure_user_by_phone(Phone1) of
                {ok, User} ->
                  case User#im_usr.active =:= true of
                    true ->
                      TokenRecord = create_or_find_token(OS, DeviceId, DeviceName, User),
                      case im_directory:is_turnedon() of
                        true ->
                          im_directory:update_directory(User#im_usr.id, Phone1),
                          im_directory:spawn_contacts(User#im_usr.id, TokenRecord#im_usr_token.deviceId);
                        _ -> skip
                      end,
                      case User#im_usr.isNew == true of
                        true -> finish_registration(User);
                        _    -> skip
                      end,
                      {ok, #'ConfirmVerificationResp'{token=TokenRecord#im_usr_token.id}};
                    false ->
                      {ok, #'ErrorResp'{code=?ERROR_CODE_BLOCKED_BY_SYSTEM,
                        ref=0, messageParams=[],
                        messageType= <<"auth.user.blocked.by.system">>,
                        message=im_common:format_utf8(im_trans:t(<<"auth.user.blocked.by.system">>))}}
                  end;
                {error, _} -> {ok, #'ErrorResp'{code=?ERROR_CODE_INVALID_PHONE_NUMBER,
                  ref=0, messageParams=[],
                  messageType= <<"auth.invalid.phone.number">>,
                  message=im_common:format_utf8(im_trans:t(<<"auth.invalid.phone.number">>))}}
              end;
            false ->
              {ok, #'ErrorResp'{code=?ERROR_CODE_SMS_CODE_NOT_CORRECT,
                ref=0, messageParams=[],
                messageType= <<"auth.sms.code.is.not.correct">>,
                message=im_common:format_utf8(im_trans:t(<<"auth.sms.code.is.not.correct">>))}}
          end
      end
  end.

logout(#'Logout'{ref=Ref}, #im_usr{id=UserId}, _) ->
  DeviceId = im_user_state:attr(UserId, deviceId),
  im_device:remove(UserId, DeviceId),
  self() ! logout,
  {ok, #'LogoutResp'{ref=Ref}}.

sms_conformity(Phone, SmsCode) ->
  case ctail:get(im_code, Phone) of
    {ok, Record} ->
      case(Record#im_code.code) of
        SmsCode ->
          ctail:delete(im_code, Record#im_code.id),
          true;
        _ ->
          false
      end;
    {error, _} ->
      false
  end.

ensure_user_by_phone(Phone) ->
  % im_logger:debug(undefined, "[Auth] Ensure user with phone ~p exists", [Phone]),
  case im_common:is_valid_phone(Phone) of
     true  ->
        FormattedPhone = im_common:format_lookup_value(Phone),
        EnsureLookupAndUserFun = fun(FormattedP) ->
          % im_logger:debug(undefined, "[Auth] Create new user with phone ~p", [Phone]),
          UserId = ctail:next_id(),
          ctail:put(#im_usr_phone{id=FormattedP, userId=UserId}),
          NowTime = sm:now(),
          User=#im_usr{id=UserId, phone=Phone, createdAt=NowTime, updatedAt=NowTime, roles=[<<"customer">>]},
          ok = ctail:put(im_user_transform:serialize(User)),
          im_event:fire(?USER_UPDATED_EVENT, User),
          {ok, User}
        end,
        case ctail:get(im_usr_phone, FormattedPhone) of
          {ok, Info} ->
            case im_roster_chat:get(Info#im_usr_phone.userId) of
              undefined -> EnsureLookupAndUserFun(FormattedPhone);
              User -> {ok, User}
            end;
          {error, not_found} ->
            EnsureLookupAndUserFun(FormattedPhone)
        end;
      false ->
        {error, invalid_phone_number}
  end.

create_or_find_token(OS, DeviceId, DeviceName, User) ->
  case find_token(DeviceId, User) of
    undefined -> create_token(OS, DeviceId, DeviceName, User);
    TokenRec  -> TokenRec
  end.

find_token(DeviceId, #im_usr{id=UserId}) ->
  Device = im_device:find(UserId, DeviceId),
  case Device =/= undefined of
    true ->
      case ctail:get(im_usr_token, Device#im_device.token) of
        {ok, TokenRec} -> TokenRec;
        {error, _}     -> undefined
      end;
    false -> undefined
  end.

find_token_by_id(Token) ->
  case ctail:get(im_usr_token, im_common:ensure_binary(Token)) of
    {ok, TokenRec} -> TokenRec;
    {error, _} -> undefined
  end.

create_token(OS, DeviceId, DeviceName, User) ->
  create_token(OS, DeviceId, DeviceName, User, undefined).
create_token(OS, DeviceId, DeviceName, User=#im_usr{id=UserId}, PreGeneratedToken) ->
  Token = case PreGeneratedToken =:= undefined of
    true -> list_to_binary(im_common:random_string(35, chars_and_numbers));
    false -> PreGeneratedToken
  end,
  TokenRecord=#im_usr_token{
    id=Token,
    userId=UserId,
    os=OS,
    deviceId=DeviceId,
    deviceName=DeviceName
  },
  ctail:put(TokenRecord),

  im_roster_chat:worker(UserId),
  ok = im_device:add(UserId, Token, OS, DeviceId, DeviceName),

  case User#im_usr.isBot =:= true of
    true -> skip;
    false -> im_message:send_from_sys_user(UserId, <<"system.you.register.new.device">>, [DeviceName])
  end,

  TokenRecord.

finish_registration(User=#im_usr{}) ->
  im_roster_chat:worker(User#im_usr.id),
  Contacts = im_contact:update_contacts_when_user_changed(User),
  lists:foreach(fun(Contact) ->
    {<<"contacts">>, UserId} = Contact#im_contact.feed_id,
    RegisteredUserId = Contact#im_contact.userId,
    ExistingUserIds = im_roster_chat:list(UserId),
    case lists:member(RegisteredUserId, ExistingUserIds) of
      false ->
        im_roster_chat:worker(UserId),
        im_roster_chat:worker(RegisteredUserId),
        im_roster_chat:add(UserId, RegisteredUserId),
        im_message:send_sys_msg(RegisteredUserId, ?MESSAGE_FEED_TYPE_CHAT, UserId, <<"system.friend.registered.in.radius">>, [], true, UserId);
      true ->
        skip
    end
  end, Contacts),

  im_roster_chat:execute(User#im_usr.id, fun(User1) ->
    {ok, User1#im_usr{isNew=false}}
  end),

  im_contact:add_contacts_after_user_registered(User),

  im_bot_update:send_system_message({user_registered, User}, User#im_usr.id),
  ok.
