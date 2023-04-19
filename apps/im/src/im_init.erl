-module(im_init).

-include("im_common.hrl").

-export([authenticate/2, initialize/3, terminate/1]).

authenticate(Token, Locale) ->
  LocaleToUse = im_locale:ensure_locale(Locale),
  case im_auth:find_token_by_id(Token) of
    undefined ->
      {error, token_not_found};
    TokenRec ->
      TokenToUse = TokenRec#im_usr_token{locale=LocaleToUse},
      User = im_roster_chat:get(TokenRec#im_usr_token.userId),
      ctail:put(TokenRec),
      im_locale:set(TokenRec#im_usr_token.deviceId, LocaleToUse),
      {ok, User, TokenToUse}
  end.

initialize(_, #im_usr_token{deviceId=undefined}, _) -> {shutdown, 400};
initialize(User=#im_usr{id=UserId}, TokenRec=#im_usr_token{id=Token, os=OS, deviceId=DeviceId, deviceName=DeviceName}, Version) ->
  ok = im_user_state:checkin(websocket, TokenRec),
  im_roster_chat:worker(UserId),
  ok = im_device:add(UserId, Token, OS, DeviceId, DeviceName),
  im_user_settings:init(UserId),

  self() ! authenticated,

  {ok, #im_state{userId=UserId, user=User, token=TokenRec, version=Version}}.

terminate(UserId) ->
  im_user_state:checkout(UserId, self),

  im_roster_chat:execute(UserId, fun(User) ->
    {ok, User#im_usr{lastSeen=sm:now()}}
  end).
