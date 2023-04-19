-module(im_push).

-include("im_common.hrl").

-export([send/3, send/4]).
-export([send_json/3, send_json/4]).

send(UserOrUserId, OS, Message) ->
  send(UserOrUserId, OS, Message, ?PUSH_TOKEN_TYPE_NORMAL).
send(User=#im_usr{}, OS, Message, Type) ->
  case im:is_debug() of
    true -> skip;
    false ->
      Tokens = im_device:filter_push_tokens(User, OS, Type),
      case Tokens =/= [] of
        true -> im_logger:debug(User#im_usr.id, "[Push] Send to FCM: ~p, tokens: ~p", [Message, Tokens]);
        false -> skip
      end,

      lists:foreach(fun(#im_device_push_token{id=Token}) ->
        im_fcm_pool:send(User#im_usr.id, Token, Message)
      end, Tokens)
  end;
send(UserId, OS, Message, Type) ->
  send(im_roster_chat:get(UserId), OS, Message, Type).

send_json(UserId, OS, Message) ->
  send_json(UserId, OS, Message, ?PUSH_TOKEN_TYPE_NORMAL).
send_json(UserId, OS, Message, Type) ->
  send(UserId, OS, Message, Type).
