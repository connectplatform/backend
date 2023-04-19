-module(im_device).

-include("im_common.hrl").

-export([get/1, find/2, add/5, remove/2, remove_all/1]).
-export([get_push_tokens/1, add_push_token/4, remove_push_token/1, remove_push_token/2, filter_push_tokens/3]).
-export([format_devices/1, parse_devices/1]).

get(UserId) ->
  User = parse_devices(im_roster_chat:get(UserId)),
  User#im_usr.devices.

find(UserId, DeviceId) ->
  DeviceId1 = im_common:format_utf8(DeviceId),
  DevicesFiltered = lists:filter(fun(#im_device{id=DevId}) -> DevId =:= DeviceId1 end, im_device:get(UserId)),
  case DevicesFiltered of
    [Device] -> Device;
    _ -> undefined
  end.

add(UserId, Token, OS, DeviceId, DeviceName) ->
  im_logger:info(UserId, "[Device] Add device. OS: ~p, DeviceId: ~p, DeviceName: ~p", [OS, DeviceId, DeviceName]),

  im_roster_chat:execute(UserId, fun(User) ->
    DeviceId1 = im_common:format_utf8(DeviceId),
    NewUser = case device_exists(DeviceId1, User) of
      true -> User;
      false ->
        NewDevice = #im_device{id=DeviceId1, token=Token, os=OS, name=DeviceName},
        User#im_usr{devices=[NewDevice|User#im_usr.devices]}
    end,
    {ok, NewUser}
  end).

remove(UserId, DeviceId) ->
  im_logger:info(UserId, "[Device] Remove device DeviceId: ~p", [DeviceId]),

  Device = find(UserId, DeviceId),
  case Device =:= undefined of
    true -> {error, not_found};
    false ->
      im_user_state:checkout(UserId, DeviceId),

      ctail:delete(im_device_locale_lookup, im_common:format_utf8(DeviceId)),
      ctail:delete(im_usr_token, Device#im_device.token),

      im_roster_chat:execute(UserId, fun(User) ->
        lists:foreach(fun(#im_device_push_token{id=Token}) ->
          ctail:delete(im_device_push_token_lookup, Token)
        end, get_push_tokens(User, Device#im_device.id)),

        {ok, remove_device(DeviceId, User)}
      end)
  end.

remove_all(UserId) ->
  [remove(UserId, DeviceId) || #im_device{id=DeviceId} <- im_device:get(UserId)].

get_push_tokens(User) ->
  get_push_tokens(User, undefined).
get_push_tokens(User=#im_usr{}, DeviceId) ->
  User1 = parse_devices(User),
  lists:foldl(fun(#im_device{id=DevId, tokens=Tokens}, Acc) ->
    case DeviceId =:= undefined orelse DevId =:= DeviceId of
      true -> Acc ++ Tokens;
      false -> Acc
    end
  end, [], User1#im_usr.devices);
get_push_tokens(UserId, _) ->
  get_push_tokens(im_roster_chat:get(UserId)).

add_push_token(UserId, DeviceId, TokenType, Token) ->
  Fun = fun(User) ->
    {ok, add_token(DeviceId, TokenType, Token, User)}
  end,
  im_roster_chat:execute(UserId, Fun).

remove_push_token(Token) ->
  case ctail:get(im_device_push_token_lookup, im_common:format_utf8(Token)) of
    {ok, Lookup} ->
      UserId = Lookup#im_device_push_token_lookup.userId,
      remove_push_token(UserId, Token);
    _ -> skip
  end.

remove_push_token(UserId, Token) ->
  im_logger:info(UserId, "[Device] Remove push token: ~p", [Token]),

  im_roster_chat:execute(UserId, fun(User) ->
    {ok, remove_token(Token, User)}
  end).

filter_push_tokens(User=#im_usr{}, OS, Type) ->
  Tokens = get_push_tokens(User),
  Result = [Token || Token <- Tokens, Token#im_device_push_token.os =:= OS, Token#im_device_push_token.type =:= Type],
  % im_logger:debug(User#im_usr.id, "[Device] Get push tokens. OS: ~p, Type: ~p, Result: ~p, All: ~p", [OS, Type, Result, Tokens]),
  Result;
filter_push_tokens(UserId, OS, Type) ->
  filter_push_tokens(im_roster_chat:get(UserId), OS, Type).

device_exists(DeviceId, #im_usr{devices=Devices}) ->
  DeviceId1 = im_common:format_utf8(DeviceId),
  DevicesFiltered = [Device || Device <- Devices, Device#im_device.id =:= DeviceId1],
  DevicesFiltered =/= [].

remove_device(DeviceId, User=#im_usr{devices=Devices}) ->
  DeviceId1 = im_common:format_utf8(DeviceId),
  Devices1 = [Device || Device <- Devices, Device#im_device.id =/= DeviceId1],
  User#im_usr{devices=Devices1}.

add_token(DeviceId, TokenType, Token, User=#im_usr{id=UserId, devices=Devices}) ->
  DeviceId1 = im_common:format_utf8(DeviceId),
  Token1 = im_common:format_utf8(Token),
  case device_exists(DeviceId, User) of
    true ->
      case token_exists(Token, User) of
        true ->
          im_logger:info(UserId, "[Device] Token already exists: ~p", [Token]),
          User;
        false ->
          Devices1 = lists:map(fun(Device=#im_device{id=DevId, os=OS, tokens=Tokens}) ->
            case DevId =:= DeviceId1 of
              true ->
                ctail:put(#im_device_push_token_lookup{id=Token1, userId=UserId}),
                DeviceToken = #im_device_push_token{id=Token1, os=OS, type=TokenType},
                Device#im_device{tokens=Tokens ++ [DeviceToken]};
              false -> Device
            end
          end, Devices),
          im_logger:info(UserId, "[Device] Add token ~p of type ~p. New devices: ~p", [Token, TokenType, Devices1]),
          User#im_usr{devices=Devices1}
      end;
    false ->
      im_logger:error(UserId, "[Device] Trying to add push token to unknown device: ~p", [DeviceId]),
      User
  end.

token_exists(Token, #im_usr{devices=Devices}) ->
  Token1 = im_common:format_utf8(Token),
  DevicesFiltered = lists:filter(fun(#im_device{tokens=Tokens}) ->
    TokensFiltered = lists:filter(fun(#im_device_push_token{id=Id}) ->
      Id =:= Token1
    end, Tokens),
    TokensFiltered =/= []
  end, Devices),
  DevicesFiltered =/= [].

remove_token(Token, User=#im_usr{id=UserId, devices=Devices}) ->
  Token1 = im_common:format_utf8(Token),
  Devices1 = lists:map(fun(Device=#im_device{tokens=Tokens}) ->
    FoundToken = lists:any(fun(#im_device_push_token{id=Id}) -> Id =:= Token1 end, Tokens),
    NewTokens = case FoundToken of
      true ->
        lists:foreach(fun(#im_device_push_token{id=Id}) ->
          ctail:delete(im_device_push_token_lookup, Id)
        end, Tokens),
        [];
      false -> Tokens
    end,
    Device#im_device{tokens=NewTokens}
  end, Devices),
  im_logger:info(UserId, "[Device] Removed token ~p. New devices: ~p", [Token, Devices1]),
  User#im_usr{devices=Devices1}.

parse_devices(User=#im_usr{devices={<<"tuple_list">>, _}}) -> User#im_usr{devices=[]}; %% fallback for old data in db
parse_devices(User=#im_usr{devices=undefined}) -> User#im_usr{devices=[]};
parse_devices(User=#im_usr{devices=Devices}) ->
  DevicesParsed = lists:foldl(fun(Device, Acc) ->
    case parse_device(Device) of
      undefined    -> Acc;
      DeviceParsed -> [DeviceParsed|Acc]
    end
  end, [], Devices),
  User#im_usr{devices=DevicesParsed}.

parse_device(Device=#im_device{}) -> Device;
parse_device({<<"device">>, DeviceProps}) ->
  OS1 = case proplists:get_value(<<"os">>, DeviceProps) of
    undefined -> undefined;
    OS -> round(OS)
  end,
  #im_device{id=proplists:get_value(<<"id">>, DeviceProps),
    token=proplists:get_value(<<"token">>, DeviceProps),
    name=proplists:get_value(<<"name">>, DeviceProps),
    os=OS1,
    tokens=[parse_token(Token) || Token <- proplists:get_value(<<"tokens">>, DeviceProps)]};
parse_device(_) -> undefined.

format_devices(User=#im_usr{devices=undefined}) -> User#im_usr{devices=[]};
format_devices(User=#im_usr{devices=Devices}) ->
  User#im_usr{devices=[format_device(Device) || Device <- Devices]}.

format_device(Device={<<"device">>, _}) -> Device;
format_device(#im_device{id=Id, token=Token, name=Name, os=OS, tokens=Tokens}) ->
  OS1 = case OS =/= undefined of
    true -> round(OS);
    false -> undefined
  end,
  {<<"device">>, [
    {<<"id">>, im_common:format_utf8(Id)},
    {<<"name">>, im_common:format_utf8(Name)},
    {<<"token">>, im_common:format_utf8(Token)},
    {<<"os">>, OS1},
    {<<"tokens">>, [format_token(T) || T <- Tokens]}
  ]}.

format_token(Token={<<"token">>, _}) -> Token;
format_token(#im_device_push_token{id=Id, os=OS, type=Type}) ->
  {<<"token">>, [
    {<<"id">>, im_common:format_utf8(Id)},
    {<<"os">>, round(OS)},
    {<<"type">>, round(Type)}
  ]}.

parse_token(Token=#im_device_push_token{}) -> Token;
parse_token({<<"token">>, TokenProps}) ->
  #im_device_push_token{id=im_common:format_utf8(proplists:get_value(<<"id">>, TokenProps)),
    os=round(proplists:get_value(<<"os">>, TokenProps)),
    type=round(proplists:get_value(<<"type">>, TokenProps))}.
