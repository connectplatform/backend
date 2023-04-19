-module(im_user_state).

-include("im_common.hrl").

-export([checkin/2, checkout/2, attr/2, pids/1]).
-export([broadcast/3, broadcast/4, broadcast/5]).

checkin(Transport, #im_usr_token{id = Token, userId = UserId, os = OS, deviceName = DeviceName, deviceId = DeviceId, locale = Locale}) ->
  case Transport =:= http orelse Transport =:= websocket of
    true ->
      Socket = #{
        pid => self(),
        token => Token,
        transport => Transport,
        os => OS,
        deviceId => DeviceId,
        deviceName => DeviceName,
        locale => Locale,
        registered => sm:now()
      },
      ModifyFunc = fun(Result) ->
        ModifiedUserInfo = case Result of
          {UserId, ExistingUserInfo} ->
            NewSockets = maps:get(sockets, ExistingUserInfo) ++ [Socket],
            ExistingUserInfo#{sockets := NewSockets};
          _ ->
            #{userId => UserId, sockets => [Socket]}
        end,

        {UserId, ModifiedUserInfo}
      end,

      case im_ets:modify(im_user_ws, UserId, ModifyFunc) of
        {UserId, UserInfo} ->
%%          io:format("~n~n[im_user_state:checkin][~p] in:{~p}:in - ~p ~n~n", [UserId, self(), pids(UserId)]),
          case maps:get(sockets, UserInfo) =:= [Socket] of
            true ->
              Msg = #'UserStatusChanged'{status = #'UserStatusEntity'{
                userId = im_common:format_id(UserId),
                status = ?USER_STATUS_ONLINE,
                lastSeen = sm:now()
              }},
              FriendsIds = im_contact:get_friends_ids(UserId),
              broadcast(UserId, FriendsIds, Msg);
            false ->
              skip
          end,
          ok;
        _ ->
          ok
      end;
    false ->
      im_logger:error(undefined, "[UserWS] Can't register, wrong transport: ~p", [Transport]),
      ok
  end.

checkout(UserId, Strategy) ->
  Pid = self(),
  ModifyFunc = fun(Result) ->
    case Result of
      {UserId, ExistingUserInfo} ->
        Sockets = maps:get(sockets, ExistingUserInfo),
        NewSockets = case Strategy of
          self ->
            lists:filter(fun(Socket) ->
              maps:get(pid, Socket) =/= Pid
            end, Sockets);
          other ->
            lists:filter(fun(Socket) ->
              maps:get(pid, Socket) =:= Pid
            end, Sockets);
          DeviceId ->
            lists:filter(fun(Socket) ->
              maps:get(deviceId, Socket) =/= DeviceId
            end, Sockets)
        end,

        {UserId, ExistingUserInfo#{sockets := NewSockets}};
      _ ->
        skip
    end
  end,

  case im_ets:modify(im_user_ws, UserId, ModifyFunc) of
    {UserId, UserInfo} ->
%%      io:format("~n~n[im_user_state:checkout][~p] out:{~p}:out - ~p ~n~n", [UserId, Pid, pids(UserId)]),
      case UserInfo =/= undefined andalso maps:get(sockets, UserInfo) =:= [] of
        true ->
          Msg = #'UserStatusChanged'{status = #'UserStatusEntity'{
            userId = im_common:format_id(UserId),
            status = ?USER_STATUS_OFFLINE,
            lastSeen = sm:now()
          }},
          FriendsIds = im_contact:get_friends_ids(UserId),
          broadcast(UserId, FriendsIds, Msg);
        false ->
          skip
      end;
    _ ->
      skip
  end.

%% utils

attr(undefined, _) -> undefined;
attr(UserId, Name) -> attr(UserId, self(), Name).
attr(UserId, Pid, Name) ->
  case im_ets:get_one(im_user_ws, UserId) of
    {UserId, ExistingUserInfo} ->
      Filtered = lists:filter(fun(Socket) ->
        maps:get(pid, Socket) =:= Pid
      end, maps:get(sockets, ExistingUserInfo)),

      case Filtered of
        [Socket | _] -> maps:get(Name, Socket, undefined);
        _ -> undefined
      end;
    _ ->
      undefined
  end.

pids(UserId) -> pids(UserId, undefined).
pids(UserId, Transport) ->
%%  io:format("PIDS [~p]: ~p~n", [UserId, im_ets:get_one(im_user_ws, UserId)]),
  case im_ets:get_one(im_user_ws, UserId) of
    {UserId, ExistingUserInfo} ->
      Filtered = lists:filter(fun(Socket) ->
        Transport =:= undefined orelse Transport =:= maps:get(transport, Socket)
      end, maps:get(sockets, ExistingUserInfo)),
      [maps:get(pid, Socket) || Socket <- Filtered];
    _ ->
      []
  end.

broadcast(UserId, ToUserIds, Msg) -> broadcast(UserId, ToUserIds, Msg, false).
broadcast(UserId, ToUserIds, Msg, SendMe) -> broadcast(UserId, ToUserIds, Msg, SendMe, undefined).
broadcast(UserId, ToUserIds, Msg, SendMe, DeviceId) ->
  FormattedMsg = im_transport_router:format_record(Msg),
  CurrentDeviceId = case UserId =/= undefined andalso UserId =/= ?SYS_USER_ID of
    true -> attr(UserId, deviceId);
    false -> undefined
  end,

  Fun = fun
    (?SYS_USER_ID) -> skip;
    (ToUserId) ->
%%      io:format("~n~n~n[BROADCAST]~nFrom user: ~p~nToUserId: ~p  - PIDS ~p~nMsg: ~p~n~n~n~n", [UserId, ToUserId, pids(ToUserId), Msg]),
      lists:foreach(fun(Pid) ->
        case attr(ToUserId, Pid, transport) =:= websocket of
          true ->
            DoSend = case im:is_debug() of
              true -> true;
              false ->
                case SendMe of
                  true -> true;
                  false ->
                    case DeviceId =:= undefined of
                      true -> attr(ToUserId, Pid, deviceId) =/= CurrentDeviceId;
                      false -> attr(ToUserId, Pid, deviceId) =:= DeviceId
                    end
                end
            end,

            case DoSend of
              true -> Pid ! FormattedMsg;
              false -> skip
            end;
          false ->
            skip
        end
      end, pids(ToUserId))
  end,

  lists:foreach(Fun, ToUserIds).
