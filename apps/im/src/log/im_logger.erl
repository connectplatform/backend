-module(im_logger).

-include("im_common.hrl").

-export([info/3, info/2]).
-export([debug/3, debug/2]).
-export([warning/3, warning/2]).
-export([error/3, error/2]).
-export([catch_errors/2]).

%% API

info(UserId, Format, Data)    -> put(info, UserId, Format, Data).
info(UserId, Msg)             -> put(info, UserId, Msg).
debug(UserId, Format, Data)   -> put(debug, UserId, Format, Data).
debug(UserId, Msg)            -> put(debug, UserId, Msg).
warning(UserId, Format, Data) -> put(warning, UserId, Format, Data).
warning(UserId, Msg)          -> put(warning, UserId, Msg).
error(UserId, Format, Data)   -> put(error, UserId, Format, Data).
error(UserId, Msg)            -> put(error, UserId, Msg).

error_wstacktrace(UserId, Format, Data) ->
  error_wstacktrace(UserId, io_lib:format(Format, Data)).

error_wstacktrace(UserId, Msg) ->
  error(UserId, "~ts. Stacktrace: ~p", [Msg, erlang:get_stacktrace()]).

catch_errors(Fun, UserId) ->
  try
    Fun()
  catch
    throw:Term   -> error_wstacktrace(UserId, "Throw: ~p", [Term]), none;
    exit:Reason  -> error_wstacktrace(UserId, "Exit: ~p", [Reason]), none;
    error:Reason -> error_wstacktrace(UserId, "Error: ~p", [Reason]), none
  end.

%% Internal

put(Level, UserId, Format, Data) ->
  Msg = io_lib:format(Format, Data),
  put(Level, UserId, Msg).

put(Level, UserId, Msg) ->
  UserId1 = im_common:format_id(UserId),

  Metadata = case UserId =/= undefined of
    true ->
      % im_roster_chat:get(UserId),
      [{userId, UserId1}];
        % {userName, User#im_usr.name},
        % {deviceId, im_user_state:attr(UserId, deviceId)},
        % {deviceName, im_user_state:attr(UserId, deviceName)}];
    false -> []
  end,

  Msg1 = re:replace(Msg, ",\n\s+", ",", [{return, list}, global]),
  MsgFormatted = io_lib:format("~ts ~ts", [UserId1, Msg1]),

  case Level of
    info -> lager:info(Metadata, MsgFormatted, []);
    debug -> lager:debug(Metadata, MsgFormatted);
    warning -> lager:warning(Metadata, MsgFormatted);
    error -> lager:error(Metadata, MsgFormatted)
  end.
