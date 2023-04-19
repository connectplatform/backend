-module(im_sms_nexmo_worker).
-behaviour(gen_server).

-define(NEXMO_URL, "https://rest.nexmo.com/sms/json").
-define(NEXMO_STATUS_CODE_SUCCESS, "0").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/4]).

-record(state, {
          alphaname   :: list(),
          from_number :: list(),
          api_key     :: list(),
          api_secret  :: list()
         }).

start_link(Alphaname, FromNumber, ApiKey, ApiSecret) ->
  gen_server:start_link(?MODULE, [Alphaname, FromNumber, ApiKey, ApiSecret], []).

init([Alphaname, FromNumber, ApiKey, ApiSecret]) ->
  {ok, #state{
    alphaname=Alphaname,
    from_number=FromNumber,
    api_key=ApiKey,
    api_secret=ApiSecret
  }}.

handle_call({send, CountryCode, PhoneNumber, Code}, _, State=#state{alphaname=Alphaname, from_number=FromNumber, api_key=ApiKey, api_secret=ApiSecret}) ->
  SendFrom = define_from(CountryCode, Alphaname, FromNumber),
  Url = ?NEXMO_URL ++
    "?api_key=" ++ ApiKey ++
    "&api_secret=" ++ ApiSecret ++
    "&to=" ++ PhoneNumber ++
    "&from=" ++ http_uri:encode(SendFrom) ++
    "&text=" ++ http_uri:encode(Code)
  ,

  HttpResponse = httpc:request(get, {Url, []}, [], []),
  {ok, {{_,HttpCode,_}, _, Data}} = HttpResponse,

  Res = case HttpCode of
    200 ->
      {ok, {struct, Json}} = yaws_json2:decode_string(Data),
      case proplists:get_value("messages", Json, "not_found") of
        {array, [{struct, Message1}|_]} ->
          RespCode = proplists:get_value("status", Message1, "not_found"),
          case RespCode of
            ?NEXMO_STATUS_CODE_SUCCESS -> ok;
            _                          -> {error, "Nexmo Error Code: " ++ RespCode}
          end;
        _ ->
          {error, "Nexmo Bad Response"}
      end;
    _ ->
      {error, "HTTP Error with code: " ++ HttpCode}
  end,

  {reply, Res, State};
handle_call(_Request=_, _, State=#state{})   -> {reply, ok, State}.

handle_cast(_Request={}, State=#state{})     -> {noreply, State}.

handle_info(_Info, State=#state{})           -> {noreply, State}.

terminate(_, _State=#state{})                -> ok.

code_change(_OldVsn, State=#state{}, _Extra) -> {ok, State}.

define_from(61,   Alphaname, _)          -> Alphaname; %%  Australia
define_from(33,   Alphaname, _)          -> Alphaname; %%  France
define_from(49,   Alphaname, _)          -> Alphaname; %%  Germany
define_from(964,  Alphaname, _)          -> Alphaname; %%  Iraq
define_from(39,   Alphaname, _)          -> Alphaname; %%  Italy
define_from(31,   Alphaname, _)          -> Alphaname; %%  Netherlands
define_from(34,   Alphaname, _)          -> Alphaname; %%  Spain
define_from(380,  Alphaname, _)          -> Alphaname; %%  Ukraine
define_from(_,    _,         FromNumber) -> FromNumber.
