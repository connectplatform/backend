-module(im_sms_twilio_worker).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/4]).

-record(state, {
          alphaname   :: list(),
          from_number :: list(),
          sid         :: list(),
          auth_token  :: list()
         }).

start_link(Alphaname, FromNumber, Sid, AuthToken) ->
  gen_server:start_link(?MODULE, [Alphaname, FromNumber, Sid, AuthToken], []).

init([Alphaname, FromNumber, Sid, AuthToken]) ->
  {ok, #state{
    alphaname=Alphaname,
    from_number=FromNumber,
    sid=Sid,
    auth_token=AuthToken
  }}.

handle_call({send, CountryCode, PhoneNumber, Code}, _, State=#state{alphaname=Alphaname, from_number=FromNumber, sid=Sid, auth_token=AuthToken}) ->
  SendFrom = define_from(CountryCode, Alphaname, FromNumber),
  Body = "Body=" ++ Code ++ "&To=%2B" ++ PhoneNumber ++ "&From=" ++ SendFrom,

  Output = httpc:request(post, {
    "https://api.twilio.com/2010-04-01/Accounts/" ++ Sid ++ "/Messages.json",
    [{"Authorization", "Basic " ++ base64:encode_to_string(Sid ++ ":" ++ AuthToken)}],
    "application/x-www-form-urlencoded",
    Body
  }, [], []),

  Res = case Output of
    {ok, {{"HTTP/1.1", 201, "CREATED"}, _, _Data}} -> ok;
    {ok, {{"HTTP/1.1", 400, "BAD REQUEST"}, _, Data}} ->
      {ok, {struct, Json}} = yaws_json2:decode_string(Data),
      {error, proplists:get_value("message", Json, "Failed to send sms (bad request)")};
    {ok, {{"HTTP/1.1", 401, "UNAUTHORIZED"}, _, Data}} ->
      {ok, {struct, Json}} = yaws_json2:decode_string(Data),
      {error, proplists:get_value("detail", Json, "Failed to send sms (unauthorized)")};
    {error, {failed_connect,[{to_address, {Address,Port}}, {inet,[inet],ssl_not_started}]}} ->
      {error, "Filed Connect to Address: " ++ Address ++ ":" ++ integer_to_list(Port) ++ ". ssl not started"};
    _ ->
      {error, "Failed to send sms (unknown error)"}
  end,

  {reply, Res, State};
handle_call(_Request=_, _, State=#state{})   -> {reply, ok, State}.

handle_cast(_Request={}, State=#state{})     -> {noreply, State}.

handle_info(_Info, State=#state{})           -> {noreply, State}.

terminate(_, _State=#state{})                -> ok.

code_change(_OldVsn, State=#state{}, _Extra) -> {ok, State}.

define_from(380, Alphaname, _) -> Alphaname;
define_from(_, _, FromNumber)  -> FromNumber.
