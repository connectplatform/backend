-module(im_sms_thailand_worker).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/2]).

-record(state, {groupCode, token}).

start_link(GroupCode, Token) ->
  gen_server:start_link(?MODULE, [GroupCode, Token], []).

init([GroupCode, Token]) ->
  {ok, #state{groupCode=GroupCode, token=Token}}.

handle_call({send, _CountryCode, PhoneNumber, Code}, _, State=#state{groupCode=GroupCode, token=Token}) ->
  Body = jsx:encode([
    {<<"group_code">>, im_common:format_utf8(GroupCode)},
    {<<"sms_text">>, im_common:format_utf8(Code)},
    {<<"recipients">>, [im_common:format_utf8(PhoneNumber)]},
    {<<"async">>, false}
  ]),

  im_logger:info(undefined, "[SmsThailand] Sending to gateway: ~p", [Body]),

  Output = httpc:request(post, {
    "http://183.88.235.50:8000/api/v1/sms/sendMessage",
    [{"Authorization", "Bearer " ++ Token}],
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
