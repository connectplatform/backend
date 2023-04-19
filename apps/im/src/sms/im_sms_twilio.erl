-module(im_sms_twilio).
-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([send/3]).

-define(SMS_TIMEOUT, 60000).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, Alphaname} = application:get_env(im, twilio_alphaname),
  {ok, From}      = application:get_env(im, twilio_from),
  {ok, Sid}       = application:get_env(im, twilio_sid),
  {ok, AuthToken} = application:get_env(im, twilio_auth_token),

  {ok, {{simple_one_for_one, 10, 60}, [
    {
      im_sms_twilio_worker,
      {im_sms_twilio_worker, start_link, [Alphaname, From, Sid, AuthToken]},
      temporary, 1000, worker, []
    }
  ]}}.

send(CountryCode, PhoneNumber, Code) ->
  {ok, Worker} = supervisor:start_child(?MODULE, []),

  Res = try gen_server:call(Worker, {send, CountryCode, remove_plus(PhoneNumber), Code}, ?SMS_TIMEOUT)
        catch
          exit:{timeout,_} -> {error, timeout}
        end,
  supervisor:terminate_child(?MODULE, Worker),
  Res.

remove_plus(V) ->
  lists:filter(fun(C) -> C =/= 43 end, V).
