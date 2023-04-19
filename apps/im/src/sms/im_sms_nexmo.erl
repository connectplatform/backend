-module(im_sms_nexmo).
-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([send/3]).

-define(SMS_TIMEOUT, 60000).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok,  Alphaname} = application:get_env(im, nexmo_alphaname),
  {ok,       From} = application:get_env(im, nexmo_from),
  {ok,     ApiKey} = application:get_env(im, nexmo_api_key),
  {ok,  ApiSecret} = application:get_env(im, nexmo_api_secret),

  {ok, {{simple_one_for_one, 10, 60}, [
    {
      im_sms_nexmo_worker,
      {im_sms_nexmo_worker, start_link, [Alphaname, From, ApiKey, ApiSecret]},
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

remove_plus(V) -> lists:filter(fun(C) -> C =/= 43 end, V).

