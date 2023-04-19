-module(im_sms_alpha).
-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([send/3]).

-define(SMS_TIMEOUT, 30000).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, AlphaName} = application:get_env(im, alphasms_alphaname),
  {ok, PackageKey} = application:get_env(im, alphasms_package_key),
  {ok, {{simple_one_for_one, 10, 60}, [
    {
      im_sms_alpha_worker,
      {im_sms_alpha_worker, start_link, [AlphaName, PackageKey]},
      temporary, 1000, worker, []
    }
  ]}}.

send(_CountryCode, PhoneNumber, Code) ->
  {ok, Worker} = supervisor:start_child(?MODULE, []),
  Res = try gen_server:call(Worker, {send, PhoneNumber, Code}, ?SMS_TIMEOUT)
    catch
      exit:{timeout, _} -> {error, timeout}
    end,
  supervisor:terminate_child(?MODULE, Worker),
  Res.
