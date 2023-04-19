-module(im_sms_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
  {ok, {{one_for_one, 10, 600}, [
    {
      im_sms_twilio,
      {im_sms_twilio, start_link, []},
      permanent, infinity, supervisor, []
    },
    {
      im_sms_alpha,
      {im_sms_alpha, start_link, []},
      permanent, infinity, supervisor, []
    },
    {
      im_sms_thailand,
      {im_sms_thailand, start_link, []},
      permanent, infinity, supervisor, []
    },
    {
      im_sms_nexmo,
      {im_sms_nexmo, start_link, []},
      permanent, infinity, supervisor, []
    }
  ]}}.
