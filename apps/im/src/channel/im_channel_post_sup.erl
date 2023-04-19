-module(im_channel_post_sup).
-behaviour(supervisor).

-include("im_common.hrl").

-export([start_link/0, init/1]).
-define(ETS_NAME, im_channel_post_sup).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
  {ok, {{simple_one_for_one, 10, 60}, [
    {
      im_channel_post_worker,
      {im_channel_post_worker, start_link, []},
      temporary, 2000, worker, []
    }
  ]}}.
