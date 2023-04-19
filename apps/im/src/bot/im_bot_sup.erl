-module(im_bot_sup).
-include("im_common.hrl").
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, {
    #{strategy => simple_one_for_one,
      intensity => 6000,
      period => 60},
    [
      #{id => im_bot_worker,
        start => {im_bot_worker, start_link, []},
        restart => temporary,
        shutdown => infinity,
        type => worker}
    ]
  }}.
