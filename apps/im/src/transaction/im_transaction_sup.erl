-module(im_transaction_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

-define(ETS_NAME, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, {
    #{
      strategy => simple_one_for_one,
      intensity => 6000,
      period => 60
    },
    [
      #{
        id => im_transaction,
        start => {im_transaction, start_link, []},
        restart => temporary,
        shutdown => infinity,
        type => worker
      }
    ]
  }}.