-module(im_channel_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).

%% Public API
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% Supervisor callbacks
init([]) ->
    ImChannelChild = {
        im_channel,
        {im_channel, start_link, []},
        permanent,
        5000,
        worker,
        [im_channel]
    },

    {ok, {{one_for_one, 5, 10}, [ImChannelChild]}}.