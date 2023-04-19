-module(im_std_worker).
-behaviour(supervisor).

-export([start_link/1, init/1]).
-export([sys_name/2, spec/3, call/2, cast/2]).

sys_name(sup, Name) -> list_to_atom("im_std_worker_" ++ atom_to_list(Name) ++ "_sup");
sys_name(wrk, Name) -> list_to_atom("im_std_worker_" ++ atom_to_list(Name) ++ "_wrk").

call(Name, Data)          -> gen_server:call(sys_name(wrk, Name), Data).
cast(Name, Data)          -> gen_server:cast(sys_name(wrk, Name), Data).

spec(Name, Options, Handler) ->
  Args = [{name, Name},
    {handler, Handler},
    {options, Options}],

  #{id => sys_name(sup, Name),
    start => {?MODULE, start_link, [Args]},
    restart => permanent,
    shutdown => infinity,
    type => supervisor,
    modules => [im]}.

% spec_pool(Name, Options, Handler) ->
%   Args = [{name, Name},
%     {handler, Handler},
%     {options, Options}],

%   #{id => sys_name(sup, Name),
%     start => {?MODULE, start_link_pool, [Args]},
%     restart => permanent,
%     shutdown => infinity,
%     type => supervisor,
%     modules => [im]}.

start_link(Args) ->
  Name = proplists:get_value(name, Args),
  supervisor:start_link({local, sys_name(sup, Name)}, ?MODULE, [Args ++ [{type, worker}]]).

% start_link_pool(Args) ->
%   Name = proplists:get_value(name, Args),
%   supervisor:start_link({local, sys_name(sup, Name)}, ?MODULE, [Args ++ [{type, pool}]]).

init([Args]) ->
  Name = proplists:get_value(name, Args),
  % Type = proplists:get_value(type, Args),

  % case Type of
  %   worker ->
      {ok, {
        #{strategy => one_for_one,
          intensity => 1,
          period => 1},
        [
          #{id => sys_name(wrk, Name),
            start => {im_std_worker_server, start_link, [Name, Args]},
            restart => permanent,
            shutdown => infinity,
            type => worker}
        ]
      }}.
  %   pool ->
  %     {ok, {
  %       #{strategy => simple_one_for_one,
  %         intensity => 6000,
  %         period => 60},
  %       [
  %         #{id => sys_name(wrk, Name),
  %           start => {im_std_worker_server, start_link, [Name, Args]},
  %           restart => temporary,
  %           shutdown => infinity,
  %           type => worker}
  %       ]
  %     }}
  % end.
