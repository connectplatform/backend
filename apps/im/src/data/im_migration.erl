-module(im_migration).

-include("im_common.hrl").

-callback up() -> ok|error.
-callback down() -> ok|error.

-export([apply/0, upgrade/1, downgrade/1]).

apply() ->
  im_logger:debug(undefined, "[Migration] List: ~p", [migrations()]),
  lists:foreach(fun(Module) -> upgrade(Module) end, migrations()).

upgrade(Module) ->
  case is_applied(Module) of
    true -> im_logger:debug(undefined, "[Migration][upgrade] ~p skipped!", [Module]);
    false ->
      case Module:up() of
        ok ->
          im_logger:debug(undefined, "[Migration][upgrade] ~p upgraded!", [Module]),
          ctail:put(#im_migration{name=im_common:ensure_binary(Module), time=sm:now()});
        error ->
          im_logger:error(undefined, "[Migration][upgrade] ~p failed!", [Module])
      end
  end.

downgrade(Module) ->
  case is_applied(Module) of
    true ->
      case Module:down() of
        ok ->
          im_logger:debug(undefined, "[Migration][downgrade][~p] downgraded!", [Module]),
          ctail:delete(im_migration, im_common:ensure_binary(Module));
        error ->
          im_logger:error(undefined, "[Migration][downgrade][~p] failed!", [Module])
      end;
    false ->
      im_logger:debug(undefined, "[Migration][downgrade][~p] skipped!", [Module])
  end.

migrations() ->
  {ok, Modules} = application:get_key(im, modules),
  Migrations = [ M || M <- Modules, lists:member(im_migration, proplists:get_value(behaviour, M:module_info(attributes), [])) ],
  lists:usort(Migrations).

is_applied(Name) ->
  case ctail:get(im_migration, im_common:ensure_binary(Name)) of
    {ok, _Record}      -> true;
    {error, not_found} -> false
  end.
