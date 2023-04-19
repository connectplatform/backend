-module(im_app_conf_watcher).

-include("im_common.hrl").

-export([spec/0, handle/3]).
-export([get/0, get/1]).

-record(state, {lastModified, json, curDir}).

spec() -> im_std_worker:spec(im_app_conf_watcher, [], fun ?MODULE:handle/3).

get() -> im_std_worker:call(im_app_conf_watcher, {get}).
get(Part) -> im_std_worker:call(im_app_conf_watcher, {get, Part}).

handle(init, _, _) ->
  {ok, CurDir} = file:get_cwd(),
  CurDirFixed = re:replace(CurDir, "/_build/test/rel/messenger_backend", "", [{return,list}]),

  ConfigFilePath = case get_config_file_path() of
    {ok, FilePath} -> CurDirFixed ++ "/" ++ FilePath;
    {error, _} -> "failed"
  end,

  im_logger:debug(undefined, "[AppConf] Initializing. Config file path: ~p", [ConfigFilePath]),

  timer:send_interval(1000, self(), refresh),

  {ok, #state{curDir = CurDirFixed, lastModified = 0}};

handle(info, refresh, State=#state{curDir = CurDir, lastModified = LastModified}) ->
  NewState=case get_config_file_path() of
    {ok, FilePath} ->
      FullFilePath = CurDir ++ "/" ++ FilePath,
      FileLastModified=filelib:last_modified(FullFilePath),

      case FileLastModified>LastModified of
        true ->
          case im_common:read_json_file(FullFilePath) of
            undefined -> State;
            Json ->
              im_logger:debug(undefined,
                "[AppConf] Config change detected. File: ~p, cache: ~p, JSON: ~p",
                [FileLastModified, LastModified, Json]
              ),
              case get_part(Json, "bots") of
                {ok, BotsConf} -> im_bot:ensure_sys_bots(BotsConf);
                {error, _} -> skip
              end,
              case get_part(Json, "permissions") of
                {ok, PermissionsConf} -> im_acl:ensure_permissions(PermissionsConf);
                {error, _} -> skip
              end,
              case get_part(Json, "roles") of
                {ok, RolesConf} -> im_acl:ensure_roles(RolesConf);
                {error, _} -> skip
              end,
              State#state{lastModified = FileLastModified, json = Json}
          end;
        false ->
          case FileLastModified =:= 0 of
            true -> State#state{lastModified = 0};
            false -> State
          end
      end;
    {error, _} ->
      im_logger:error(undefined, "[AppConf] Failed to get config file path", []),
      State
  end,

  {noreply, NewState};

handle(call, {get}, State=#state{json=Json}) -> {reply, Json, State};
handle(call, {get, Part}, State=#state{json=Json}) -> {reply, get_part(Json, Part), State};

handle(_, _, _) -> ok.

get_config_file_path() ->
  case get_watch_folder() of
    {ok, Folder} -> {ok, Folder++"/app_conf.json"};
    {error, Reason} -> {error, Reason}
  end.

get_watch_folder() ->
  case application:get_env(im, priv_folder) of
    {ok, P} -> {ok, P++"/data"};
    _ -> {error, invalid_priv_folder_config}
  end.

get_part(Json, Part) ->
  case Json of
    {struct, List} when is_list(List) ->
      case proplists:get_value(Part, List) of
        undefined -> {error, not_found};
        Data -> {ok, Data}
      end;
    _ ->
      {error, invalid_json}
  end.
