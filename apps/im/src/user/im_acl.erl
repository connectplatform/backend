-module(im_acl).

-include("im_common.hrl").

-export([spec/0, handle/3]).
-export([ensure_permissions/1, ensure_roles/1]).
-export([get/1, get_roles/1, get_roles_for_perm/1]).
-export([has_role/2, has_role_one_of/2]).
-export([has_perm/2, has_perm_one_of/2, has_super_perms/1]).

-define(ROLE_UPDATE_INTERVAL, 5000).
-record(state, {roles=[], lastRolesUpdate=0}).

spec() -> im_std_worker:spec(acl, [], fun ?MODULE:handle/3).

get(#'UserRoles'{ref=Ref}) ->
  #'UserRolesResp'{ref=Ref, roles=[im_dto:format_user_role(Role) || Role <- ctail:all(im_usr_role)]}.

get_roles(UserId)                -> im_std_worker:call(acl, {get_roles, UserId}).
get_roles_for_perm(PermId)       -> im_std_worker:call(acl, {get_roles_for_perm, PermId}).
has_role(RoleId, UserId)         -> im_std_worker:call(acl, {has_role, UserId, [RoleId]}).
has_role_one_of(RoleIds, UserId) -> im_std_worker:call(acl, {has_role, UserId, RoleIds}).
has_perm(PermId, UserId)         -> im_std_worker:call(acl, {has_perm, UserId, [PermId]}).
has_perm_one_of(PermIds, UserId) -> im_std_worker:call(acl, {has_perm, UserId, PermIds}).

ensure_permissions({array, PermissionsConf}) ->
  lists:foreach(fun({struct, PermissionsConfigProps}) ->
    Id = im_common:ensure_binary(proplists:get_value("id", PermissionsConfigProps)),
    Name = proplists:get_value("name", PermissionsConfigProps),
    ctail:put(#im_usr_perm{id=Id, name=Name})
  end, PermissionsConf);
ensure_permissions(_) -> skip.

ensure_roles({array, RolesConf}) ->
  lists:foreach(fun({struct, RolesConfigProps}) ->
    Id = im_common:ensure_binary(proplists:get_value("id", RolesConfigProps)),
    Name = proplists:get_value("name", RolesConfigProps),
    Permissions = case proplists:get_value("permissions", RolesConfigProps) of
      {array, P} -> P;
      _          -> []
    end,
    ctail:put(#im_usr_role{id=Id, name=Name, permissions=Permissions})
  end, RolesConf);
ensure_roles(_) -> skip.

handle(init, _, _) ->
  Roles = ctail:all(im_usr_role),
  {ok, #state{roles=Roles}};

handle(call, {get_roles, UserId}, State) ->
  case im_roster_chat:get(UserId) of
    undefined -> {reply, [], State};
    User      -> {reply, User#im_usr.roles, State}
  end;

handle(call, {get_roles_for_perm, PermId}, State=#state{roles=Roles}) ->
  RolesFiltered = lists:filter(fun(#im_usr_role{id=RoleId, permissions=Perms}) ->
    lists:member(im_common:format_utf8(PermId), Perms)
  end, Roles),
  {reply, RolesFiltered, State};

handle(call, {has_role, UserId, RoleIds}, State) ->
  HasRole = case im_roster_chat:get(UserId) of
    undefined -> false;
    User ->
      Fun = fun(RoleId) ->
        lists:member(im_common:format_utf8(RoleId), User#im_usr.roles)
      end,
      case lists:filter(Fun, RoleIds) of
        [] -> false;
        _ -> true
      end
  end,
  {reply, HasRole, State};

handle(call, {has_perm, UserId, PermIds}, State=#state{roles=Roles, lastRolesUpdate=LastRolesUpdate}) ->
  case im_roster_chat:get(UserId) of
    undefined -> {reply, false, State};
    User ->
      case lists:member(<<"super_admin">>, User#im_usr.roles) of
        true ->
          {reply, true, State};
        false ->
          {Roles1, LastRolesUpdate1} = case LastRolesUpdate + ?ROLE_UPDATE_INTERVAL < sm:now() of
            true -> {ctail:all(im_usr_role), sm:now()};
            false -> {Roles, LastRolesUpdate}
          end,
          State1 = State#state{roles=Roles1, lastRolesUpdate=LastRolesUpdate1},
          Perms = lists:map(fun(#im_usr_role{id=RoleId, permissions=Perms2}) ->
            case lists:member(RoleId, User#im_usr.roles) of
              true -> Perms2;
              false -> []
            end
          end, Roles1),
          Perms1 = im_common:list_unique(lists:flatten(Perms)),
          Fun = fun(PermId) ->
            lists:member(im_common:ensure_binary(PermId), Perms1)
          end,
          HasPerm = case lists:filter(Fun, PermIds) of
            [] -> false;
            _ -> true
          end,
          {reply, HasPerm, State1}
      end
  end;
handle(_, _, _) -> ok.

has_super_perms(UserId) ->
  case has_role("super_admin", UserId) of
    true -> true;
    false -> im_bot:is_system(UserId)
  end.
