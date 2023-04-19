-module(im_fixtures).

-include("im_common.hrl").

-export([apply/0]).

apply() ->
  ensure_sys_user(),
  ensure_roles(),
  ensure_mongodb_indexes(),
  rpc:async_call(node(), im_elastic_search, drop_and_reindex_users, []),
  % im_workflow:ensure_sys_workflows(),
  ok.

ensure_sys_user() ->
  ok = ctail:put(#im_usr{id=?SYS_USER_ID}).

ensure_roles() ->
  Permissions = [
    {"assign_vendor_staff", "Assign vendor staff"},
    {"assign_vendor_rep", "Assign vendor rep"},
    {"manage_products", "Manage products"},
    {"manage_orders", "Manage orders"},
    {"manage_csr", "Manage support requests"},
    {"view_public_pages", "View public pages"}
  ],
  [ctail:put(#im_usr_perm{id=im_common:format_utf8(Id), name=im_common:format_utf8(Name)}) || {Id, Name} <- Permissions],

  Roles = [
    {"super_admin", "Super Admin", []},
    {"vendor", "Vendor", ["assign_role_vendor_staff", "assign_role_vendor_rep", "manage_products", "manage_orders", "manage_csr"]},
    {"vendor_staff", "Vendor Staff", ["assign_role_vendor_rep", "manage_products", "manage_orders", "manage_csr"]},
    {"vendor_rep", "Vendor Rep", ["manage_csr"]},
    {"customer", "Customer", ["view_public_pages"]}
  ],
  [ctail:put(#im_usr_role{id=im_common:format_utf8(Id), name=im_common:format_utf8(Name), permissions=Perms}) || {Id, Name, Perms} <- Roles].

ensure_mongodb_indexes() ->
  ctail_mongo:exec(ensure_index, [<<"im_feed_post">>, {<<"key">>, {<<"location">>, <<"2dsphere">>}}]),
  ctail_mongo:exec(ensure_index, [<<"im_localized_store">>, {<<"key">>, {<<"location">>, <<"2dsphere">>}}]).
