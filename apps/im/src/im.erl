-module(im).

-behaviour(application).
-behaviour(supervisor).

-include("im_common.hrl").

-export([start/2, stop/1, init/1, clear_state/1, is_debug/0, is_test/0, is_dev/0]).

is_debug() -> sm:env(im, env) =:= "test" orelse sm:env(im, env) =:= "dev".
is_test() -> sm:env(im, env) =:= "test".
is_dev() -> sm:env(im, env) =:= "dev".

start(_, _) ->
  _ProtocolAtomsChacheWarmupResult = im_atoms:get(),
  supervisor:start_link({local, im_sup}, ?MODULE, []).

stop(_) -> ok.

init([]) ->
  ctail:init(),

  StartArgs = [
    {ranch,  sm:env(smoothie, ranch)},
    {cowboy, sm:env(smoothie, cowboy)},
    {routes, routes()}
  ],

  case sm:env(smoothie, protocol) of
    https -> sm:start_https(StartArgs);
    _     -> sm:start_http(StartArgs)
  end,

  im_trans:init(),
  fcm:start(im_fcm_sender, sm:env(im, fcm_server_key, "")),
  im_fcm_pool:start_pool(),
  im_apn_pool:start_pool(),

  BotSup          = supervisor(im_bot_sup, im_bot_sup, permanent, infinity),
  ChannelPostSup  = supervisor(im_channel_post_sup, im_channel_post_sup, permanent, infinity),
  CallSup         = supervisor(im_call_sup, im_call_sup, permanent, infinity),
  SmsSup          = supervisor(im_sms_sup, im_sms_sup, permanent, infinity),
  TransactionSup  = supervisor(im_transaction_sup, im_transaction_sup, permanent, infinity),
  RosterSup       = supervisor(im_roster_sup, im_roster_sup, permanent, infinity),

  EtsSup          = im_ets:spec(),
  ContactQueueSup = im_contact_queue:spec(),
  UserTransformSup= im_user_transform:spec(),
  AclSup          = im_acl:spec(),
  AppConfSup      = im_app_conf_watcher:spec(),
  WorkerPoolSup   = im_worker_pool:spec(),

  ChildSpecs = [EtsSup, WorkerPoolSup, UserTransformSup, ContactQueueSup, AclSup,
    ChannelPostSup, CallSup, SmsSup, TransactionSup, RosterSup, BotSup, AppConfSup],

  im_cron:init(),
  im_event:initialize(),
  im_migration:apply(),
  im_fixtures:apply(),

  % case is_dev() of
  %   true -> sync:go();
  %   false -> skip
  % end,

  {ok, {#{
    strategy => one_for_one,
    intensity => 6000,
    period => 60
  }, ChildSpecs}}.

routes() ->
  [
    {"/",                 {priv_file, im, "static/index.html"}},
    {"/static/[...]",     {priv_dir,  im, "static"}},
    {"/media/:hash",      {request,   im_media_route, handle}},
    {"/api/profile/:id",  {request,   im_profile_route, handle}},
    {"/api/feed-post/:id",{request,   im_feed_post_route, handle}},
    {"/api/:version",     {request,   im_transport_http, handle, sm_protocol_bert}},
    {"/api",              {request,   im_transport_http, handle, sm_protocol_bert}},
    {"/ws",               {websocket, im_transport_ws, sm_protocol_bert, 60000}}
  ].

supervisor(Name, Module, Restart, Shutdown) ->
  #{id => Name,
    start => {Module, start_link, []},
    restart => Restart,
    shutdown => Shutdown,
    type => supervisor,
    modules => [im]}.

clear_state(#'ClearState'{ref=Ref}) ->
  im_roster_sup:restart_all_workers(),
  #'ClearStateResp'{ref=Ref}.
