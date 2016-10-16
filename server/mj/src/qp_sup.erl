-module(world_sup).
-author('yh@gmail.com').
-behaviour(supervisor).
-export([start_link/0]).

-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.




init([]) ->
	{success, {DbAddr, DbPort, DbUser, DbPassword, DbName}} = world_config:get_cfg(db_addr),

	sql:start(DbAddr, DbPort, DbUser, DbPassword, DbName),

	CsvData =
		{csv_data,
		 {csv_data, start_link, []},
		 permanent,
		 5000,
		 worker,
		 [csv_data]},

	GatewayUserSupervisor =
		{gateway_user_sup,
		 {tmp_sup, start_link, [gateway_user_sup, gateway_user]},
		 transient,
		 brutal_kill,
		 supervisor,
		 [gateway_user_sup]},

	Im =
		{im,
		 {im, start_link, []},
		 permanent,
		 5000,
		 worker,
		 [im]},

    EtsTest =
        {ets_test,
         {ets_test, start_link, []},
         permanent,
         5000,
         worker,
         [ets_test]},

	ActorRead =
		{actor_read,
		 {actor_read, start_link, []},
		 permanent,
		 5000,
		 worker,
		 [actor_read]},

	TimerManager =
		{timer_manager,
		 {timer_manager, start_link, []},
		 permanent,
		 5000,
		 worker,
		 [timer_manager]},

	AccountManager =
		{account_manager,
		 {account_manager, start_link, []},
		 permanent,
		 5000,
		 worker,
		 [account_manager]},

	ActorManager =
		{actor_manager,
		 {actor_manager, start_link, []},
		 permanent,
		 5000,
		 worker,
		 [actor_manager]},

%%    ActorHelper =
%%        {actor_help,
%%            {actor_help, start_link, []},
%%            permanent,
%%            5000,
%%            worker,
%%            [actor_help]},
	GatewayUserManager =
		{gateway_user_manager,
		 {gateway_user_manager, start_link, []},
		 permanent,
		 5000,
		 worker,
		 [gateway_user_manager]},

	IdManager =
		{id_manager,
		 {id_manager, start_link, []},
		 permanent,
		 5000,
		 worker,
		 [id_manager]},


	GatewayManager =
		{gateway_manager,
		 {gateway_manager, start_link, []},
		 permanent,
		 5000,
		 worker,
		 [gateway_manager]},

	SceneManager =
		{scene_manager,
			{scene_manager, start_link, []},
			permanent,
			5000,
			worker,
			[scene_manager]},

	DbManager =
		{db_manager,
		 {db_manager, start_link, []},
		 permanent,
		 5000,
		 worker,
		 [db_manager]},

	HomeDuplicateManager =
		{home_manager,
		 {home_manager, start_link, []},
		 permanent,
		 5000,
		 worker,
		 [home_manager]},

	{success, {GatewayListenIp, GatewayListenPort}} = world_config:get_cfg(gateway_listen_addr),

	GatewayReceiverSupervisor =
		{gateway_receiver_sup,
		 {tmp_sup, start_link, [gateway_receiver_sup, tcp_receiver]},
		 transient,
		 brutal_kill,
		 supervisor,
		 [gateway_receiver_sup]},
	GatewayConnectSupervisor =
		{gateway_connect_sup,
		 {tmp_sup, start_link, [gateway_connect_sup, gateway_connect]},
		 transient,
		 brutal_kill,
		 supervisor,
		 [gateway_connect_sup]},
	GatewayListenServer =
		{
			gateway_server,
		 	{tcp_server, start_link, [world_util:ipstr_to_v4(GatewayListenIp), GatewayListenPort, gateway_server, gateway_connect, 20, gateway_connect_sup, gateway_receiver_sup, 256 * 1024]},
			transient,
		 	brutal_kill,
		 	supervisor,
		 	[gateway_server]
		},

	{success, {SceneListenIp, SceneListenPort}} = world_config:get_cfg(scene_listen_addr),
	SceneReceiverSupervisor =
		{scene_receiver_sup,
		 {tmp_sup, start_link, [scene_receiver_sup, tcp_receiver]},
		 transient,
		 brutal_kill,
		 supervisor,
		 [scene_receiver_sup]},
	SceneUserSupervisor =
		{scene_user_sup,
		 {tmp_sup, start_link, [scene_user_sup, scene_user]},
		 transient,
		 brutal_kill,
		 supervisor,
		 [scene_user_sup]},
	SceneListenServer =
		{
			scene_server,
			{tcp_server, start_link, [world_util:ipstr_to_v4(SceneListenIp), SceneListenPort, scene_server, scene_user, 2, scene_user_sup, scene_receiver_sup, 256 * 1024]},
			transient,
			brutal_kill,
			supervisor,
			[scene_server]
		},

    GuildManager  =
        {
            guild_manager,
            {guild_manager, start_link, []},
            permanent,
            5000,
            worker,
            [guild_manager]
        },


    GuildRead  =
        {
            guild_read,
            {guild_read, start_link, []},
            permanent,
            5000,
            worker,
            [guild_read]
        },

    GuildApplyManager  =
        {
            guild_apply_manager,
            {guild_apply_manager, start_link, []},
            permanent,
            5000,
            worker,
            [guild_apply_manager]
        },

    FilterSup =
        {
            filter_sup,
            {filter_sup, start_link, []},
            permanent,
            infinity,
            supervisor,
            [filter_sup]
        },

    StrongHoldBattleMgr =
        {
            stronghold_battle_manager,
            {stronghold_battle_manager, start_link, []},
            permanent,
            5000,
            worker,
            [stronghold_battle_manager]
        },

    {ok,
    	{
    		{one_for_one, 10, 10},
			[
				CsvData,
				GatewayUserSupervisor,
				Im,
                EtsTest,
				ActorRead,
				TimerManager,
				AccountManager,
				ActorManager,
%%                ActorHelper,
				GatewayUserManager,
				IdManager,
				GatewayManager,
				SceneManager,
				DbManager,
				HomeDuplicateManager,
				GatewayReceiverSupervisor,
				GatewayConnectSupervisor,
				GatewayListenServer,
				SceneReceiverSupervisor,
				SceneUserSupervisor,
				SceneListenServer,
                GuildManager,
                GuildRead,
                GuildApplyManager,
                FilterSup,
                StrongHoldBattleMgr
			]
    	}
    }.


%%web_specs(Mod, ListenIp, Port) ->
%%	WebConfig = [{ip, ListenIp},
%%		{port, Port},
%%		{nodelay, true},
%%		{docroot, http_deps:local_path(["priv", "www"])}],
%%	{Mod,
%%		{Mod, start, [WebConfig]},
%%		permanent, 5000, worker, dynamic}.
