-module(qp_sup).
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
	{success, {DbAddr, DbPort, DbUser, DbPassword, DbName}} = qp_config:get_cfg(db_addr),

	sql:start(DbAddr, DbPort, DbUser, DbPassword, DbName),



	GatewayUserSupervisor =
		{gateway_user_sup,
		 {tmp_sup, start_link, [gateway_user_sup, gateway_user]},
		 transient,
		 brutal_kill,
		 supervisor,
		 [gateway_user_sup]},




	TimerManager =
		{timer_manager,
		 {timer_manager, start_link, []},
		 permanent,
		 5000,
		 worker,
		 [timer_manager]},


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
