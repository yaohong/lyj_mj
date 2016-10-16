-module(server_frame_sup).
-export([start_link/1]).
-export([init/1]).

%%好友进程的个数，不用一个进程的原因在于压力的分摊
start_link({ServerName, UserHandleModule, PacketHeadLen, UserSup, ReceiverSup, Ip, Port}) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ServerName, UserHandleModule, PacketHeadLen, UserSup, ReceiverSup, Ip, Port]).

init([ServerName, UserHandleModule, PacketHeadLen, UserSup, ReceiverSup, Ip, Port]) ->
	ReceiverSupervisor =
		{ReceiverSup,
			{tmp_sup, start_link, [ReceiverSup, tcp_receiver]},
			transient,
			brutal_kill,
			supervisor,
			[ReceiverSup]},
	UserSupervisor =
		{UserSup,
			{tmp_sup, start_link, [UserSup, UserHandleModule]},
			transient,
			brutal_kill,
			supervisor,
			[UserSup]},
	UserListenServer =
		{ServerName,
			{tcp_server, start_link, [Ip, Port, ServerName, UserHandleModule, PacketHeadLen, UserSup, ReceiverSup]},
			transient,
			brutal_kill,
			supervisor,
			[ServerName]},
	{ok,
		{
			{one_for_one, 10, 10},
			[
				ReceiverSupervisor,
				UserSupervisor,
				UserListenServer
			]
		}
	}.
