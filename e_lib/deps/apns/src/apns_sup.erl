-module(apns_sup).
-export([start_link/0]).
-export([init/1]).

%%好友进程的个数，不用一个进程的原因在于压力的分摊
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	{ok, AppPushServiceNodeName} = application:get_env(apns, apns_service_node_name),
	ApnsWork =
		{apns_work,
			{apns_work, start_link, [AppPushServiceNodeName]},
			permanent,
			brutal_kill,
			worker,
			[gen_server]
		},
    {ok, {{one_for_one, 5, 10}, [ApnsWork]}}.
