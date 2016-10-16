-module(hash_service_sup).
-export([start_link/0]).
-export([init/1]).


%%好友进程的个数，不用一个进程的原因在于压力的分摊
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).



init([]) ->
	HashServiceWork =
		{hash_service_work,
			{hash_service_work, start_link, []},
			permanent,
			5000,
			worker,
			[hash_service_work]},
	{ok,
		{
			{one_for_one, 10, 10},
			[
				HashServiceWork
			]
		}
	}.


