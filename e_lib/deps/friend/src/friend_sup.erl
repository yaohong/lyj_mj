-module(friend_sup).
-export([start_link/0]).
-export([init/1]).
-export([build_friend_process_name/1]).
-export([get_process/1]).

%%好友进程的个数，不用一个进程的原因在于压力的分摊
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	ProcessIndexList = lists:seq(0, get_friend_pool_size() - 1),
	FriendNode = list_to_atom(os:getenv("FRIEND_NODE")),
    SpecList = lists:map(
        fun(Index) ->
        	Name = build_friend_process_name(Index),
        	{
				{friend, Index},
				{friend_work, start_link, [Name, FriendNode]},
				permanent,
				brutal_kill,
				worker,
				[gen_server]
			}
        end, ProcessIndexList),
    {ok, {{one_for_one, 5, 10}, SpecList}}.


-spec build_friend_process_name(Index :: integer()) -> atom().
build_friend_process_name(Index) ->
    NameList = "friend_" ++ integer_to_list(Index),
    list_to_atom(NameList).

-spec get_process(FriendAccount :: string()) -> atom().
get_process(FriendAccount)  ->
	PoolSize = get_friend_pool_size(),		%%获取池的大小
	Md5 = erlang:md5(FriendAccount),
	HashValue = erlang:phash2(Md5),
	RemValue = HashValue rem PoolSize,
	build_friend_process_name(RemValue).

get_friend_pool_size() ->
	{ok, PoolSize} = application:get_env(friend, friend_pool_size),
	PoolSize.
