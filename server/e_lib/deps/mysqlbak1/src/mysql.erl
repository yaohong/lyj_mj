-module(mysql).
-author('yh@gmail.com').
-export([start/0, stop/0]).
-export([
	add_sql_pool/8,
	remove_sql_pool/1,


	transaction/2,
	fetch/2
]).
-spec start() -> ok | {error, term()}.
start() ->
	application:start(mysql).

-spec stop() -> ok | {error, term()}.
stop() ->
	application:stop(mysql).


add_sql_pool(PoolName, PoolSize, Host, Port, User, Password, Database, LogFunc) when is_atom(PoolName) ->
	ChildSpec =
	{
		PoolName,
		{mysql_pool, start_link, [PoolName, PoolSize, Host, Port, User, Password, Database, LogFunc]},
		transient,
		brutal_kill,
		worker,
		[?MODULE]
	},
	supervisor:start_child(mysql_sup, ChildSpec).


remove_sql_pool(PoolName) ->
	mysql_pool:stop(PoolName).
%%	supervisor:delete_child(mysql_sup, PoolName).


fetch(PoolId, Querie) ->
	case get(mysql_context) of
		undefined ->
			{success, Pid} = mysql_pool:get_con(PoolId),
			mysql_conn:fetch(Pid, Querie);
		{Sock, LogFunc, Version} ->
			%%在事务里执行的查询
			mysql_conn:do_query(Sock, LogFunc, Querie, Version)
	end.


transaction(PoolId, Func) ->
	{success, Pid} = mysql_pool:get_con(PoolId),
	mysql_conn:transaction(Pid, Func).