-module(yhsql).
-author('yh@gmail.com').
-export([start/0, stop/0]).
-export([
	add_sql_pool/8, add_sql_pool/9,
	remove_sql_pool/1,
	transaction/2,
	fetch/2
]).

-include("yhsql.hrl").

-export(
	[
		open_sql/6,open_sql/7,
		sfetch/2,
		stransaction/2,
		close_sql/1
	]).
-spec start() -> ok | {error, term()}.
start() ->
	application:start(yhsql).

-spec stop() -> ok | {error, term()}.
stop() ->
	application:stop(yhsql).


log(Module, Line, _Level, FormatFun) ->
	{Format, Arguments} = FormatFun(),
	io:format("~w:~b: "++ Format ++ "~n", [Module, Line] ++ Arguments).


add_sql_pool(PoolName, PoolSize, Host, Port, User, Password, Database, LogFunc) when is_atom(PoolName) ->
	add_sql_pool(PoolName, PoolSize, Host, Port, User, Password, Database, LogFunc, ?RECV_TIMEOUT).
add_sql_pool(PoolName, PoolSize, Host, Port, User, Password, Database, LogFunc, Timeout) when is_atom(PoolName) ->
	NewLogFunc =
		if
			LogFunc =:= undefined -> fun log /4;
			true -> LogFunc
		end,
	ChildSpec =
	{
		PoolName,
		{yhsql_pool, start_link, [PoolName, PoolSize, Host, Port, User, Password, Database, NewLogFunc, Timeout]},
		transient,
		brutal_kill,
		worker,
		[?MODULE]
	},
	supervisor:start_child(yhsql_sup, ChildSpec).


remove_sql_pool(PoolName) ->
	yhsql_pool:stop(PoolName).


fetch(PoolId, Querie) ->
	case get(yhsql_context) of
		undefined ->
			{success, Pid} = yhsql_pool:get_con(PoolId),
			yhsql_conn:fetch(Pid, Querie);
		{Sock, LogFunc, Version} ->
			%%在事务里执行的查询
			yhsql_conn:do_query(Sock, LogFunc, Querie, Version)
	end.


transaction(PoolId, Func) ->
	{success, Pid} = yhsql_pool:get_con(PoolId),
	yhsql_conn:transaction(Pid, Func).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

open_sql(Host, Port, User, Password, Database, LogFunc) ->
	open_sql(Host, Port, User, Password, Database, LogFunc, ?RECV_TIMEOUT).
open_sql(Host, Port, User, Password, Database, LogFunc, SocketTimeout) ->
	NewLogFunc =
		if
			LogFunc =:= undefined -> fun log /4;
			true -> LogFunc
		end,
	case yhsql_conn:start_link(Host, Port, User, Password, Database, NewLogFunc, SocketTimeout) of
		{ok, Pid} -> {success, Pid};
		{error, Reason} -> {fail, Reason}
	end.

sfetch(Pid, Sql) when is_pid(Pid) ->
	case get(yhsql_context) of
		undefined ->
			yhsql_conn:fetch(Pid, Sql);
		{Sock, LogFunc, Version} ->
			%%在事务里执行的查询
			yhsql_conn:do_query(Sock, LogFunc, Sql, Version)
	end.

stransaction(Pid, Func) when is_pid(Pid) ->
	yhsql_conn:transaction(Pid, Func).


close_sql(Pid) when is_pid(Pid) ->
	yhsql_conn:exit(Pid).