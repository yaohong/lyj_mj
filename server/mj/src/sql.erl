%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 三月 2016 21:16
%%%-------------------------------------------------------------------
-module(sql).
-author("yaohong").
-export([log/4]).
%% API
-export([start/5,
         stop/0]).

-export([pool_name/0, user_table_name/ 0]).
-export([task_table_name/0]).

-define(DB_POOL_SIZE, 20).

log(Module, Line, _Level, FormatFun) ->
	{Format, Arguments} = FormatFun(),
	file_log_server:send(debug, self(), Module, Line, Format, Arguments).

pool_name() -> zb.

user_table_name()->"zb_users".
task_table_name()->"zb_actor_task".






start(DbAddr, DbPort, DbUser, DbPassword, DbName) ->
	%%常规SQL操作池
	{ok, _} = yhsql:add_sql_pool(pool_name(), ?DB_POOL_SIZE, DbAddr, DbPort, DbUser, DbPassword, DbName, fun sql:log/4),
	%%用于数据落地的
	ok.

stop() ->
	yhsql:remove_sql_pool(pool_name).