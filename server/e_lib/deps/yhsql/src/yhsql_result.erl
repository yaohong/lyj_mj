%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. 八月 2015 18:27
%%%-------------------------------------------------------------------
-module(yhsql_result).
-author("yaohong").
-include("yhsql.hrl").
%% API
-export([
	get_result_field_info/1,
	get_result_rows/1,
	get_result_affected_rows/1,
	get_result_reason/1,
	get_result_err_code/1,
	get_result_err_sql_state/1,
	get_result_insert_id/1
        ]).



get_result_field_info(#yhsql_result{fieldinfo = FieldInfo}) ->
	FieldInfo.

%% @doc Extract the Rows from MySQL Result on data received
%%
%% @spec get_result_rows(MySQLRes::yhsql_result()) -> [Row::list()]
get_result_rows(#yhsql_result{rows=AllRows}) ->
	AllRows.

%% @doc Extract the Rows from MySQL Result on update
%%
%% @spec get_result_affected_rows(MySQLRes::yhsql_result()) ->
%%           AffectedRows::integer()
get_result_affected_rows(#yhsql_result{affectedrows=AffectedRows}) ->
	AffectedRows.

%% @doc Extract the error Reason from MySQL Result on error
%%
%% @spec get_result_reason(MySQLRes::yhsql_result()) ->
%%    Reason::string()
get_result_reason(#yhsql_result{error=Reason}) ->
	Reason.

%% @doc Extract the error ErrCode from MySQL Result on error
%%
%% @spec get_result_err_code(MySQLRes::yhsql_result()) ->
%%    ErrCode::integer()
get_result_err_code(#yhsql_result{errcode=ErrCode}) ->
	ErrCode.

%% @doc Extract the error ErrSqlState from MySQL Result on error
%%
%% @spec get_result_err_sql_state(MySQLRes::yhsql_result()) ->
%%    ErrSqlState::string()
get_result_err_sql_state(#yhsql_result{errsqlstate=ErrSqlState}) ->
	ErrSqlState.

%% @doc Extract the Insert Id from MySQL Result on update
%%
%% @spec get_result_insert_id(MySQLRes::yhsql_result()) ->
%%           InsertId::integer()
get_result_insert_id(#yhsql_result{insertid=InsertId}) ->
	InsertId.
