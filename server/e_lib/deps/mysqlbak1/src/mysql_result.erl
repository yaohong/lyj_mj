%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. 八月 2015 18:27
%%%-------------------------------------------------------------------
-module(mysql_result).
-author("yaohong").
-include("mysql.hrl").
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



get_result_field_info(#mysql_result{fieldinfo = FieldInfo}) ->
	FieldInfo.

%% @doc Extract the Rows from MySQL Result on data received
%%
%% @spec get_result_rows(MySQLRes::mysql_result()) -> [Row::list()]
get_result_rows(#mysql_result{rows=AllRows}) ->
	AllRows.

%% @doc Extract the Rows from MySQL Result on update
%%
%% @spec get_result_affected_rows(MySQLRes::mysql_result()) ->
%%           AffectedRows::integer()
get_result_affected_rows(#mysql_result{affectedrows=AffectedRows}) ->
	AffectedRows.

%% @doc Extract the error Reason from MySQL Result on error
%%
%% @spec get_result_reason(MySQLRes::mysql_result()) ->
%%    Reason::string()
get_result_reason(#mysql_result{error=Reason}) ->
	Reason.

%% @doc Extract the error ErrCode from MySQL Result on error
%%
%% @spec get_result_err_code(MySQLRes::mysql_result()) ->
%%    ErrCode::integer()
get_result_err_code(#mysql_result{errcode=ErrCode}) ->
	ErrCode.

%% @doc Extract the error ErrSqlState from MySQL Result on error
%%
%% @spec get_result_err_sql_state(MySQLRes::mysql_result()) ->
%%    ErrSqlState::string()
get_result_err_sql_state(#mysql_result{errsqlstate=ErrSqlState}) ->
	ErrSqlState.

%% @doc Extract the Insert Id from MySQL Result on update
%%
%% @spec get_result_insert_id(MySQLRes::mysql_result()) ->
%%           InsertId::integer()
get_result_insert_id(#mysql_result{insertid=InsertId}) ->
	InsertId.
