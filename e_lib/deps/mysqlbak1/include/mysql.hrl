%% MySQL result record:
-define(SECURE_CONNECTION, 32768).
-define(MYSQL_QUERY_OP, 3).
-define(DEFAULT_STANDALONE_TIMEOUT, 5000).
-define(MYSQL_4_0, 40). %% Support for MySQL 4.0.x
-define(MYSQL_4_1, 41). %% Support for MySQL 4.1.x et 5.0.x

%% Used by transactions to get the state variable for this connection
%% when bypassing the dispatcher.
-define(STATE_VAR, mysql_connection_state).

-define(Log(LogFun,Level,Msg),
	LogFun(?MODULE, ?LINE,Level,fun()-> {Msg,[]} end)).
-define(Log2(LogFun,Level,Msg,Params),
	LogFun(?MODULE, ?LINE,Level,fun()-> {Msg,Params} end)).
-define(L(Msg), io:format("~p:~b ~p ~n", [?MODULE, ?LINE, Msg])).

-define(RECV_TIMEOUT, 8 * 1000).

-record(mysql_result,
	{fieldinfo=[],
	 rows=[],
	 affectedrows=0,
	 insertid=0,
	 error="",
	 errcode=0,
	 errsqlstate=""}).


