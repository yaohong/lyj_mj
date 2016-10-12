%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. 八月 2015 18:39
%%%-------------------------------------------------------------------
-module(mysql_conn).
-author("yaohong").

-behaviour(gen_server).
-include("mysql.hrl").
%% API
-export([start_link/6]).
-export([
	fetch/2,
	transaction/2
]).

-export([do_query/4]).
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
	socket,
	mysql_version,
	log_fun
}).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Host :: string(), Port :: integer(), User :: string(), Password :: string(), Database :: string(), LogFunc :: function()) ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Host, Port, User, Password, Database, LogFunc) ->
	gen_server:start_link(?MODULE, [Host, Port, User, Password, Database, LogFunc], []).




fetch(Pid, Querie)  ->
	gen_server:call(Pid, {fetch, Querie}).


transaction(Pid, Func) ->
	gen_server:call(Pid, {transaction, Func}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
	{ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([Host, Port, User, Password, Database, LogFunc]) ->
	{ok, Sock} = gen_tcp:connect(Host, Port, [binary, {packet, 0}, {active, false}], 3 * 1000),
	{ok, Version} = mysql_init(Sock, User, Password, LogFunc),
	Db = iolist_to_binary(Database),
	case do_query(Sock, LogFunc,<<"use `", Db/binary, "`">>, Version) of
		{error, MySQLRes} ->
			?Log2(LogFunc, error,
			      "mysql_conn: Failed changing to database "
			      "~p : ~p",
			      [Database, mysql_result:get_result_reason(MySQLRes)]),
			{stop, normal};
		{_ResultType, _MySQLRes} ->
%% 			case Encoding of
%% 				undefined -> undefined;
%% 				_ ->
%% 					EncodingBinary = list_to_binary(atom_to_list(Encoding)),
%% 					do_query(Sock, RecvPid, LogFun,
%% 					         <<"set names '", EncodingBinary/binary, "'">>,
%% 					         Version)
%% 			end,
			put(mysql_context, {Sock, LogFunc, Version}),
			State =
				#state{
					mysql_version=Version,
					socket   = Sock,
					log_fun  = LogFunc
				},
			{ok, State}
	end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #state{}) ->
	                 {reply, Reply :: term(), NewState :: #state{}} |
	                 {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
	                 {noreply, NewState :: #state{}} |
	                 {noreply, NewState :: #state{}, timeout() | hibernate} |
	                 {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
	                 {stop, Reason :: term(), NewState :: #state{}}).
handle_call({fetch, Querie}, _From, #state{socket = Socket, log_fun = LogFunc, mysql_version = Version} = State) ->
	Reply = do_query(Socket, LogFunc, Querie, Version),
	{reply, Reply, State};
handle_call({transaction, Func}, _From, #state{socket = Socket, log_fun = LogFunc, mysql_version = Version} = State) ->
	Reply = do_transaction(Socket, LogFunc, Version, Func),
	{reply, Reply, State};
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_info(exit, #state{socket = Socket, log_fun = LogFun} = State) ->
	?Log2(LogFun, debug, "mysql connect exit", []),
	catch gen_tcp:close(Socket),
	{stop, normal, State};
handle_info(_Info, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #state{}) -> term()).
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
                  Extra :: term()) ->
	                 {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

mysql_init(Sock, User, Password, LogFun) ->
	{success, {Packet, InitSeqNum}} = mysql_socket:do_recv(LogFun, Sock, undefined),
	{Version, Salt1, Salt2, Caps} = greeting(Packet, LogFun),
	AuthRes =
		case Caps band ?SECURE_CONNECTION of
			?SECURE_CONNECTION ->
				mysql_auth:do_new_auth(Sock, InitSeqNum + 1, User, Password, Salt1, Salt2, LogFun);
			_ ->
				mysql_auth:do_old_auth(Sock, InitSeqNum + 1, User, Password, Salt1, LogFun)
		end,
	case AuthRes of
		{success, {<<0:8, _Rest/binary>>, _RecvNum}} ->
			{ok,Version};
		{success, {<<255:8, Rest/binary>>, _RecvNum}} ->
			{Code, ErrData} = get_error_data(Rest, Version),
			?Log2(LogFun, error, "init error ~p: ~p",
			      [Code, ErrData]),
			{error, ErrData};
		{success, {RecvPacket, _RecvNum}} ->
			?Log2(LogFun, error,
			      "init unknown error ~p",
			      [binary_to_list(RecvPacket)]),
			{error, binary_to_list(RecvPacket)}
	end.




greeting(Packet, LogFun) ->
	<<Protocol:8, Rest/binary>> = Packet,
	{Version, Rest2} = asciz(Rest),
	<<_TreadID:32/little, Rest3/binary>> = Rest2,
	{Salt, Rest4} = asciz(Rest3),
	<<Caps:16/little, Rest5/binary>> = Rest4,
	<<ServerChar:16/binary-unit:8, Rest6/binary>> = Rest5,
	{Salt2, _Rest7} = asciz(Rest6),
	?Log2(LogFun, debug,
	      "greeting version ~p (protocol ~p) salt ~p caps ~p serverchar ~p"
	      "salt2 ~p",
	      [Version, Protocol, Salt, Caps, ServerChar, Salt2]),
	{normalize_version(Version, LogFun), Salt, Salt2, Caps}.


asciz(Data) when is_binary(Data) ->
	asciz_binary(Data, []);
asciz(Data) when is_list(Data) ->
	{String, [0 | Rest]} = lists:splitwith(fun (C) ->
		C /= 0
	                                       end, Data),
	{String, Rest}.

asciz_binary(<<>>, Acc) ->
	{lists:reverse(Acc), <<>>};
asciz_binary(<<0:8, Rest/binary>>, Acc) ->
	{lists:reverse(Acc), Rest};
asciz_binary(<<C:8, Rest/binary>>, Acc) ->
	asciz_binary(Rest, [C | Acc]).


normalize_version([$4,$.,$0|_T], LogFun) ->
	?Log(LogFun, debug, "switching to MySQL 4.0.x protocol."),
	?MYSQL_4_0;
normalize_version([$4,$.,$1|_T], _LogFun) ->
	?MYSQL_4_1;
normalize_version([$5|_T], _LogFun) ->
	%% MySQL version 5.x protocol is compliant with MySQL 4.1.x:
	?MYSQL_4_1;
normalize_version(_Other, LogFun) ->
	?Log(LogFun, error, "MySQL version not supported: MySQL Erlang module "
	                    "might not work correctly."),
	%% Error, but trying the oldest protocol anyway:
	?MYSQL_4_0.

get_error_data(ErrPacket, ?MYSQL_4_0) ->
	<<Code:16/little, Message/binary>> = ErrPacket,
	{Code, binary_to_list(Message)};
get_error_data(ErrPacket, ?MYSQL_4_1) ->
	<<Code:16/little, _M:8, SqlState:5/binary, Message/binary>> = ErrPacket,
	{Code, {binary_to_list(SqlState), binary_to_list(Message)}}.


do_query(Sock, LogFun, Query, Version) ->
	Query1 = iolist_to_binary(Query),
	%%?Log2(LogFun, debug, "fetch ~p (id ~p)", [Query1,RecvPid]),
	Packet =  <<?MYSQL_QUERY_OP, Query1/binary>>,
	mysql_socket:do_send(Sock, Packet, 0, LogFun),
	get_query_response(LogFun,Sock,Version).


get_query_response(LogFun, Sock, Version) ->
	{success, {Packet, _Num}} = mysql_socket:do_recv(LogFun, Sock, undefined),
	{Fieldcount, Rest} = get_lcb(Packet),
	case Fieldcount of
		0 ->
			%% No Tabular data
			{AffectedRows, Rest2} = get_lcb(Rest),
			{InsertId, _} = get_lcb(Rest2),
			{updated, #mysql_result{affectedrows=AffectedRows, insertid=InsertId}};
		255 ->
			case get_error_data(Rest, Version) of
				{Code, {SqlState, Message}} ->
					% MYSQL_4_1 error data
					{error, #mysql_result{error=Message,
					                      errcode=Code,
					                      errsqlstate=SqlState}};
				{Code, Message} ->
					% MYSQL_4_0 error data
					{error, #mysql_result{error=Message,
					                      errcode=Code}}
			end;
		_ ->
			%% Tabular data received
			case get_fields(LogFun, Sock, [], Version) of
				{ok, Fields} ->
					case get_rows(Fields, LogFun, Sock, [], Version) of
						{ok, Rows} ->
							{data, #mysql_result{fieldinfo=Fields,
							                     rows=Rows}};
						{error, {Code, {SqlState, Message}}} ->
							% MYSQL_4_1 error data
							{error, #mysql_result{error=Message,
							                      errcode=Code,
							                      errsqlstate=SqlState}};
						{error, {Code, Message}} ->
							% MYSQL_4_0 error data
							{error, #mysql_result{error=Message,
							                      errcode=Code}}
					end;
				{error, Reason} ->
					{error, #mysql_result{error=Reason}}
			end
	end.

do_transaction(Sock, LogFun, Version, Fun) ->
	case do_query(Sock, LogFun, <<"BEGIN">>, Version) of
		{error, _} = Err ->
			{aborted, Err};
		_ ->
			case catch Fun() of
				error = Err -> rollback(Sock, LogFun, Version, Err);
				{error, _} = Err -> rollback(Sock, LogFun, Version, Err);
				{'EXIT', _} = Err -> rollback(Sock, LogFun, Version, Err);
				Res ->
					case do_query(Sock, LogFun, Version, <<"COMMIT">>) of
						{error, _} = Err ->
							rollback(Sock, LogFun, Version, {commit_error, Err});
						_ ->
							case Res of
								{atomic, _} -> Res;
								_ -> {atomic, Res}
							end
					end
			end
	end.

rollback(Sock, LogFun, Version, Err) ->
	Res = do_query(Sock, LogFun, <<"ROLLBACK">>, Version),
	{aborted, {Err, {rollback_result, Res}}}.





get_lcb(<<251:8, Rest/binary>>) ->
	{null, Rest};
get_lcb(<<252:8, Value:16/little, Rest/binary>>) ->
	{Value, Rest};
get_lcb(<<253:8, Value:24/little, Rest/binary>>) ->
	{Value, Rest};
get_lcb(<<254:8, Value:32/little, Rest/binary>>) ->
	{Value, Rest};
get_lcb(<<Value:8, Rest/binary>>) when Value < 251 ->
	{Value, Rest};
get_lcb(<<255:8, Rest/binary>>) ->
	{255, Rest}.


get_fields(LogFun, Sock, Res, ?MYSQL_4_0) ->
	{success, {Packet, _Num}} = mysql_socket:do_recv(LogFun, Sock, undefined),
	case Packet of
		<<254:8>> ->
			{ok, lists:reverse(Res)};
		<<254:8, Rest/binary>> when size(Rest) < 8 ->
			{ok, lists:reverse(Res)};
		_ ->
			{Table, Rest} = get_with_length(Packet),
			{Field, Rest2} = get_with_length(Rest),
			{LengthB, Rest3} = get_with_length(Rest2),
			LengthL = size(LengthB) * 8,
			<<Length:LengthL/little>> = LengthB,
			{Type, Rest4} = get_with_length(Rest3),
			{_Flags, _Rest5} = get_with_length(Rest4),
			This = {Table,
			        Field,
			        Length,
			        %% TODO: Check on MySQL 4.0 if types are specified
			        %%       using the same 4.1 formalism and could
			        %%       be expanded to atoms:
			        Type},
			get_fields(LogFun, Sock, [This | Res], ?MYSQL_4_0)
	end;
%% Support for MySQL 4.1.x and 5.x:
get_fields(LogFun, Sock, Res, ?MYSQL_4_1) ->
	{success, {Packet, _Num}} = mysql_socket:do_recv(LogFun, Sock, undefined),
	case Packet of
		<<254:8>> ->
			{ok, lists:reverse(Res)};
		<<254:8, Rest/binary>> when size(Rest) < 8 ->
			{ok, lists:reverse(Res)};
		_ ->
			{_Catalog, Rest} = get_with_length(Packet),
			{_Database, Rest2} = get_with_length(Rest),
			{Table, Rest3} = get_with_length(Rest2),
			%% OrgTable is the real table name if Table is an alias
			{_OrgTable, Rest4} = get_with_length(Rest3),
			{Field, Rest5} = get_with_length(Rest4),
			%% OrgField is the real field name if Field is an alias
			{_OrgField, Rest6} = get_with_length(Rest5),

			<<_Metadata:8/little, _Charset:16/little,
			  Length:32/little, Type:8/little,
			  _Flags:16/little, _Decimals:8/little,
			  _Rest7/binary>> = Rest6,

			This = {Table,
			        Field,
			        Length,
			        get_field_datatype(Type)},
			get_fields(LogFun, Sock, [This | Res], ?MYSQL_4_1)
	end.

get_with_length(Bin) when is_binary(Bin) ->
	{Length, Rest} = get_lcb(Bin),
	case get_lcb(Bin) of
		{null, Rest} -> {null, Rest};
		_ -> split_binary(Rest, Length)
	end.


get_rows(Fields, LogFun, Sock, Res, Version) ->
	{success, {Packet, _Num}} = mysql_socket:do_recv(LogFun, Sock, undefined),
	case Packet of
		<<254:8, Rest/binary>> when size(Rest) < 8 ->
			{ok, lists:reverse(Res)};
		<<255:8, Rest/binary>> ->
			{Code, ErrData} = get_error_data(Rest, Version),
			{error, {Code, ErrData}};
		_ ->
			{ok, This} = get_row(Fields, Packet, []),
			get_rows(Fields, LogFun, Sock, [This | Res], Version)
	end.

%% part of get_rows/4
get_row([], _Data, Res) ->
	{ok, lists:reverse(Res)};
get_row([Field | OtherFields], Data, Res) ->
	{Col, Rest} = get_with_length(Data),
	This = case Col of
		       null ->
			       undefined;
		       _ ->
			       convert_type(Col, element(4, Field))
	       end,
	get_row(OtherFields, Rest, [This | Res]).



get_field_datatype(0) ->   'DECIMAL';
get_field_datatype(1) ->   'TINY';
get_field_datatype(2) ->   'SHORT';
get_field_datatype(3) ->   'LONG';
get_field_datatype(4) ->   'FLOAT';
get_field_datatype(5) ->   'DOUBLE';
get_field_datatype(6) ->   'NULL';
get_field_datatype(7) ->   'TIMESTAMP';
get_field_datatype(8) ->   'LONGLONG';
get_field_datatype(9) ->   'INT24';
get_field_datatype(10) ->  'DATE';
get_field_datatype(11) ->  'TIME';
get_field_datatype(12) ->  'DATETIME';
get_field_datatype(13) ->  'YEAR';
get_field_datatype(14) ->  'NEWDATE';
get_field_datatype(246) -> 'NEWDECIMAL';
get_field_datatype(247) -> 'ENUM';
get_field_datatype(248) -> 'SET';
get_field_datatype(249) -> 'TINYBLOB';
get_field_datatype(250) -> 'MEDIUM_BLOG';
get_field_datatype(251) -> 'LONG_BLOG';
get_field_datatype(252) -> 'BLOB';
get_field_datatype(253) -> 'VAR_STRING';
get_field_datatype(254) -> 'STRING';
get_field_datatype(255) -> 'GEOMETRY'.

convert_type(Val, ColType) ->
	case ColType of
		T when T == 'TINY';
		       T == 'SHORT';
		       T == 'LONG';
		       T == 'LONGLONG';
		       T == 'INT24';
		       T == 'YEAR' ->
			list_to_integer(binary_to_list(Val));
		T when T == 'TIMESTAMP';
		       T == 'DATETIME' ->
			{ok, [Year, Month, Day, Hour, Minute, Second], _Leftovers} =
				io_lib:fread("~d-~d-~d ~d:~d:~d", binary_to_list(Val)),
			{datetime, {{Year, Month, Day}, {Hour, Minute, Second}}};
		'TIME' ->
			{ok, [Hour, Minute, Second], _Leftovers} =
				io_lib:fread("~d:~d:~d", binary_to_list(Val)),
			{time, {Hour, Minute, Second}};
		'DATE' ->
			{ok, [Year, Month, Day], _Leftovers} =
				io_lib:fread("~d-~d-~d", binary_to_list(Val)),
			{date, {Year, Month, Day}};
		T when T == 'DECIMAL';
		       T == 'NEWDECIMAL';
		       T == 'FLOAT';
		       T == 'DOUBLE' ->
			{ok, [Num], _Leftovers} =
				case io_lib:fread("~f", binary_to_list(Val)) of
					{error, _} ->
						io_lib:fread("~d", binary_to_list(Val));
					Res ->
						Res
				end,
			Num;
		_Other ->
			Val
	end.




