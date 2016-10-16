%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. 八月 2015 18:09
%%%-------------------------------------------------------------------
-module(yhsql_pool).
-author("yaohong").
-include("yhsql.hrl").
-behaviour(gen_server).

%% API
-export([start_link/9]).
-export([
	stop/1,
	get_con/1
]).
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
	con_list :: [pid()],
	log_fun :: function()
}).

%%%===================================================================
%%% API
%%%===================================================================
stop(PoolName) ->
	gen_server:call(PoolName, stop).


get_con(PoolName) ->
	gen_server:call(PoolName, get_con).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(
					PoolName :: atom(),
					PoolSize :: integer(),
					Host :: string(),
					Port :: integer(),
					User :: string(),
					Password :: string(),
					Database :: string(),
					LogFunc :: function(),
					Timeout :: integer()
				) ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(PoolName, PoolSize, Host, Port, User, Password, Database, LogFunc, Timeout) ->
	gen_server:start_link({local, PoolName}, ?MODULE, [PoolSize, Host, Port, User, Password, Database, LogFunc, Timeout], []).

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
init([PoolSize, Host, Port, User, Password, Database, LogFunc, Timeout]) ->
	ConList =
	lists:map(
		fun(_Index) ->
			{ok, Pid} = yhsql_conn:start_link(Host, Port, User, Password, Database, LogFunc, Timeout),
			Pid
		end, lists:seq(1, PoolSize)),
	{ok, #state{con_list = ConList, log_fun = LogFunc}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},State :: #state{}) ->
	{reply, Reply :: term(), NewState :: #state{}} |
	{reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_call(get_con, _From, #state{con_list = [H|T]} = State) ->
	{reply, {success, H}, State#state{con_list = T ++ [H]}};
handle_call(stop, _From, #state{con_list = ConList} = State) ->
	lists:foreach(
		fun(ConPid) ->
			ConPid ! exit
	    end, ConList),
	{stop, normal, success, State};
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
