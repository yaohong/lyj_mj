%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 三月 2016 16:06
%%%-------------------------------------------------------------------
-module(timer_work).
-author("yaohong").

-behaviour(gen_server).
-include("../deps/file_log/include/file_log.hrl").
%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).
-export([addDelayTask/5]).
-define(SERVER, ?MODULE).

-record(state, {tableId :: ets:tab()}).
-record(task, {delayTimer, taskDataList :: [{Mod :: atom(), Func :: atom(), Args :: list()}]}).
%%%===================================================================
%%% API
%%%===================================================================
addDelayTask(Pid, DelayTime, Mod, Func, Args) when is_pid(Pid) andalso is_integer(DelayTime) ->
	gen_server:call(Pid, {addDelayTask, {DelayTime, Mod, Func, Args}}).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
	gen_server:start_link(?MODULE, [], []).

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
init([]) ->
	TableId = ets:new(task, [ordered_set, {keypos, #task.delayTimer}]),
	start_timer(),
	{ok, #state{tableId = TableId}}.

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
handle_call({addDelayTask, {DelayTime, Mod, Func, Args}}, _From, #state{tableId = TableId} = State) ->
	Ref = make_ref(),
	case ets:lookup(TableId, DelayTime) of
		[] ->
			ets:insert(TableId, #task{delayTimer = DelayTime, taskDataList = [{Mod, Func, [Ref|Args]}]});
		[#task{taskDataList = OldTaskDataList}] ->
			ets:update_element(TableId, DelayTime, [{#task.taskDataList, [{Mod, Func, [Ref|Args]}|OldTaskDataList]}])
	end,
	{reply, {success, Ref}, State};
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
handle_info({timeout, _TimerRef, 'timerTask'}, #state{tableId = TableId} = State) ->
	CurrentTime = qp_util:timestamp(),
	TaskKeyList =
		case ets:first(TableId) of
			'$end_of_table' -> [];
			FirstKey ->
				if
					FirstKey > CurrentTime -> [];
					true -> task_extract(TableId, FirstKey, CurrentTime, [FirstKey])
				end
		end,
	lists:foreach(
		fun(TaskKey) ->
			[#task{taskDataList = TaskList}] = ets:lookup(TableId, TaskKey),
			lists:foreach(
				fun({Mod, Func, Args}) ->
						catch apply(Mod, Func, Args)
				end, TaskList),
			ets:delete(TableId, TaskKey)
		end, TaskKeyList),
	start_timer(),
	{noreply, State};
handle_info(_Info, State) ->
	{noreply, State}.


task_extract(TableId, Key, CurrentTime, TaskKeyList) ->
	case ets:next(TableId, Key) of
		'$end_of_table' -> TaskKeyList;
		NextKey when NextKey > CurrentTime -> TaskKeyList;
		NextKey when NextKey =:= CurrentTime -> [NextKey|TaskKeyList];
		NextKey -> task_extract(TableId, NextKey, CurrentTime, [NextKey|TaskKeyList])
	end.

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
	State :: #state{}) ->
	term()).
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
-define(TIMER_SPACE_TIME, 1 * 1000).
start_timer() ->
	erlang:start_timer(?TIMER_SPACE_TIME, self(), 'timerTask').