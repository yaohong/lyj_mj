%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 十月 2016 16:00
%%%-------------------------------------------------------------------
-module(qp_room_manager).
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
-export([create_room/2,
         get_room_pid/1]).
-define(SERVER, ?MODULE).

-record(state, {}).
-record(room_data, {
    room_id :: integer(),
    owner_user_id :: integer(),
    room_type :: integer(),
    create_time :: integer(),
    room_pid :: pid()
}).
%%%===================================================================
%%% API
%%%===================================================================
create_room(OwnerUserId, RoomType) ->
    gen_server:call(?MODULE, {create_room, {OwnerUserId, RoomType}}).

get_room_pid(RoomId) ->
    gen_server:call(?MODULE, {get_room_pid, RoomId}).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
    erlang:process_flag(trap_exit, true),
    random:seed(qp_util:timestamp()),
    ets:new(room_data, [set, protected, named_table, {keypos, #room_data.room_id}]),
    {ok, #state{}}.

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
handle_call({create_room, {OwnerUserId, RoomType}}, _From, State) ->
    Reply =
        case create_room() of
            failed -> failed;
            {success, RoomId} ->
                case qp_room:start_link(OwnerUserId, RoomId, RoomType) of
                    {ok, RoomPid} ->
                        ets:insert(
                            room_data,
                            #room_data{
                                room_id = RoomId,
                                owner_user_id = OwnerUserId,
                                room_type = RoomType,
                                create_time = qp_util:timestamp(),
                                room_pid = RoomPid}),
                        {success, {RoomId, RoomPid}};
                    Other ->
                        ?FILE_LOG_ERROR("user_id:~p, room_type:~p, ~p", [OwnerUserId, RoomType, Other]),
                        failed
                end
        end,
    {reply, Reply, State};
handle_call({get_room_pid, RoomId}, _From, State) ->
    Reply =
        case ets:lookup(room_data, RoomId) of
            [] -> failed;
            [#room_data{room_pid = RoomPid}] ->
                {success, RoomPid}
        end,
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

handle_info(Info, State) ->
    ?FILE_LOG_DEBUG("~p", [Info]),
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
create_room() ->
    create_room(100).

create_room(0) -> failed;
create_room(Count) ->
    RoomId = qp_util:random_in_range(100000, 999999),
    case ets:lookup(room_data, RoomId) of
        [] -> {success, RoomId};
        [_] -> create_room(Count - 1)
    end.
