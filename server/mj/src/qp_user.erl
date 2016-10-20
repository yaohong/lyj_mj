%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 十月 2016 19:49
%%%-------------------------------------------------------------------
-module(qp_user).
-author("yaohong").

-behaviour(gen_fsm).

-include("qp_type.hrl").
-include("../deps/file_log/include/file_log.hrl").
-include("qp_proto.hrl").
-include("../include/mj_pb.hrl").
%% API
-export([start_link/2]).

%% gen_fsm callbacks
-export([init/1,
         wait_login/2,              %%等待登陆
         hall/2,                    %%大厅
         room/2,                    %%房间
         game/2,                    %%游戏
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).
-export([head_len/2,
         closed/1,
         complete_packet/2,
         timer_callback/2
]).
-export([
    start/3,
    start_link/2
]).
-define(SERVER, ?MODULE).
-define(TIMER_SPACE, 3).
-define(RECV_TIMEOUT, 10).
-record(state, {
    receiveMonitor,
    sockModule,
    sockData,
    last_recv_packet_time}).

%%%===================================================================
%%% API
%%%===================================================================
head_len(HeadBin, _HeadLen) when is_binary(HeadBin) ->
    %%读取头
    <<PacketSize:?BIG_UINT16>> = HeadBin,
    PacketSize.

closed(Pid) when is_pid(Pid) ->
    Pid ! closed.

complete_packet(Pid, Bin) when is_pid(Pid) andalso is_binary(Bin) ->
    gen_fsm:send_all_state_event(Pid, {complete_packet, Bin}).

timer_callback(_Ref, Pid) ->
    Pid ! timeout_check.

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------


-spec start(UserSup :: atom(), SockModule :: module(), Socket :: tcp_socket:socket()) ->
    {ok, pid()}.
start(UserSup, SockModule, SocketData) ->
    supervisor:start_child(UserSup, [SockModule, SocketData]).

start_link(SockModule, Socket) ->
    gen_fsm:start_link(?MODULE, [SockModule, Socket], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, StateName :: atom(), StateData :: #state{}} |
    {ok, StateName :: atom(), StateData :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([SockModule, SocketData]) ->
    ReceiveMonitor = SockModule:monitor(SocketData),
    case catch SockModule:peername(SocketData) of
        {ok, _} ->
            CurrentTime = qp_util:timestamp(),
            timer_manager:addDelayTask(
                CurrentTime,
                CurrentTime + ?TIMER_SPACE,
                qp_user, timer_callback, [self()]),
            {ok,
             wait_login,
             #state{
                 receiveMonitor = ReceiveMonitor,
                 sockModule = SockModule,
                 sockData = SocketData,
                 last_recv_packet_time = CurrentTime
             }};
        Other ->
            ?FILE_LOG_ERROR("socket init peername fail reason=[~p]", [Other]),
            {stop, normal}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @end
%%--------------------------------------------------------------------
-spec(wait_login(Event :: term(), State :: #state{}) ->
    {next_state, NextStateName :: atom(), NextState :: #state{}} |
    {next_state, NextStateName :: atom(), NextState :: #state{},
     timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
wait_login(_Event, State) ->
    {next_state, state_name, State}.


hall(_Event, State) ->
    {next_state, state_name, State}.


room(_Event, State) ->
    {next_state, state_name, State}.


game(_Event, State) ->
    {next_state, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @end
%%--------------------------------------------------------------------
%%-spec(state_name(Event :: term(), From :: {pid(), term()},
%%                 State :: #state{}) ->
%%                    {next_state, NextStateName :: atom(), NextState :: #state{}} |
%%                    {next_state, NextStateName :: atom(), NextState :: #state{},
%%                     timeout() | hibernate} |
%%                    {reply, Reply, NextStateName :: atom(), NextState :: #state{}} |
%%                    {reply, Reply, NextStateName :: atom(), NextState :: #state{},
%%                     timeout() | hibernate} |
%%                    {stop, Reason :: normal | term(), NewState :: #state{}} |
%%                    {stop, Reason :: normal | term(), Reply :: term(),
%%                     NewState :: #state{}}).
%%state_name(_Event, _From, State) ->
%%    Reply = ok,
%%    {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_event(Event :: term(), StateName :: atom(),
                   StateData :: #state{}) ->
                      {next_state, NextStateName :: atom(), NewStateData :: #state{}} |
                      {next_state, NextStateName :: atom(), NewStateData :: #state{},
                       timeout() | hibernate} |
                      {stop, Reason :: term(), NewStateData :: #state{}}).
handle_event({complete_packet, Bin}, StateName, #state{last_recv_packet_time = OldLastRecvPacketTime} = State) ->
    Request = qp_proto:decode_qp_packet(Bin),
    {NewStateName, NewState, IsUpdate} = packet_handle(Request, StateName, State),
    NewLastRecvPacketTime =
        if
            IsUpdate =:= true -> qp_util:timestamp();
            true -> OldLastRecvPacketTime
        end,
    {next_state, NewStateName, NewState#state{last_recv_packet_time = NewLastRecvPacketTime}};
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_sync_event(Event :: term(), From :: {pid(), Tag :: term()},
                        StateName :: atom(), StateData :: term()) ->
                           {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term()} |
                           {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term(),
                            timeout() | hibernate} |
                           {next_state, NextStateName :: atom(), NewStateData :: term()} |
                           {next_state, NextStateName :: atom(), NewStateData :: term(),
                            timeout() | hibernate} |
                           {stop, Reason :: term(), Reply :: term(), NewStateData :: term()} |
                           {stop, Reason :: term(), NewStateData :: term()}).
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: term(), StateName :: atom(),
                  StateData :: term()) ->
                     {next_state, NextStateName :: atom(), NewStateData :: term()} |
                     {next_state, NextStateName :: atom(), NewStateData :: term(),
                      timeout() | hibernate} |
                     {stop, Reason :: normal | term(), NewStateData :: term()}).
handle_info(timeout_check, StateName, #state{last_recv_packet_time = LastRecvPackTime} = State) ->
    CurrentTime = qp_util:timestamp(),
    SpacheTime = CurrentTime - LastRecvPackTime,
    if
        SpacheTime > ?RECV_TIMEOUT ->
            ?FILE_LOG_DEBUG("timeout_check true , exit.", []),
            {stop,normal,State};
        true ->
            ?FILE_LOG_DEBUG("timeout_check space_time=~p , not timeout.", [SpacheTime]),
            timer_manager:addDelayTask(
                CurrentTime,
                CurrentTime + ?TIMER_SPACE,
                qp_user, timer_callback, [self()]),
            {next_state, StateName, State}
    end;
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: normal | shutdown | {shutdown, term()}
                | term(), StateName :: atom(), StateData :: term()) ->
                   term()).
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, StateName :: atom(),
                  StateData :: #state{}, Extra :: term()) ->
                     {ok, NextStateName :: atom(), NewStateData :: #state{}}).
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

packet_handle(#qp_login_req{account = Account}, wait_login, State) ->
    ?FILE_LOG_DEBUG("login_request, acc=~p", [Account]),
    {hall, State, true};
packet_handle(Request, wait_login, State) ->
    ?FILE_LOG_WARNING("wait_login request=~p", [Request]),
    {wait_login, State, false};


packet_handle(#qp_create_room_req{room_type = _} = Request, hall, State) ->
    ?FILE_LOG_WARNING("hall request=~p", [Request]),
    {room, State, true};
packet_handle(#qp_join_room_req{} = Request, hall, State) ->
    ?FILE_LOG_WARNING("hall request=~p", [Request]),
    {room, State, true};
packet_handle(#qp_ping_req{} = Request, hall, State) ->
    ?FILE_LOG_WARNING("hall request=~p", [Request]),
    {hall, State, true};
packet_handle(Request, hall, State) ->
    ?FILE_LOG_WARNING("hall request=~p", [Request]),
    {hall, State, false};


packet_handle(#qp_ready_req{}=Request, room, State) ->
    ?FILE_LOG_WARNING("room request=~p", [Request]),
    {room, State, true};
packet_handle(#qp_ping_req{} = Request, room, State) ->
    ?FILE_LOG_WARNING("room request=~p", [Request]),
    {room, State, true};
packet_handle(#qp_exit_room_req{} = Request, room, State) ->
    ?FILE_LOG_WARNING("room request=~p", [Request]),
    {hall, State, true};
packet_handle(Request, room, State) ->
    ?FILE_LOG_WARNING("room request=~p", [Request]),
    {room, State, false};


packet_handle(#qp_game_data{}=Request, game, State) ->
    ?FILE_LOG_WARNING("game request=~p", [Request]),
    {game, State, true};
packet_handle(#qp_ping_req{} = Request, game, State) ->
    ?FILE_LOG_WARNING("game request=~p", [Request]),
    {game, State, true};
packet_handle(#qp_exit_room_req{} = Request, room, State) ->
    ?FILE_LOG_WARNING("game request=~p", [Request]),
    {hall, State, true};
packet_handle(Request, game, State) ->
    ?FILE_LOG_WARNING("game request=~p", [Request]),
    {game, State, false}.