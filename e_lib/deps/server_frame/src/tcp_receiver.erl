-module(tcp_receiver).
-author('erlangonrails@gmail.com').
-include("file_log.hrl").
-behaviour(gen_server).

%% API
-export([
	start_link/4,
	start/4, start/5,
	become_controller/2,
	close/1]).

%% gen_server callbacks
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
         terminate/2, 
         code_change/3]).

-record(state, {socket :: gen_tcp:socket(),
                user_pid :: pid(),  %% halld_user | halld_room 
                user_handle_module :: module(),
                packet_head_len :: integer(),
                socket_data = <<>> :: binary()}).

-define(HIBERNATE_TIMEOUT, 90000).

%% simple_one_for_one的启动回调
-spec start_link(
        UserHandle :: module(),
        PacketHandle :: module(),
        Socket :: gen_tcp:socket(),
        MaxStanzaSize :: integer() | infinity) ->
    {ok, pid()}.
start_link(UserHandle, PacketHandle, Socket, MaxStanzaSize) ->
    gen_server:start_link(?MODULE, [UserHandle, PacketHandle, Socket, MaxStanzaSize], []).


-spec start(
    UserHandle :: module(), 
    ReceiveSup :: module(),
	PacketHeadLen :: integer(),
    Socket :: gen_tcp:socket()) -> {ok, pid()}.
start(UserHandle, ReceiveSup, PacketHeadLen, Socket) ->
    start(UserHandle, ReceiveSup, PacketHeadLen, Socket, infinity).

-spec start(
        UserHandle :: module(),
        ReceiveSup :: module(),
		PacketHeadLen :: integer(),
        Socket :: gen_tcp:socket(), 
        MaxStanzaSize :: integer() | infinity) ->
    {ok, pid()}.
start(UserHandle, ReceiveSup, PacketHeadLen, Socket, MaxStanzaSize) ->
    {ok, Pid} = supervisor:start_child(ReceiveSup, [UserHandle, PacketHeadLen, Socket, MaxStanzaSize]),
    {ok, Pid}.

%% active socket开始真正接收数据
-spec become_controller(Pid :: pid(), UserPid :: pid()) -> ok.
become_controller(Pid, UserPid) when is_pid(UserPid) ->
    gen_server:call(Pid, {become_controller, UserPid}).

-spec close(Pid :: pid()) -> ok.
close(Pid) ->
    gen_server:cast(Pid, close).

%% gen_server callbacks

init([UserHandle, PacketHeadLen, Socket, _MaxStanzaSize]) ->
    {ok, #state{
            socket = Socket,
            user_handle_module = UserHandle,
            packet_head_len = PacketHeadLen}}.

handle_call({become_controller, UserPid}, _From, State) ->
    NewState = State#state{user_pid = UserPid},
    activate_socket(NewState),
    Reply = ok,
    {reply, Reply, NewState, ?HIBERNATE_TIMEOUT};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State, ?HIBERNATE_TIMEOUT}.

handle_cast(close, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State, ?HIBERNATE_TIMEOUT}.

handle_info({'tcp', _TCPSocket, Data}, #state{socket_data = OldData} = State) ->
    case process_packet(iolist_to_binary([OldData, Data]), State) of
        {ok, NewState} ->
            {noreply, NewState, ?HIBERNATE_TIMEOUT};
        {error, Reason} ->
            {stop, Reason, State}
    end;
handle_info({'tcp_closed', _TCPSocket}, State) ->
    {stop, normal, State};
handle_info({'tcp_error', _TCPSocket, Reason}, State) ->
    case Reason of
        timeout ->
            {noreply, State, ?HIBERNATE_TIMEOUT};
        _ ->
            {stop, normal, State}
    end;
handle_info(timeout, State) ->
    %% 一段时间内没有TCP数据, 休眠, 节省内存
    proc_lib:hibernate(gen_server, enter_loop, [?MODULE, [], State]),
    {noreply, State, ?HIBERNATE_TIMEOUT};
handle_info(_Info, State) ->
    {noreply, State, ?HIBERNATE_TIMEOUT}.

terminate(_Reason, #state{user_pid = UserPid, user_handle_module = UserModule} = State) ->
    if
        UserPid /= undefined ->
            %% 停止对应的halld_user | halld_room进程
            UserModule:closed(UserPid);
            %%catch gen_fsm:send_event(UserPid, closed);
        true ->
            ok
    end,
    catch gen_tcp:close(State#state.socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

activate_socket(#state{socket = Socket}) ->
    inet:setopts(Socket, [{active, once}]),
    case inet:peername(Socket) of
        {error, _Reason} ->
            self() ! {tcp_closed, Socket};
        {ok, _} ->
            ok
    end.

%% 解析数据包的核心逻辑
-spec process_packet(binary(), #state{}) ->
    {ok, #state{}} | {error, any()}.
process_packet(
    Data, 
    #state{user_pid = UserPid, 
            user_handle_module = UserModule,
			packet_head_len = PacketHeadLen} = State) ->
	try
		case decode_message(Data, PacketHeadLen) of
			{continue, DataRes} ->
				activate_socket(State),
				{ok, State#state{socket_data = DataRes}};
			{CompleteBin, DataRes} when is_binary(CompleteBin) andalso is_binary(DataRes) ->
				UserModule:complete_packet(UserPid, CompleteBin),
				process_packet(DataRes, State)
		end
	catch
		Type:What ->
			?FILE_LOG_DEBUG("decode_message exception Type=~p What=~p stack=~p", [Type, What, erlang:get_stacktrace()]),
			{error, message_parse_error}
	end.

decode_message(Data, PacketHeadLen) when size(Data) < PacketHeadLen ->
	{continue, Data};
decode_message(Data, PacketHeadLen) ->
	SizeLen = PacketHeadLen * 8,
	<<Len:SizeLen/unsigned-integer-little, DataRest/binary>> = Data,
	case size(DataRest) < Len of
		true ->
			{continue, Data};
		false ->
			<<CompleteBin:Len/binary, DataRest1/binary>> = DataRest,
			{CompleteBin, DataRest1}
	end.
