-module(netlib_client_conn).
-author('erlangonrails@gmail.com').
-include("netlib.hrl").
-include("file_log.hrl").
-behaviour(gen_fsm).

%% API
-export([start_link/5,
         do_send/2,
         do_recv/2]).
-export([ping/1]).

%% gen_fsm callbacks
-export([init/1, 
         connecting/2, 
         connecting/3,
         connected/2,
         connected/3,
         handle_event/3,
         handle_sync_event/4, 
         handle_info/3, 
         terminate/3, 
         code_change/4]).

-record(state, {timerref :: netlib_reference(),              %% ping包的定时器
                socket :: netlib_socket(),                   %% TCP Socket
                address :: netlib_address(),          
                addressid :: netlib_binary(),
                module :: netlib_module(),
                module_state :: netlib_term(),
                opts :: netlib_proplist(),
                start_interval :: netlib_non_neg_integer(),  %% 超时重连的间隔(second)
                ping_interval :: netlib_timeout()}).         %% 发送ping包的间隔(second)

-spec ping(Pid :: netlib_pid()) ->
    ok.
ping(Pid) ->
    gen_fsm:sync_send_event(Pid, {cmd, do_ping}, infinity).

-spec start_link(Address :: netlib_address(),
                 StartInterval :: netlib_non_neg_integer(),   %% second
                 PingInterval :: netlib_timeout(),            %% second
                 Module :: netlib_module(),
                 Opts :: netlib_proplist()) -> 
    netlib_ret_ok(netlib_pid()).
start_link(Address, StartInterval, PingInterval, Module, Opts) ->
    gen_fsm:start_link(?MODULE, 
                       [Address, StartInterval, PingInterval, Module, Opts], 
                       []).


init([Address, StartInterval, PingInterval, Module, Opts]) ->
    gen_fsm:send_event(self(), connect),
    {ok, connecting, #state{address = Address,
                            addressid = netlib_util:address_to_binary(Address),
                            module = Module,
                            opts = Opts,
                            start_interval = StartInterval,
                            ping_interval = PingInterval}}.

connecting(connect, #state{address = {Ip, Port} = Address,
                           addressid = AddressId,
                           module = Module,
                           opts = Opts} = State) ->
    {NewTcpOpts, NewOpts} = process_opts(?NETLIB_CLIENT_TCP_OPTS, Opts),
    case gen_tcp:connect(Ip,
                         Port,
                         NewTcpOpts,
                         ?NETLIB_CLIENT_TCP_SEND_TIMEOUT) of
        {ok, Socket} ->
            netlib_client_conn_sup:add_pid(AddressId, self()),
            %% connecting success
            {ok, ModuleState} = Module:tcp_start(ok, Socket, Address, NewOpts),
            {ok, TRef} = timer:apply_interval(State#state.ping_interval * 1000, 
                                              ?MODULE, ping, [self()]),
            {next_state, connected, State#state{socket = Socket,
                                                timerref = TRef,
                                                module_state = ModuleState,
                                                opts = NewOpts}};
        {error, Reason} ->
            %% connecting failed, retry after some seconds
            {ok, ModuleState} = Module:tcp_start(
                                    {error, Reason}, State#state.start_interval, Address, NewOpts),
            gen_fsm:send_event_after(State#state.start_interval * 1000, connect),
            {next_state, connecting, State#state{module_state = ModuleState}}
    end;
connecting(_Event, State) ->
    {next_state, connecting, State}.

connecting(_Event, _From, State) ->
    {reply, ok, connecting, State}.

connected(_Event, State) ->
    {next_state, connected, State}.

connected({cmd, {do_send_recv, Term}}, _From, 
          #state{module = Module,
                 module_state = ModuleState} = State) ->
    {ok, NewModuleState, RecvData} = Module:do_send_recv(Term, ModuleState),
    {reply, {ok, RecvData}, connected, State#state{module_state = NewModuleState}};
connected({cmd, do_ping}, _From, 
          #state{module = Module,
                 module_state = ModuleState} = State) ->
    {ok, NewModuleState} = Module:do_ping(ModuleState),
    {reply, ok, connected, State#state{module_state = NewModuleState}};
connected(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, connected, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(Reason, _StateName, #state{timerref = TRef,
                                     addressid = AddressId,
                                     module = Module} = State) ->
    ?FILE_LOG_INFO("$netlib$ netlib_client_conn terminate due to:~p~n", [Reason]),
    netlib_client_conn_sup:delete_pid(AddressId, self()),
    catch gen_tcp:close(State#state.socket),
    catch timer:cancel(TRef),
    Module:tcp_close(State#state.module_state),
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.


%% Internal APIs:

%% =======================
%% @doc
%% 发送数据
%%
%% 注意:
%% 调用失败返回exit(_)
%% =======================
-spec do_send(Socket :: netlib_socket(),
              Data :: netlib_iolist()) ->
    ok.
do_send(Socket, Data) ->
    case gen_tcp:send(Socket, Data) of
        ok ->
            ok;
        {error, Reason} ->
            ?FILE_LOG_INFO("$netlib$ netlib_client_conn_do_send_error:~p", 
                           [Reason]),
            exit({error, Reason})
    end.

%% =======================
%% @doc
%% 接收数据
%%
%% 注意:
%% 调用失败返回exit(_)
%% ========================
-spec do_recv(Socket :: netlib_socket(),
              Length :: netlib_integer()) ->
    netlib_binary() | netlib_list().
do_recv(Socket, Length) ->
    case gen_tcp:recv(Socket, Length, ?NETLIB_CLIENT_TCP_RECV_TIMEOUT) of
        {ok, Data} ->
            Data;
        {error, Reason} ->
            ?FILE_LOG_INFO("$netlib$ netlib_client_conn_do_recv_error:~p", 
                           [Reason]),
        exit({error, Reason})
    end.

%% the same internal apis as netlib_server_listener module
-spec process_opts(TcpOpts :: netlib_proplist(),
                   Opts :: netlib_proplist()) ->
    {NewTcpOpts :: netlib_proplist(),
     NewOpts :: netlib_proplist()}.
process_opts(TcpOpts, Opts) ->
    case proplists:get_value(list, Opts) of
        true ->
            NewTcpOpts = [list | proplists:delete(binary, TcpOpts)],
            NewOpts = proplists:delete(list, Opts),
            {NewTcpOpts, NewOpts};
        undefined ->
            {TcpOpts, Opts}
    end.
