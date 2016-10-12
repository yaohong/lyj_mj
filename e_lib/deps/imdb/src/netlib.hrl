-author('erlangonrails@gmail.com').

%% assert
-define(NETLIB_ASSERT(A, B), 
    case A =:= B of
        true -> 
            ok;
        false -> 
            exit(io_lib:format(
                     "netlib_assert_error:~p#~p~n", [A, B]))
    end).


%% uuid config
-define(NETLIB_DEFAULT_UUID_ALGORITHM, 'random').

%% logger config
-define(NETLIB_LOGMOD, 'netlib_dynamic_log').
-define(NETLIB_DEFAULT_LOG, "netlib.log").
-define(NETLIB_DEFAULT_LOG_LEVEL, debug).
-define(NETLIB_DEFAULT_LOG_ROTATE_INTERVAL, 21600). %% 6 hours - 60*60*6


%% return value format
-define(NETLIB_RET_OK(Term), {ok, Term}).
-define(NETLIB_RET_ERROR(Term), {error, Term}).
-type netlib_ret_ok(Term) :: {ok, Term}.
-type netlib_ret_error(Term) :: {error, Term}.
-type netlib_ret(Term) :: netlib_ret_ok(Term) | 
                          netlib_ret_error(Term).

%% integer
-define(NETLIB_UINT8,   8/big-integer).
-define(NETLIB_UINT16, 16/big-integer).
-define(NETLIB_UINT24, 24/big-integer).
-define(NETLIB_UINT32, 32/big-integer).
-define(NETLIB_UINT48, 48/big-integer).


%% netlib basic type
-type netlib_term() :: any().
-type netlib_atom() :: atom().
-type netlib_list() :: [netlib_term()].
-type netlib_tuple() :: tuple().
-type netlib_binary() :: binary().
-type netlib_bool() :: boolean().
-type netlib_integer() :: integer().
-type netlib_float() :: float().
-type netlib_number() :: integer() | float().
-type netlib_pid() :: pid().
-type netlib_fun() :: fun().
-type netlib_byte() :: 0..255.
-type netlib_char() :: 0..16#10ffff.
-type netlib_pos_integer() :: pos_integer().         %% 1..
-type netlib_non_neg_integer() :: non_neg_integer(). %% 0..
-type netlib_neg_integer() :: neg_integer().         %% ..-1
-type netlib_string() :: [netlib_char()].            %% Unicode Code Point list
-type netlib_iolist() :: iolist().
-type netlib_nonempty_string() :: [netlib_char(), ...].
-type netlib_timeout() :: 'infinity' | netlib_non_neg_integer().
-type netlib_module() :: netlib_atom().
-type netlib_reference() :: reference().
-type netlib_proplist() :: [{netlib_term(), netlib_term()} | netlib_atom()].
-type netlib_socket() :: any(). %% gen_tcp socket
-type netlib_fd() :: any().     %% file handler
-type netlib_log_level() :: error | warning| info | debug.
-type netlib_uuid_algorithm() :: random | utc_random | sequential.
-type netlib_event_handler_type() :: async | sync.
-type netlib_event_handler_mode() :: no_queue |
                                     one_queue |
                                     {queues, netlib_non_neg_integer()}.

%% ip & port
-type netlib_port() :: integer().
-type netlib_ip()   :: {integer(), integer(), integer(), integer()} | %% ipv4
                       {integer(), integer(), integer(), integer(),   %% ipv6
                        integer(), integer(), integer(), integer()}.  
-type netlib_address() :: {netlib_ip(), netlib_port()}.

%% netlib_gen_request_server config
-define(NETLIB_REQUEST_SERVER_TCP_SEND_TIMEOUT, 15000). %% 15 seconds
-define(NETLIB_REQUEST_SERVER_TCP_RECV_TIMEOUT, 15000). %% 15 seconds
-define(NETLIB_REQUEST_SERVER_TCP_OPTS(Ip), 
        [binary,
         {ip, Ip},
         {packet, 0},
         {active, false},
         {reuseaddr, true},
         {nodelay, false},
         {backlog, 128},
         {send_timeout, ?NETLIB_REQUEST_SERVER_TCP_SEND_TIMEOUT},
         {keepalive, true}]).


%% netlib_gen_server config
-define(NETLIB_SERVER_TCP_SEND_TIMEOUT, 15000). %% 15 seconds
-define(NETLIB_SERVER_TCP_OPTS(Ip), 
        [binary,
         {ip, Ip}, 
         {packet, 0},
         {active, false},
         {reuseaddr, true},
         {nodelay, true},
         {send_timeout, ?NETLIB_SERVER_TCP_SEND_TIMEOUT},
         {keepalive, true}]).

-record(server_socket_state, {socket :: netlib_socket(),
                              user :: netlib_pid(),
                              receiver :: netlib_pid()}).


%% netlib_gen_client config
-define(NETLIB_CLIENT_TCP_SEND_TIMEOUT, 15000). %% 15 seconds
-define(NETLIB_CLIENT_TCP_RECV_TIMEOUT, 15000). %% 15 seconds
-define(NETLIB_CLIENT_TCP_OPTS, 
        [binary,
         {packet, 0},
         {active, false},
         {reuseaddr, true},
         {nodelay, true},
         {send_timeout, ?NETLIB_CLIENT_TCP_SEND_TIMEOUT},
         {keepalive, true}]).

-record(netlib_client_conn, {id :: netlib_binary(),
                             pid :: netlib_pid()}).

%% benchmark apis:
-define(NETLIB_BENCHMARK_RUN(Num, ProcNum, Fun),
        netlib_benchmark:run(Num, ProcNum, Fun)).
-define(NETLIB_BENCHMARK_RUN_DETAIL(Num, ProcNum, Fun),
        netlib_benchmark:run_detail(Num, ProcNum, Fun)).

%% dynamic compile apis:
-define(NETLIB_DYNAMIC_NEW(Module),
        netlib_dynamic:new(Module)).
-define(NETLIB_DYNAMIC_ADD_FUN(Mod, Fun, BExport),
        netlib_dynamic:add_fun(Mod, Fun, BExport)).
-define(NETLIB_DYNAMIC_COMPILE(Mod),
        netlib_dynamic:compile(Mod)).

%% varint encode & decode apis:
-define(NETLIB_VARINT_ENCODE(Val), 
        netlib_integer:encode_varint(Val)).
-define(NETLIB_VARINT_DECODE(Val), 
        netlib_integer:decode_varint(Val)).

%% md5 apis:
-define(NETLIB_MD5(Data), 
        netlib_util:md5(Data)).    %% 16 bytes binary md5

%% uuid apis:
-define(NETLIB_UUID, 
        netlib_uuid_server:get()).  %% 32 bytes binary uuid

%% hook apis:
-define(NETLIB_HOOK_ADD3(Hook, Function, Seq),
        netlib_hook_server:add(Hook, Function, Seq)).
-define(NETLIB_HOOK_ADD4(Hook, Module, Function, Seq),
        netlib_hook_server:add(Hook, Module, Function, Seq)).
-define(NETLIB_HOOK_DELETE3(Hook, Function, Seq),
        netlib_hook_server:delete(Hook, Function, Seq)).
-define(NETLIB_HOOK_DELETE4(Hook, Module, Function, Seq),
        netlib_hook_server:delete(Hook, Module, Function, Seq)).
-define(NETLIB_HOOK_RUN(Hook, Args),
        netlib_hook_server:run(Hook, Args)).
-define(NETLIB_HOOK_RUN_FOLD(Hook, Val, Args),
        netlib_hook_server:run_fold(Hook, Val, Args)).

%% netlib_gen_request_server apis:
-define(NETLIB_GEN_REQUEST_SERVER_START(Address, Module, PoolSize, Opts),
        netlib_gen_request_server:start_server(Address, Module, PoolSize, Opts)).
-define(NETLIB_GEN_REQUEST_SERVER_STOP(Address),
        netlib_gen_request_server:stop_server(Address)).
-define(NETLIB_GEN_REQUEST_SERVER_STOP_ALL,
        netlib_gen_request_server:stop_servers()).
-define(NETLIB_GEN_REQUEST_SERVER_GET_ALL,
        netlib_gen_request_server:get_servers()).
-define(NETLIB_GEN_REQUEST_SERVER_SOCKNAME(Socket),
        netlib_gen_request_server:sockname(Socket)).
-define(NETLIB_GEN_REQUEST_SERVER_PEERNAME(Socket),
        netlib_gen_request_server:peername(Socket)).
-define(NETLIB_GEN_REQUEST_SERVER_SEND(Socket, Iolist),
        netlib_gen_request_server:send(Socket, Iolist)).
-define(NETLIB_GEN_REQUEST_SERVER_RECV(Socket, Length),
        netlib_gen_request_server:recv(Socket, Length)).


%% netlib_gen_request_server callbacks:
-define(NETLIB_GEN_REQUEST_SERVER_CALLBACK_DO_RECV_AND_SEND(
          Socket, Opts),
        do_recv_and_send(Socket, Opts)).

%% netlig_gen_server apis:
-define(NETLIB_GEN_SERVER_START(Address, Module, Opts),
        netlib_gen_server:start_server(Address, Module, Opts)).
-define(NETLIB_GEN_SERVER_STOP(Address),
        netlib_gen_server:stop_server(Address)).
-define(NETLIB_GEN_SERVER_STOP_ALL,
        netlib_gen_server:stop_servers()).
-define(NETLIB_GEN_SERVER_GET_ALL,
        netlib_gen_server:get_servers()).
-define(NETLIB_GEN_SERVER_SOCKET_CLOSE(Socket),
        netlib_gen_server:close(Socket)).
-define(NETLIB_GEN_SERVER_SOCKET_SOCKNAME(Socket),
        netlib_gen_server:sockname(Socket)).
-define(NETLIB_GEN_SERVER_SOCKET_PEERNAME(Socket),
        netlib_gen_server:peername(Socket)).
-define(NETLIB_GEN_SERVER_SOCKET_SEND(Socket, Data),
        netlib_gen_server:send(Socket, Data)).
-define(NETLIB_GEN_SERVER_SOCKET_USER_SEND(Pid, Data),
        netlib_server_user:send(Pid, Data)).

%% netlib_gen_server callbacks:
-define(NETLIB_GEN_SERVER_CALLBACK_TCP_START(SSocketState, Address, Opts),
        tcp_start(SSocketState, Address, Opts)).
-define(NETLIB_GEN_SERVER_CALLBACK_TCP_DATA(TcpData, State),
        tcp_data(TcpData, State)).
-define(NETLIB_GEN_SERVER_CALLBACK_TCP_CLOSE(State),
        tcp_close(State)).

%% netlib_gen_client apis:
-define(NETLIB_GEN_CLIENT_START(
         Address, PoolSize, StartInterval, PingInterval, Module, Opts),
       netlib_gen_client:start(
         Address, PoolSize, StartInterval, PingInterval, Module, Opts)).
-define(NETLIB_GEN_CLIENT_CALL(Address, Term),
        netlib_gen_client:call(Address, Term)).
-define(NETLIB_GEN_CLIENT_DO_SEND(Socket, Data),
        netlib_gen_client:do_send(Socket, Data)).
-define(NETLIB_GEN_CLIENT_DO_RECV(Socket, Length),
        netlib_gen_client:do_recv(Socket, Length)).

%% netlib_gen_client callbacks:
-define(NETLIB_GEN_CLIENT_CALLBACK_TCP_START(A, B, Address, Opts),
        tcp_start(A, B, Address, Opts)).
-define(NETLIB_GEN_CLIENT_CALLBACK_DO_SEND_RECV(Iolist, State),
        do_send_recv(Iolist, State)).
-define(NETLIB_GEN_CLIENT_CALLBACK_DO_PING(State),
        do_ping(State)).
-define(NETLIB_GEN_CLIENT_CALLBACK_TCP_CLOSE(State),
        tcp_close(State)).

%% event_handler apis:
-define(NETLIB_EVENT_HANDLER_ADD(
          Type, Component, Event, Module, Function, Mode),
        netlib_event_handler:add(
          Type, Component, Event, Module, Function, Mode)).
-define(NETLIB_EVENT_HANDLER_REMOVE(
          Type, Component, Event),
        netlib_event_handler:remove(Type, Component, Event)).
-define(NETLIB_EVENT_HANDLER_ASYNC_CALL(
          Component, Event, Data),
        netlib_event_handler:async_call(Component, Event, Data)).
-define(NETLIB_EVENT_HANDLER_SYNC_CALL(
          Component, Event, Data),
        netlib_event_handler:sync_call(Component, Event, Data)).


