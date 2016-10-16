-module(tcp_socket).
-author('erlangonrails@gmail.com').
-include("file_log.hrl").
-export([start/5, 
         close/1, 
         send/2,
         monitor/1, 
         sockname/1, 
         peername/1]).

%% 该模块将halld_receiver & halld_user | halld_room相关联.

-record(socket_state, {
            socket :: gen_tcp:socket(), %% 客户端socket
            receiver :: pid()
        }).        %% halld_receiver进程的pid

-type socket() :: #socket_state{}.

%% 将halld_receiver & halld_user | halld_room相关联的逻辑:
%% 1. 启动halld_receiver
%% 2. 启动halld_user | halld_room
%% 3. 将halld_receiver进程设置为socket对应的控制进程用来接收tcp数据
%% 4. 调用halld_receiver:become_controller/2来active socket, 触发真正
%%    接收数据的逻辑.
%%
%% halld_receiver不能启动如何处理?
%% start/1函数因为模式不匹配直接抛出异常后终止.
%%
%% halld_user | halld_room不能启动如何处理(此时halld_receiver已经成功启动)?
%% 调用halld_socket:close/1来释放资源, 会关闭socket, 并退出halld_receiver
%% 进程.
-spec start(
        Socket :: gen_tcp:socket(), 
        UserHandle :: module(),
		PacketHeadLen :: integer(),
        UserSup :: module(),
        ReceiveSup :: module()) -> ok.
start(Socket, UserHandle, PacketHeadLen, UserSup, ReceiveSup) ->
    {ok, ReceiverPid} = tcp_receiver:start(UserHandle, ReceiveSup, PacketHeadLen, Socket),
    SocketData = #socket_state{socket = Socket, receiver = ReceiverPid},
    case UserHandle:start(UserSup, ?MODULE, SocketData) of
        {ok, UserPid} ->
            case gen_tcp:controlling_process(Socket, ReceiverPid) of
                ok ->
                    ok;
                {error, Reason} ->
                    ?FILE_LOG_ERROR("controller_process error:~p", [Reason]),
                    close(SocketData)
            end,
            %% 触发开始真正接收数据的逻辑
            tcp_receiver:become_controller(ReceiverPid, UserPid);
        {error, Reason} ->
            ?FILE_LOG_ERROR("~p start error:~p", [UserHandle, Reason]),
            gen_tcp:close(Socket)
    end.

-spec monitor(SocketData :: tcp_socket:socket()) -> reference().
monitor(SocketData) when is_record(SocketData, socket_state) andalso is_pid(SocketData#socket_state.receiver) ->
    erlang:monitor(process, SocketData#socket_state.receiver).

-spec close(SocketData :: tcp_socket:socket()) ->
    ok.
close(SocketData) when is_record(SocketData, socket_state) ->
    tcp_receiver:close(SocketData#socket_state.receiver).

-spec sockname(SocketData :: tcp_socket:socket()) -> 
    {ok, {Address :: inet:ip_address(), Port :: integer()}} | 
    {error, _}.
sockname(SocketData) when is_record(SocketData, socket_state)  ->
    inet:sockname(SocketData#socket_state.socket).

-spec peername(SocketData :: tcp_socket:socket()) ->
    {ok, {Address :: inet:ip_address(), Port :: integer()}} | 
    {error, _}.
peername(SocketData) when is_record(SocketData, socket_state) ->
    inet:peername(SocketData#socket_state.socket).

-spec send(SocketData :: tcp_socket:socket(), Data :: iodata()) -> 
    ok.
send(SocketData, Data) when is_record(SocketData, socket_state) ->
    case catch gen_tcp:send(SocketData#socket_state.socket, Data) of
        ok -> 
            ok;
        {error, timeout} ->
            ?FILE_LOG_ERROR("socket send timeout:~p",[SocketData#socket_state.socket]),
            exit(normal);
        Reason ->
            ?FILE_LOG_ERROR("socket send error:~p ~p",[Reason, Data]),
            exit(normal)
    end.
