-module(tcp_server).
-author('erlangonrails@gmail.com').
-include("../deps/file_log/include/file_log.hrl").
-export([start_link/7,start_link/8,
         init/8]).

-define(TCP_SEND_TIMEOUT, 15000).



-spec start_link(
        Ip :: inet:ip_address(),            %%监听的IP
        Port :: integer(),                  %%监听的端口
        ServerName :: atom(),
        UserHandle :: module(),               %%处理命令的USER模块
		PacketHeadLen :: integer(),           %%包头的长度
        UserSup :: module(),                  %%监控USER的监控树
        ReceiveSup :: module()                %%监控接收进程的监控树
      ) -> 
    {ok, pid()}.
start_link(Ip, Port, ServerName, UserHandle, PacketHeadLen, UserSup, ReceiveSup) ->
	start_link(Ip, Port, ServerName, UserHandle, PacketHeadLen, UserSup, ReceiveSup, 1024).
start_link(Ip, Port, ServerName, UserHandle, PacketHeadLen, UserSup, ReceiveSup, MaxStanzaSize) ->
	proc_lib:start_link(?MODULE, init, [Ip, Port, ServerName, UserHandle, PacketHeadLen, UserSup, ReceiveSup, MaxStanzaSize]).


init(Ip, Port, ServerName, UserHandle, PacketHeadLen, UserSup, ReceiveSup, MaxStanzaSize) ->
    true = register(ServerName, self()),
    process_flag(trap_exit, true),
    SockOpts = try erlang:system_info(otp_release) >= "R13B" of
                   true -> [{send_timeout_close, true}];
                   false -> []
               catch
                   _:_ -> []
               end,
    ListenRet = gen_tcp:listen(Port, [binary,
                                      {ip, Ip},
                                      {packet, 0},
                                      {active, false},
                                      {reuseaddr, true},
                                      {nodelay, true},
                                      {send_timeout, ?TCP_SEND_TIMEOUT},
                                      {keepalive, true} |
                                      SockOpts]),
    case ListenRet of
        {ok, ListenSocket} ->
            ?FILE_LOG_INFO("listen on ~s:~p", [inet_parse:ntoa(Ip), Port]),
            proc_lib:init_ack({ok, self()}),
            accept(ListenSocket, UserHandle, PacketHeadLen, UserSup, ReceiveSup, MaxStanzaSize);
        {error, Reason} ->
            ?FILE_LOG_WARNING("socket listen error:~p ~p", [Port,Reason])
    end.


accept(ListenSocket, UserHandle, PacketHeadLen, UserSup, ReceiveSup, MaxStanzaSize) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            case catch {inet:sockname(Socket), inet:peername(Socket)} of
                {{ok, _Addr}, {ok, _PAddr}} -> ok;
                    %%?FILE_LOG_DEBUG("accepted connection: ~p -> ~p", [Addr, PAddr]);
                _ ->
                    ok
            end,
            catch tcp_socket:start(Socket, UserHandle, PacketHeadLen, UserSup, ReceiveSup, MaxStanzaSize),
            accept(ListenSocket, UserHandle, PacketHeadLen, UserSup, ReceiveSup, MaxStanzaSize);
        {error, Reason} ->
            ?FILE_LOG_WARNING("socket accept error:~p", [Reason]),
            accept(ListenSocket, UserHandle, PacketHeadLen, UserSup, ReceiveSup, MaxStanzaSize)
    end.
