-module(relax_echo).
-author('erlangonrails@gmail.com').
-include("file_log.hrl").
-export([start/0,       %% 有监控树
         start_link/0,  %% 没有监控树
         stop/0, 
         loop/1]).

-define(RELAX_ECHO_PORT, 6789).

%% 一个演示使用relax_socket_server的echo demo.

start() ->
    ok = relax_util:ensure_app_started(relaxweb),
    relax_socket_server:start(options()).

start_link() ->
    ok = relax_util:ensure_app_started(relaxweb),
    relax_socket_server:start_link(options()).

stop() ->
    relax_socket_server:stop(?MODULE).

options() ->
    [{name, ?MODULE},
     {port, ?RELAX_ECHO_PORT},
     {ip, any},
     {loop, {?MODULE, loop}},
     {profile_fun, fun(A) -> ?FILE_LOG_INFO("profile: ~p", [A]) end}].

loop(Socket) ->
    case relax_socket:recv(Socket, 0, 30000) of
        {ok, Data} ->
            case relax_socket:send(Socket, Data) of
                ok ->
                    loop(Socket);
                _ ->
                    ?FILE_LOG_INFO("socket_exit:~p", [Socket]),
                    exit(normal)
            end;
        _Other ->
            ?FILE_LOG_INFO("socket_exit:~p", [Socket]),
            exit(normal)
    end.
