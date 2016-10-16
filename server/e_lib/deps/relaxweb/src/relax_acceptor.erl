-module(relax_acceptor).
-author('erlangonrails@gmail.com').
-include("file_log.hrl").
-export([start_link/3, init/3]).

%% Server: 是relax_socket_server进程的pid.
-spec start_link(
        Server :: pid(), 
        Listen :: relax_socket:socket(), 
        Loop :: fun() | {M :: atom(), F :: atom()} | {M :: atom(), F :: atom(), A :: list()}) ->
    pid().
start_link(Server, Listen, Loop) ->
    proc_lib:spawn_link(?MODULE, init, [Server, Listen, Loop]).

init(Server, Listen, Loop) ->
    T1 = erlang:timestamp(),
    case catch relax_socket:accept(Listen) of
        {ok, Socket} ->
            gen_server:cast(Server, {accepted, self(), timer:now_diff(erlang:timestamp(), T1)}),
            call_loop(Loop, Socket);
        {error, closed} ->
            exit(normal);
        {error, timeout} ->
            init(Server, Listen, Loop);
        {error, esslaccept} ->
            exit(normal);
        Other ->
            ?FILE_LOG_INFO("accept failed due to:~p", [Other]),
            exit({error, accept_failed})
    end.

call_loop({M, F}, Socket) ->
    M:F(Socket);
call_loop({M, F, A}, Socket) ->
    erlang:apply(M, F, [Socket | A]);
call_loop(Loop, Socket) ->
    Loop(Socket).
