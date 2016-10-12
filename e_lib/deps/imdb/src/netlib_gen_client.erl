-module(netlib_gen_client).
-author('erlangonrails@gmail.com').
-include("netlib.hrl").
-export([behaviour_info/1]).
-export([start/6,
         call/2,
         do_send/2,
         do_recv/2]).

behaviour_info(callbacks) ->
    [{tcp_start, 4}, 
     {do_send_recv, 2},
     {do_ping, 1},
     {tcp_close, 1}];
behaviour_info(_Other) ->
    undefined.


-spec start(Address :: netlib_address(),
            PoolSize :: netlib_timeout(),
            StartInterval :: netlib_non_neg_integer(),   %% second
            PingInterval :: netlib_timeout(),    %% sencod
            Module :: netlib_module(),                   
            Opts :: netlib_proplist()) -> 
    netlib_ret_ok(netlib_pid()).
start(Address, PoolSize, StartInterval, PingInterval, Module, Opts) ->
    netlib_client_conn_sup:start_link(
        Address, PoolSize, StartInterval, PingInterval, Module, Opts).


-spec call(Address :: netlib_address() | 
                      netlib_binary(),
           Term :: netlib_term()) ->
    undefined | netlib_term().
call(Address, Term) ->
    case netlib_client_conn_sup:get_random_pid(Address) of
        undefined ->
            undefined;
        Pid ->
            %% 取消超时的限制 
            case catch gen_fsm:sync_send_event(Pid, {cmd, {do_send_recv, Term}}, infinity) of
                {ok, Data} ->
                    Data;
                _ ->
                    undefined
            end
    end.

-spec do_send(Socket :: netlib_socket(),
              Data :: netlib_iolist()) ->
    ok.
do_send(Socket, Data) ->
    netlib_client_conn:do_send(Socket, Data).

-spec do_recv(Socket :: netlib_socket(),
              Length :: netlib_integer()) ->
    netlib_binary() | netlib_list().
do_recv(Socket, Length) ->
    netlib_client_conn:do_recv(Socket, Length).


