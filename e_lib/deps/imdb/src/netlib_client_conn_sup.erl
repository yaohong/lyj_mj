-module(netlib_client_conn_sup).
-author('erlangonrails@gmail.com').
-include("netlib.hrl").
-behaviour(supervisor).

%% API
-export([start_link/6,
         get_pids/1,
         get_random_pid/1,
         add_pid/1, add_pid/2,
         delete_pid/1, delete_pid/2]).

%% Supervisor callbacks
-export([init/1]).

-spec start_link(Address :: netlib_address(),
                 PoolSize :: netlib_timeout(),
                 StartInterval :: netlib_non_neg_integer(),   %% second
                 PingInterval :: netlib_non_neg_integer(),    %% sencod
                 Module :: netlib_module(),                   
                 Opts :: netlib_proplist()) -> 
    netlib_ret_ok(netlib_pid()).
start_link(Address, PoolSize, StartInterval, PingInterval, Module, Opts) ->
    Name = netlib_util:address_to_atom(Address),
    supervisor:start_link(
        {local, Name}, 
        ?MODULE, 
        [Address, PoolSize, StartInterval, PingInterval, Module, Opts]).


init([Address, PoolSize, StartInterval, PingInterval, Module, Opts]) ->
    AllChildSpec = 
        make_child_spec(Address, PoolSize, StartInterval, PingInterval, Module, Opts),
    {ok,{{one_for_one, 999999, 1}, AllChildSpec}}.


-spec get_pids(Address :: netlib_binary() |
netlib_address()) ->
    [pid()].
get_pids(Address) when is_tuple(Address) ->
    get_pids(netlib_util:address_to_binary(Address));
get_pids(Address) when is_binary(Address) ->
    Conns = ets:lookup(netlib_client_conn_pool, Address),
    [R#netlib_client_conn.pid || R <- Conns].


-spec get_random_pid(Address :: netlib_binary() |
                                netlib_address()) ->
    pid() | undefined.
get_random_pid(Address) ->
    case get_pids(Address) of
        [] ->
            undefined;
        Pids ->
            lists:nth(erlang:phash(now(), length(Pids)), Pids)
    end.


-spec add_pid(Conn :: #netlib_client_conn{}) ->
    true.
add_pid(Conn) when is_record(Conn, netlib_client_conn) ->
    ets:insert(netlib_client_conn_pool, Conn).


-spec add_pid(Address :: netlib_binary() |
                         netlib_address(),
              Pid :: pid()) ->
    true.
add_pid(Address, Pid) when is_tuple(Address) ->
    add_pid(netlib_util:address_to_binary(Address), Pid);
add_pid(Address, Pid) when is_binary(Address) ->
    Conn = #netlib_client_conn{id = Address,
                               pid = Pid},
    ets:insert(netlib_client_conn_pool, Conn).


-spec delete_pid(Conn :: #netlib_client_conn{}) ->
    true.
delete_pid(Conn) when is_record(Conn, netlib_client_conn) ->
    ets:delete_object(netlib_client_conn_pool, Conn).


-spec delete_pid(Address :: netlib_binary() |
                            netlib_address(),
                 Pid :: pid()) ->
    true.
delete_pid(Address, Pid) when is_tuple(Address) ->
    delete_pid(netlib_util:address_to_binary(Address), Pid);
delete_pid(Address, Pid) when is_binary(Address) ->
    Conn = #netlib_client_conn{id = Address,
                               pid = Pid},
    ets:delete_object(netlib_client_conn_pool, Conn).


%% Internal APIs:
-spec make_child_spec(Address :: netlib_address(),
                      PoolSize :: netlib_non_neg_integer(),
                      StartInterval :: netlib_non_neg_integer(),
                      PingInterval :: netlib_non_neg_integer(),
                      Module :: netlib_module(),
                      Opts :: netlib_proplist()) ->
    list().
make_child_spec(Address, PoolSize, StartInterval, PingInterval, Module, Opts) ->
    lists:map(
        fun(I) ->
            {I, 
             {netlib_client_conn, start_link, 
              [Address, StartInterval, PingInterval, Module, Opts]},
             transient,
             brutal_kill,
             worker,
            [?MODULE]}
        end, lists:seq(1, PoolSize)).
