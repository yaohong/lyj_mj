-module(netlib_client_ets).
-author('erlangonrails@gmail.com').
-export([start/0]).

-spec start() ->
    ok.
start() ->
    netlib_client_conn_pool = 
    ets:new(netlib_client_conn_pool, 
            [bag, 
             public,
             named_table,
             {keypos, 2}]),
    ok.
