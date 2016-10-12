-module(protobuffs_sup).
-author('erlangonrails@gmail.com').
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).


-spec start_link() ->
    {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 100, 5}, []}}.
