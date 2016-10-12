-module(mysql_sup).
-author('erlangonrails@gmail.com').
-behaviour(supervisor).
-export([start_link/0,
         init/1]).

%% 监控树模块

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 10, 1}, []}}.

