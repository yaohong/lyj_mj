-module(yhsql_app).
-author('erlangonrails@gmail.com').
-behaviour(application).
-export([start/2,
         stop/1]).

%% Application模块

start(_StartType, _StartArgs) ->
    ok = yhsql_util:ensure_app_started(crypto),
    {ok, Pid} = yhsql_sup:start_link(),
    {ok, Pid}.

stop(_State) ->
    ok.

