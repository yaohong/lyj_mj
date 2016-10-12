-module(friend_app).
-author('erlangonrails@gmail.com').
-behaviour(application).

-export([start/2, 
         stop/1,
         prep_stop/1]).

-spec start(Type :: term(), StartArgs :: term()) -> 
    {ok, pid()} | {error, 'badarg'}.
start(normal, _StartArgs) ->
    ok = friend_util:ensure_app_started(file_log),
    {ok, Pid} = friend_sup:start_link(),
    {ok, Pid};
start(_Type, _StartArgs) ->
    {error, 'badarg'}.

prep_stop(State) ->
    State.

stop(_State) ->
    ok.
