-module(hash_service_app).
-author('yh@gmail.com').
-behaviour(application).

-export([start/2, 
         stop/1,
         prep_stop/1]).
-spec start(Type :: term(), StartArgs :: term()) -> 
    {ok, pid()} | {error, 'badarg'}.
start(normal, []) ->
    ok = hash_service_util:ensure_app_started(file_log),
    {ok, Pid} = hash_service_sup:start_link(),
    {ok, Pid};
start(_Type, _StartArgs) ->
    {error, 'badarg'}.

prep_stop(State) ->
    State.

stop(_State) ->
    ok.
