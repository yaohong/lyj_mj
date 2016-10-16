-module(uuid_app).
-author('erlangonrails@gmail.com').
-behaviour(application).

-export([start/2, 
         stop/1,
         prep_stop/1]).

-spec start(Type :: term(), StartArgs :: term()) -> 
    {ok, pid()} | {error, 'badarg'}.
start(normal, _StartArgs) ->
    ok = uuid_util:ensure_app_started(crypto),
    {ok, Pid} = uuid_sup:start_link(),
    {ok, Pid};
start(_Type, _StartArgs) ->
    {error, 'badarg'}.

prep_stop(State) ->
    State.

stop(_State) ->
    ok.
