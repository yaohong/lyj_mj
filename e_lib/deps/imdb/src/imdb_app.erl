-module(imdb_app).
-author('erlangonrails@gmail.com').
-behaviour(application).

-export([start/2, 
         stop/1,
         prep_stop/1]).

-spec start(Type :: term(), StartArgs :: term()) -> 
    {ok, pid()} | {error, 'badarg'}.
start(normal, _StartArgs) ->
    ok = imdb_util:ensure_app_started(sasl),
    ok = imdb_util:ensure_app_started(crypto),
    ok = imdb_util:ensure_app_started(file_log),
    ok = netlib_client_ets:start(),
    {ok, _Pid} = imdb_netlib_gen_client:start(),
    {ok, Pid} = imdb_sup:start_link(),
    {ok, Pid};
start(_Type, _StartArgs) ->
    {error, 'badarg'}.

prep_stop(State) ->
    State.

stop(_State) ->
    ok.
