-module(relax_app).
-author('erlangonrails@gmail.com').
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ok = relax_util:ensure_app_started(crypto),
    ok = relax_util:ensure_app_started(sasl),
    ok = relax_util:ensure_app_started(file_log),
    ok = relax_util:ensure_app_started(json),
    relax_sup:start_link().

stop(_State) ->
    ok.
