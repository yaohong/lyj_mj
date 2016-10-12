-module(uuid_util).
-author('erlangonrails@gmail.com').
-export([ensure_app_started/1]).

-spec ensure_app_started(App :: atom()) -> ok.
ensure_app_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
