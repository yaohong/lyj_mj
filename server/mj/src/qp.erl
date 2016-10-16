-module(qp).
-author('yh@gmail.com').

-include("../deps/file_log/include/file_log.hrl").

-export([start/0, stop/0]).

-spec start() -> ok | {error, term()}.
start() ->
    application:start(qp).

-spec stop() -> ok | {error, term()}.
stop() ->
    application:stop(qp).
