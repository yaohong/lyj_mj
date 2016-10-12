-module(friend).
-author('yh@gmail.com').
-export([start/0, stop/0]).

-spec start() -> ok | {error, term()}.
start() ->
	application:start(friend).

-spec stop() -> ok | {error, term()}.
stop() ->
	application:stop(friend).