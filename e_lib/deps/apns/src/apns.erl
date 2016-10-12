-module(apns).
-author('yh@gmail.com').
-export([start/0, stop/0]).

-spec start() -> ok | {error, term()}.
start() ->
	application:start(apns).

-spec stop() -> ok | {error, term()}.
stop() ->
	application:stop(apns).