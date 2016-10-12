-module(server_frame_app).
-author('erlangonrails@gmail.com').
-behaviour(application).

-export([start/2, 
         stop/1,
         prep_stop/1]).
-include("file_log.hrl").
-spec start(Type :: term(), StartArgs :: term()) -> 
    {ok, pid()} | {error, 'badarg'}.
start(normal, [ServerName, UserHandleModule, PacketHeadLen, UserSup, ReceiverSup, Ip, Port]) ->
    ok = server_frame_util:ensure_app_started(file_log),
    {ok, Pid} = server_frame_sup:start_link({ServerName, UserHandleModule, PacketHeadLen, UserSup, ReceiverSup, Ip, Port}),
    {ok, Pid};
start(_Type, _StartArgs) ->
    {error, 'badarg'}.

prep_stop(State) ->
    State.

stop(_State) ->
    ok.
