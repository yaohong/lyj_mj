-module(world_app).
-author('yh@gmail.com').
-behaviour(application).
-include("../deps/file_log/include/file_log.hrl").
-include("world.hrl").

-export([start/2, 
         stop/1,
         prep_stop/1]).

-spec start(Type :: term(), StartArgs :: term()) -> 
    {ok, pid()} | {error, 'badarg'}.
start(normal, _StartArgs) ->
	world_ctl:write_pid_file(),
	ok = world_util:ensure_app_started(log4erl),
    ok = world_util:ensure_app_started(sasl),
    ok = world_util:ensure_app_started(crypto),
    ok = world_util:ensure_app_started(yhsql),

    ok = world_util:ensure_app_started(asn1),
    ok = world_util:ensure_app_started(public_key),
    ok = world_util:ensure_app_started(ssl),
    ok = world_util:ensure_app_started(inets),
	{ok, _} = world_config:start_link(),
    {ok, Pid} = world_sup:start_link(),
%% 	MailServer = os:getenv("MAIL_SERVER"),
%% 	{ok, _} = log4erl:add_logger("sl_email"),
%% 	{ok, _} = log4erl:add_smtp_appender(
%% 		sl_email, sl_email_app,
%% 		{debug, "smtp.qq.com", {"231344781", "yaohong231344781"}, {"231344781@qq.com", MailServer, "log", "[%T %j] %L:%n%l%n"}}),
    log_running_applications(),
    ?FILE_LOG_INFO("world ~s is started in the node ~p", [?WORLD_VERSION, node()]),
    {ok, Pid};
start(_Type, _StartArgs) ->
    {error, 'badarg'}.

prep_stop(State) ->
    State.

stop(_State) ->
	world_ctl:delete_pid_file(),
    ?FILE_LOG_INFO("world ~s is stopped in the node ~p", [?WORLD_VERSION, node()]),
    ok.
    
-spec log_running_applications() ->
    ok.
log_running_applications() ->
    lists:foreach(
        fun(App) ->
            ?FILE_LOG_INFO("running application#~p", [App])
        end, application:which_applications()).

