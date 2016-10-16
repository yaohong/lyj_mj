-module(qp_app).
-author('yh@gmail.com').
-behaviour(application).
-include("../deps/file_log/include/file_log.hrl").
-include("qp.hrl").

-export([start/2, 
         stop/1,
         prep_stop/1]).

-spec start(Type :: term(), StartArgs :: term()) -> 
    {ok, pid()} | {error, 'badarg'}.
start(normal, _StartArgs) ->
	qp_ctl:write_pid_file(),
	ok = qp_util:ensure_app_started(log4erl),
    ok = qp_util:ensure_app_started(sasl),
    ok = qp_util:ensure_app_started(crypto),
    ok = qp_util:ensure_app_started(yhsql),

    ok = qp_util:ensure_app_started(asn1),
    ok = qp_util:ensure_app_started(public_key),
    ok = qp_util:ensure_app_started(ssl),
    ok = qp_util:ensure_app_started(inets),
	{ok, _} = qp_config:start_link(),
    {ok, Pid} = qp_sup:start_link(),
    log_running_applications(),
    ?FILE_LOG_INFO("qp ~s is started in the node ~p", [?QP_VERSION, node()]),
    {ok, Pid};
start(_Type, _StartArgs) ->
    {error, 'badarg'}.

prep_stop(State) ->
    State.

stop(_State) ->
    qp_ctl:delete_pid_file(),
    ?FILE_LOG_INFO("qp ~s is stopped in the node ~p", [?QP_VERSION, node()]),
    ok.
    
-spec log_running_applications() ->
    ok.
log_running_applications() ->
    lists:foreach(
        fun(App) ->
            ?FILE_LOG_INFO("running application#~p", [App])
        end, application:which_applications()).

