-module(relax_demo).
-author('erlangonrails@gmail.com').
-include("file_log.hrl").
-export([start/0,            %% 有监控树
         start_link/0,       %% 没有监控树
         stop/0, 
         handle_request/2]).

-define(RELAX_DEMO_PORT, 6789).
-define(RELAX_DEMO_DOCROOT, ".").

start() ->
    ok = relax_util:ensure_app_started(relaxweb),
    relax_http:start(options()).

start_link() ->
    ok = relax_util:ensure_app_started(relaxweb),
    relax_http:start_link(options()).

stop() ->
    relax_http:stop(?MODULE).

options() ->
    [{name, ?MODULE},
     {port, ?RELAX_DEMO_PORT},
     {ip, any},
     {loop, {?MODULE, handle_request, [?RELAX_DEMO_DOCROOT]}},
     {profile_fun, fun(A) -> ?FILE_LOG_INFO("profile: ~p", [A]) end}].


handle_request(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    Method = Req:get(method),
    {Main, Minor} = Req:get(version),
    ?FILE_LOG_INFO("~s ~s HTTP ~B.~B", [Method, Path, Main, Minor]),
    case Method of
        'GET' ->
            route_req_get(Req, Path, DocRoot);
        'POST' ->
            route_req_post(Req, Path, DocRoot);
        _ ->
            Req:respond({500,[],"relax_demo error"})
    end.


%% Internal API
route_req_get(Req, Path, _DocRoot) ->
    Req:respond({200, [{"Content-Type","text/plain"}], "route_req_get:" ++ Path}).
route_req_post(Req, Path, _DocRoot) ->
    Req:respond({200, [{"Content-Type","text/plain"}], "route_req_post:" ++ Path}).
