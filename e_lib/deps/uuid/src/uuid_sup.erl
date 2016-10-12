-module(uuid_sup).
-author('erlangonrails@gmail.com').
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    UUIDSup = 
      {uuid_sup, {uuid_server, start_link, []},
       permanent,
       brutal_kill,
       supervisor,
       [uuid_server]},
    {ok, {{one_for_one, 100, 5}, [UUIDSup]}}.
