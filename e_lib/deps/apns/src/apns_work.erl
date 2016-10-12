%% Copyright
-module(apns_work).
-author("yaohong").
-include("file_log.hrl").
-include("apns.hrl").
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).
-export([]).
-record(state, {push_service_node_name}).
%% API
start_link(AppPushServiceNodeName) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [AppPushServiceNodeName], []).


init([AppPushServiceNodeName]) ->
	{ok, #state{push_service_node_name=AppPushServiceNodeName}}.

handle_call(_Request, _From, State) ->
	{noreply, State}.

handle_cast({apns_push, {GameName, PushMode, Device, Msg}}, State) ->
	rpc:cast(State#state.push_service_node_name, apns, apns_push, [GameName, PushMode, Device, Msg]),
	{noreply, State};
handle_cast(_Request, State) ->
	{noreply, State}.
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.