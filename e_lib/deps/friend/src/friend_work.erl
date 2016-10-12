%% Copyright
-module(friend_work).
-author("yaohong").
-include("file_log.hrl").
-include("friend.hrl").
-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).
-export([friend_event/3]).
%% API
start_link(ProcessName, ServiceNode) ->
	gen_server:start_link({local, ProcessName}, ?MODULE, [ServiceNode], []).

friend_event(Account, Session, Value) ->
	ProcessAtom = friend_sup:get_process(Account),
	ProcessAtom ! {friend_event, {Session, Value}}.
%% gen_server callbacks
-record(state, {login_tb, friend_service_node}).
-record(login_tb, {session, account, remote_pid, from_pid}).

init([ServiceNode]) ->
	LoginTb = ets:new(?MODULE, [set, protected, {keypos, #login_tb.session}]),
	{ok,
		#state{
			login_tb = LoginTb,
			friend_service_node = ServiceNode
		}}.

handle_call({login, {FromAccount, FromPid, GameId}}=Event, _From, #state{login_tb=LoginTb, friend_service_node=ServiceNode}=State) ->
	?FILE_LOG_DEBUG("~p", [Event]),
	Reply =
	case catch rpc:call(ServiceNode, friend_service_monitor, fd_event_handle, [login, {FromAccount, GameId, node()}]) of
		{badrpc, Reason} ->
			?FILE_LOG_ERROR("rpc login badrpc reason=~p", [Reason]),
			{fail, ?FD_OTHER_ERROR};
		{'EXIT', Reason1} ->
			?FILE_LOG_ERROR("rpc login exit reason=~p", [Reason1]),
			{fail, ?FD_OTHER_ERROR};
		{success, {Session, RemotePid}} ->
			ets:insert(LoginTb, #login_tb{session=Session, account=FromAccount, remote_pid=RemotePid, from_pid=FromPid}),
			?FILE_LOG_DEBUG("login success ~p ~p ~p", [FromAccount, Session, RemotePid]),
			{success, Session};
		{fail, ErrorCode} ->
			{fail, ErrorCode};
		Other ->
			?FILE_LOG_ERROR("rpc login other=~p", [Other]),
			{fail, ?FD_OTHER_ERROR}
	end,
	{reply, Reply, State};
handle_call({add_friend, {Session, ToAccount, ReqMsg}}=Event, _From, #state{login_tb=LoginTb, friend_service_node=ServiceNode}=State) ->
	?FILE_LOG_DEBUG("~p", [Event]),
	Reply =
	case ets:lookup(LoginTb, Session) of
		[] -> {fail, ?FD_GAME_NOT_LOGIN};
		[LoginItem] ->
			case catch rpc:call(ServiceNode, friend_service_monitor, fd_event_handle, [add_friend, {LoginItem#login_tb.remote_pid, {ToAccount, ReqMsg}}]) of
				{badrpc, Reason} ->
					?FILE_LOG_ERROR("rpc add_friend badrpc reason=~p", [Reason]),
					{fail, ?FD_OTHER_ERROR};
				{'EXIT', Reason1} ->
					?FILE_LOG_ERROR("rpc add_friend exit reason=~p", [Reason1]),
					{fail, ?FD_OTHER_ERROR};
				Other -> Other
			end
	end,
	{reply, Reply, State};
handle_call({del_friend, {Session, FriendAccount}}=Event, _From, #state{login_tb=LoginTb, friend_service_node=ServiceNode}=State) ->
	?FILE_LOG_DEBUG("~p", [Event]),
	Reply =
	case ets:lookup(LoginTb, Session) of
		[] -> {fail, ?FD_GAME_NOT_LOGIN};
		[LoginItem] ->
			case catch rpc:call(ServiceNode, friend_service_monitor, fd_event_handle, [del_friend, {LoginItem#login_tb.remote_pid, FriendAccount}]) of
				{badrpc, Reason} ->
					?FILE_LOG_ERROR("rpc del_friend badrpc reason=~p", [Reason]),
					{fail, ?FD_OTHER_ERROR};
				{'EXIT', Reason1} ->
					?FILE_LOG_ERROR("rpc del_friend exit reason=~p", [Reason1]),
					{fail, ?FD_OTHER_ERROR};
				Other -> Other
			end
	end,
	{reply, Reply, State};
handle_call({friend_req_ack, {Session, ToAccount, AckState,AckMsg}}=Event, _From, #state{login_tb=LoginTb, friend_service_node=ServiceNode}=State) ->
	?FILE_LOG_DEBUG("~p", [Event]),
	Reply =
		case ets:lookup(LoginTb, Session) of
			[] -> {fail, ?FD_GAME_NOT_LOGIN};
			[LoginItem] ->
				case catch rpc:call(ServiceNode, friend_service_monitor, fd_event_handle, [friend_req_ack, {LoginItem#login_tb.remote_pid, {ToAccount, AckState,AckMsg}}]) of
					{badrpc, Reason} ->
						?FILE_LOG_ERROR("rpc friend_req_ack badrpc reason=~p", [Reason]),
						{fail, ?FD_OTHER_ERROR};
					{'EXIT', Reason1} ->
						?FILE_LOG_ERROR("rpc friend_req_ack exit reason=~p", [Reason1]),
						{fail, ?FD_OTHER_ERROR};
					Other -> Other
				end
		end,
	{reply, Reply, State};
handle_call({friend_msg, {Session, FriendAccount, {MsgData, MsgFormat, MsgId}}}=Event, _From, #state{login_tb=LoginTb, friend_service_node=ServiceNode}=State) ->
	?FILE_LOG_DEBUG("~p", [Event]),
	Reply =
		case ets:lookup(LoginTb, Session) of
			[] -> {fail, ?FD_GAME_NOT_LOGIN};
			[LoginItem] ->
				case catch rpc:call(
						ServiceNode,
						friend_service_monitor, fd_event_handle,
						[friend_msg, {LoginItem#login_tb.remote_pid, {FriendAccount, {MsgData, MsgFormat, MsgId}}}]) of
					{badrpc, Reason} ->
						?FILE_LOG_ERROR("rpc friend_msg badrpc reason=~p", [Reason]),
						{fail, ?FD_OTHER_ERROR};
					{'EXIT', Reason1} ->
						?FILE_LOG_ERROR("rpc friend_msg exit reason=~p", [Reason1]),
						{fail, ?FD_OTHER_ERROR};
					Other -> Other
				end
		end,
	{reply, Reply, State};
handle_call({friend_custom_data, {Session, FriendAccount, CustomData}}=Event, _From, #state{login_tb=LoginTb, friend_service_node=ServiceNode}=State) ->
	?FILE_LOG_DEBUG("~p", [Event]),
	Reply =
		case ets:lookup(LoginTb, Session) of
			[] -> {fail, ?FD_GAME_NOT_LOGIN};
			[LoginItem] ->
				case catch rpc:call(
						ServiceNode,
						friend_service_monitor, fd_event_handle,
						[friend_custom_data, {LoginItem#login_tb.remote_pid, {FriendAccount, CustomData}}]) of
					{badrpc, Reason} ->
						?FILE_LOG_ERROR("rpc friend_custom_data badrpc reason=~p", [Reason]),
						{fail, ?FD_OTHER_ERROR};
					{'EXIT', Reason1} ->
						?FILE_LOG_ERROR("rpc friend_custom_data exit reason=~p", [Reason1]),
						{fail, ?FD_OTHER_ERROR};
					Other -> Other
				end
		end,
	{reply, Reply, State};
handle_call({get_friend_list, {Session, Mode}}=Event, _From, #state{login_tb=LoginTb, friend_service_node=ServiceNode}=State) ->
	?FILE_LOG_DEBUG("~p", [Event]),
	Reply =
		case ets:lookup(LoginTb, Session) of
			[] -> {fail, ?FD_GAME_NOT_LOGIN};
			[LoginItem] ->
				case catch rpc:call(ServiceNode, friend_service_monitor, fd_event_handle, [get_friend_list, {LoginItem#login_tb.remote_pid, Mode}]) of
					{badrpc, Reason} ->
						?FILE_LOG_ERROR("rpc get_friend_list badrpc reason=~p", [Reason]),
						{fail, ?FD_OTHER_ERROR};
					{'EXIT', Reason1} ->
						?FILE_LOG_ERROR("rpc get_friend_list exit reason=~p", [Reason1]),
						{fail, ?FD_OTHER_ERROR};
					Other -> Other
				end
		end,
	{reply, Reply, State};
handle_call(_Request, _From, State) ->
	{noreply, State}.

handle_cast({logout, Session}=Event, #state{login_tb=LoginTb, friend_service_node=ServiceNode}=State) ->
	?FILE_LOG_DEBUG("~p", [Event]),
	case ets:lookup(LoginTb, Session) of
		[] -> ok;
		[LoginItem] ->
			?FILE_LOG_DEBUG("~p logout success.", [LoginItem#login_tb.account]),
			catch rpc:cast(ServiceNode, friend_service_monitor, fd_event_handle, [logout, LoginItem#login_tb.remote_pid]),
			ets:delete(LoginTb, Session)
	end,
	{noreply, State};
handle_cast({send_friend_msg_ack, {Session,MsgId}}=Event, #state{login_tb=LoginTb, friend_service_node=ServiceNode}=State) ->
	?FILE_LOG_DEBUG("~p", [Event]),
	case ets:lookup(LoginTb, Session) of
		[] -> ok;
		[LoginItem] ->
			catch rpc:cast(ServiceNode, friend_service_monitor, fd_event_handle, [send_friend_msg_ack, {LoginItem#login_tb.remote_pid, MsgId}])
	end,
	{noreply, State};
handle_cast({send_friend_req_response_ack, {Session, AckId}}=Event, #state{login_tb=LoginTb, friend_service_node=ServiceNode}=State) ->
	?FILE_LOG_DEBUG("~p", [Event]),
	case ets:lookup(LoginTb, Session) of
		[] -> ok;
		[LoginItem] ->
			catch rpc:cast(ServiceNode, friend_service_monitor, fd_event_handle, [send_friend_req_response_ack, {LoginItem#login_tb.remote_pid, AckId}])
	end,
	{noreply, State};
handle_cast({option_friend_req, Session}=Event, #state{login_tb=LoginTb, friend_service_node=ServiceNode}=State) ->
	?FILE_LOG_DEBUG("~p", [Event]),
	case ets:lookup(LoginTb, Session) of
		[] -> ok;
		[LoginItem] ->
			catch rpc:cast(ServiceNode, friend_service_monitor, fd_event_handle, [option_friend_req, LoginItem#login_tb.remote_pid])
	end,
	{noreply, State};
handle_cast({get_undone_msg, Session}=Event, #state{login_tb=LoginTb, friend_service_node=ServiceNode}=State) ->
	?FILE_LOG_DEBUG("~p", [Event]),
	case ets:lookup(LoginTb, Session) of
		[] -> ok;
		[LoginItem] ->
			catch rpc:cast(ServiceNode, friend_service_monitor, fd_event_handle, [get_undone_msg, LoginItem#login_tb.remote_pid])
	end,
	{noreply, State};
handle_cast(_Request, State) ->
	{noreply, State}.

handle_info({friend_event, {Session, Value}},  #state{login_tb=LoginTb}=State) ->
	case ets:lookup(LoginTb, Session) of
		[] ->
			?FILE_LOG_ERROR("friend_event exception", []);
		[Item] ->
			friend_event_handle(Item#login_tb.from_pid, Value)
	end,
	{noreply, State};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

friend_event_handle(FromPid, {notify_friend_req, {FromAccount, ReqMsg}}) ->
	FromPid ! {notify_friend_req, {FromAccount, ReqMsg}};
friend_event_handle(FromPid, {notify_be_friend_del, FromAccount}) ->
	FromPid ! {notify_be_friend_del, FromAccount};
friend_event_handle(FromPid, {notify_friend_req_ack, {FromAccount, AckState, AckMsg}}) ->
	FromPid ! {notify_friend_req_ack, {FromAccount, AckState, AckMsg}};
friend_event_handle(FromPid, {notify_friend_msg, {FromAccount, MsgFormat, MsgData}}) ->
	FromPid ! {notify_friend_msg, {FromAccount, MsgFormat, MsgData}};
friend_event_handle(FromPid, {notify_friend_custom_data, {FromAccount, CustomData}}) ->
	FromPid ! {notify_friend_custom_data, {FromAccount, CustomData}};
friend_event_handle(FromPid, {notify_friend_state_change, {FromAccount, OnlineState}}) ->
	FromPid ! {notify_friend_state_change, {FromAccount, OnlineState}};
friend_event_handle(_FromPid, Other) ->
	?FILE_LOG_ERROR("friend_event_handle other=~p", [Other]),
	ok.