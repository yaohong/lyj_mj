%% Copyright
-module(friend_api).
-author("yaohong").

-include("file_log.hrl").
-include("friend.hrl").
%% API
-export([
	fd_login/3,
	fd_logout/2,
	fd_add_friend/3,
	fd_del_friend/2,
	fd_send_friend_req_ack/4,
	fd_send_friend_msg/3,
	fd_send_friend_custom_data/3,
	fd_get_friend_list/3,
	fd_send_friend_msg_ack/3,
	fd_send_friend_req_response_ack/3,
	fd_option_friend_req/2,
	fd_get_undone_msg/2]).


-spec fd_login(
	FromAccount :: string(),
	FromPid :: pid(),
	GameId :: string()) -> {success, Session :: reference()} |  {fail, Code :: integer()}.
fd_login(FromAccount, FromPid, GameId) when is_list(FromAccount) andalso is_pid(FromPid) andalso is_list(GameId) ->
    ProcessName = friend_sup:get_process(FromAccount),
    case catch gen_server:call(ProcessName, {login, {FromAccount, FromPid, GameId}}) of
	    {'EXIT', Reason} ->
		    ?FILE_LOG_ERROR("fd_login fail reason=~p", [Reason]),
		    {fail, ?FD_OTHER_ERROR};
	    Other -> Other
    end.



%%登出
-spec fd_logout(FromAccount :: string(), Session :: reference()) -> ok.
fd_logout(FromAccount, Session) ->
	ProcessName = friend_sup:get_process(FromAccount),
    catch gen_server:cast(ProcessName, {logout, Session}),
	ok.

%%添加好友
-spec fd_add_friend(
	{FromAccount :: string(),
	 Session :: reference()},
	ToAccount :: string(),
	ReqMsg :: string()) -> success | {fail, Code :: integer()}.
fd_add_friend({FromAccount, Session}, ToAccount, ReqMsg) ->
	ProcessName = friend_sup:get_process(FromAccount),
    case catch gen_server:call(ProcessName, {add_friend, {Session, ToAccount, ReqMsg}}) of
	    {'EXIT',Reason} ->
		    ?FILE_LOG_ERROR("fd_add_friend fail reason=~p", [Reason]),
		    {fail, ?FD_OTHER_ERROR};
	    Other -> Other
	end.

%%删除好友
-spec fd_del_friend(
	{Account :: string(), Session :: reference()},
	FriendAccount :: string()) -> success | {fail, Code :: integer()}.
fd_del_friend({Account, Session}, FriendAccount) ->
	ProcessName = friend_sup:get_process(Account),
	case catch gen_server:call(ProcessName, {del_friend, {Session, FriendAccount}}) of
		{'EXIT', Reason} ->
			?FILE_LOG_ERROR("fd_del_friend reason=~p", [Reason]),
			{fail, ?FD_OTHER_ERROR};
		Other -> Other
	end.

%%应答其他人申请加我为好友的请求
-spec fd_send_friend_req_ack(
	{Account :: string(), Session :: reference()},
	ToAccount :: string(),
	AckState :: boolean(),
	AckMsg :: string()) -> success | {fail, Code :: integer()}.
fd_send_friend_req_ack({Account, Session}, ToAccount, AckState, AckMsg) ->
    ProcessName = friend_sup:get_process(Account),
	case catch gen_server:call(ProcessName, {friend_req_ack, {Session, ToAccount, AckState,AckMsg}}) of
		{'EXIT', Reason} ->
			?FILE_LOG_ERROR("fd_friend_req_ack reason=~p", [Reason]),
			{fail, ?FD_OTHER_ERROR};
		Other -> Other
	end.

%%发送好友消息
-spec fd_send_friend_msg(
	{Account :: string(), Session :: reference()},
	FriendAccount :: string(),
	{MsgData :: string(), MsgFormat :: string(), MsgId :: integer()}) -> success | {fail, Code :: integer()}.
fd_send_friend_msg({Account, Session}, FriendAccount, {MsgData, MsgFormat, MsgId}) ->
	ProcessName = friend_sup:get_process(Account),
	case catch gen_server:call(ProcessName, {friend_msg, {Session, FriendAccount, {MsgData, MsgFormat, MsgId}}}) of
		{'EXIT', Reason} ->
			?FILE_LOG_ERROR("fd_friend_msg reason=~p", [Reason]),
			{fail, ?FD_OTHER_ERROR};
		Other -> Other
	end.
-spec fd_send_friend_custom_data(
	{Account :: string(), Session :: reference()},
	FriendAccount :: string(),
	CustomData :: any()) -> success | {fail, Code :: integer()}.
fd_send_friend_custom_data({Account, Session}, FriendAccount, CustomData) ->
	ProcessName = friend_sup:get_process(Account),
	case catch gen_server:call(ProcessName, {friend_custom_data, {Session, FriendAccount, CustomData}}) of
		{'EXIT', Reason} ->
			?FILE_LOG_ERROR("fd_send_friend_custom_data reason=~p", [Reason]),
			{fail, ?FD_OTHER_ERROR};
		Other -> Other
	end.

%%获取好友列表
-spec fd_get_friend_list(
	Account :: string(),
	Session :: reference(),
	Mode :: mode()) ->
	{success, {State :: mode(), [{FriendAccount :: string(), State :: integer()}]}} |
	{fail, Code :: integer()}.
fd_get_friend_list(Account, Session, Mode) ->
	ProcessName = friend_sup:get_process(Account),
	case catch gen_server:call(ProcessName, {get_friend_list, {Session, Mode}}) of
		{'EXIT', Reason} ->
			?FILE_LOG_ERROR("fd_get_friend_list reason=~p", [Reason]),
			{fail, ?FD_OTHER_ERROR};
		Other -> Other
	end.

%%通知服务器我收到了好友发给我的消息
-spec fd_send_friend_msg_ack(Account :: string(), Session :: reference(), MsgId :: integer()) -> ok.
fd_send_friend_msg_ack(Account, Session, MsgId) ->
	ProcessName = friend_sup:get_process(Account),
	catch gen_server:cast(ProcessName, {send_friend_msg_ack, {Session,MsgId}}),
	ok.

%%通知服务器，我收到了我请求的目标对我的应答
-spec fd_send_friend_req_response_ack(Account :: string(), Session :: reference(), AckId :: integer()) -> ok.
fd_send_friend_req_response_ack(Account, Session, AckId) ->
	ProcessName = friend_sup:get_process(Account),
	catch gen_server:cast(ProcessName, {send_friend_req_response_ack, {Session, AckId}}),
	ok.


%%通知服务器，我看到了好友请求（将当前对我的请求设置为已读）.
-spec fd_option_friend_req(Account :: string(), Session :: reference()) -> ok.
fd_option_friend_req(Account, Session) ->
	ProcessName = friend_sup:get_process(Account),
	catch gen_server:cast(ProcessName, {option_friend_req, Session}),
	ok.

%%获取所有未处理的消息
-spec fd_get_undone_msg(Account :: string(), Session :: reference()) -> ok.
fd_get_undone_msg(Account, Session) ->
	ProcessName = friend_sup:get_process(Account),
	catch gen_server:cast(ProcessName, {get_undone_msg, Session}).


