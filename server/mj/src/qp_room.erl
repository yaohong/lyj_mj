%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 十月 2016 16:33
%%%-------------------------------------------------------------------
-module(qp_room).
-author("yaohong").

-behaviour(gen_fsm).

%% API
-export([start_link/3]).
-include("../deps/file_log/include/file_log.hrl").
-include("../include/common_pb.hrl").
%% gen_fsm callbacks
-export([init/1,
	idle/2,
%%         game/2,
	idle/3,
	game/3,
	handle_event/3,
	handle_sync_event/4,
	handle_info/3,
	terminate/3,
	code_change/4]).
-export([join/2, quit/3, ready/4]).
-export([dismiss/1]).
-export([game_data/4]).
-define(SERVER, ?MODULE).
-record(seat_data, {user_data=undefined, is_ready=false}).
-record(state, {
	owner_user_id :: integer(),         %%房间管理员的ID
	owner_is_join :: boolean(),         %%管理员是否进入
	room_id :: integer(),               %%房间ID
	room_type :: integer(),             %%房间类型(麻将玩法类型)
	seat_list :: [{SeatNum :: integer(), #seat_data{}}],       %%描述四个座位
	game_private_data :: term()           %%游戏的私有数据
}).



%%%===================================================================
%%% API
%%%===================================================================
join(RoomPid, UserData) ->
	gen_fsm:sync_send_event(RoomPid, {join, UserData}).

quit(RoomPid, UserKey, SeatNum) ->
	gen_fsm:sync_send_event(RoomPid, {quit, {UserKey, SeatNum}}).


ready(RoomPid, UserKey, SeatNum, ReadyState) when is_boolean(ReadyState) ->
	gen_fsm:sync_send_event(RoomPid, {ready, {UserKey, SeatNum, ReadyState}}).


game_data(RoomPid, UserKey, SeatNum, GameData) ->
	gen_fsm:sync_send_event(RoomPid, {game_data, {UserKey, SeatNum, GameData}}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%解散房间
dismiss(RoomPid) ->
	gen_fsm:send_event(RoomPid, dis_miss).
%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(OwnerUserId :: integer(), RoomId :: integer(), RoomType :: integer()) ->
	{ok, pid()} | ignore | {error, Reason :: term()}).
start_link(OwnerUserId, RoomId, RoomType) ->
	gen_fsm:start_link(?MODULE, [OwnerUserId, RoomId, RoomType], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
	{ok, StateName :: atom(), StateData :: #state{}} |
	{ok, StateName :: atom(), StateData :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([OwnerUserId, RoomId, RoomType]) ->
	SeatList = [{0, undefined}, {1, undefined}, {2, undefined}, {3, undefined}],
	LogicMod = qp_logic_cfg:get_logic_mod(RoomType),
	InitGamePrivateData = LogicMod:init_private_data(),
	{ok,
		idle,
		#state{
			owner_user_id = OwnerUserId,
			owner_is_join = false,
			room_id = RoomId,
			room_type = RoomType,
			seat_list = SeatList,
			game_private_data = InitGamePrivateData}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @end
%%--------------------------------------------------------------------
%%-spec(idle(Event :: term(), State :: #state{}) ->
%%    {next_state, NextStateName :: atom(), NextState :: #state{}} |
%%    {next_state, NextStateName :: atom(), NextState :: #state{},
%%     timeout() | hibernate} |
%%    {stop, Reason :: term(), NewState :: #state{}}).
idle(dis_miss, #state{seat_list = SeatList} = State) ->
	%%这个时候房间不应该有人
	[] = SeatList,
	{stop, normal, State};
idle(_Event, State) ->
	{next_state, state_name, State}.
%%
%%
%%game(_Event, State) ->
%%    {next_state, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @end
%%--------------------------------------------------------------------
%%给普通玩家查找位置
find_idle_seat(SeatList) ->
	find_idle_seat([1,2,3], SeatList).
find_idle_seat([], _SeatList) ->
	false;
find_idle_seat([SeatNumer|T], SeatList) ->
	SeatData = proplists:get_value(SeatNumer, SeatList),
	if
		SeatData =:= undefined -> {true, SeatNumer};
		true -> find_idle_seat(T, SeatList)
	end.

%%提取房间里的所有玩家
extract_room_users(SeatList) ->
	extract_room_users(SeatList, []).
extract_room_users([],  Users) -> Users;
extract_room_users([{SeatNum, SeatData}|T], Users) ->
	if
		SeatData =:= undefined ->
			%%座位上没有人
			extract_room_users(T, Users);
		true ->
			RoomUser = {SeatData#seat_data.user_data, SeatNum, SeatData#seat_data.is_ready},
			extract_room_users(T, [RoomUser|Users])
	end.

%%添加玩家到座位
add_user_to_seat(0, UserData, [{0, undefined}|T]) ->
	[{0, #seat_data{user_data = UserData}}|T];
add_user_to_seat(1, UserData, [S0, {1, undefined}|T]) ->
	[S0, {1, #seat_data{user_data = UserData}}|T];
add_user_to_seat(2, UserData, [S0, S1, {2, undefined}|T]) ->
	[S0, S1, {2, #seat_data{user_data = UserData}}|T];
add_user_to_seat(3, UserData, [S0, S1, S2, {3, undefined}]) ->
	[S0, S1, S2, {3, #seat_data{user_data = UserData}}].


%%更新座位信息

update_seat_user(0, NewSeatData, [{0, OldSeatData}|T]) when is_record(NewSeatData, seat_data) andalso is_record(OldSeatData, seat_data) ->
	[{0, NewSeatData}|T];
update_seat_user(1, NewSeatData, [S0, {1, OldSeatData}|T]) when is_record(NewSeatData, seat_data) andalso is_record(OldSeatData, seat_data) ->
	[S0, {1, NewSeatData}|T];
update_seat_user(2, NewSeatData, [S0, S1, {2, OldSeatData}|T]) when is_record(NewSeatData, seat_data) andalso is_record(OldSeatData, seat_data) ->
	[S0, S1, {2, NewSeatData}|T];
update_seat_user(3, NewSeatData, [S0, S1, S2, {3, OldSeatData}]) when is_record(NewSeatData, seat_data) andalso is_record(OldSeatData, seat_data) ->
	[S0, S1, S2, {3, NewSeatData}].


quit_user_from_seat(UserKey, 0, [{0, SeatData}|T]) when is_record(SeatData, seat_data) ->
	case UserKey:compare(SeatData#seat_data.user_data) of
		false -> failed;
		true -> {success, [{0, undefined}|T]}
	end;
quit_user_from_seat(UserKey, 1, [S0, {1, SeatData}|T]) when is_record(SeatData, seat_data) ->
	case UserKey:compare(SeatData#seat_data.user_data) of
		false -> failed;
		true -> {success, [S0, {1, undefined}|T]}
	end;
quit_user_from_seat(UserKey, 2, [S0, S1, {2, SeatData}|T]) when is_record(SeatData, seat_data) ->
	case UserKey:compare(SeatData#seat_data.user_data) of
		false -> failed;
		true -> {success, [S0, S1, {2, undefined}|T]}
	end;
quit_user_from_seat(UserKey, 3, [S0, S1, S2, {0, SeatData}]) when is_record(SeatData, seat_data) ->
	case UserKey:compare(SeatData#seat_data.user_data) of
		false -> failed;
		true -> {success, [S0, S1, S2, {3, undefined}]}
	end.





-spec(idle(Event :: term(), From :: {pid(), term()},
	State :: #state{}) ->
	{next_state, NextStateName :: atom(), NextState :: #state{}} |
	{next_state, NextStateName :: atom(), NextState :: #state{},
		timeout() | hibernate} |
	{reply, Reply, NextStateName :: atom(), NextState :: #state{}} |
	{reply, Reply, NextStateName :: atom(), NextState :: #state{},
		timeout() | hibernate} |
	{stop, Reason :: normal | term(), NewState :: #state{}} |
	{stop, Reason :: normal | term(), Reply :: term(),
		NewState :: #state{}}).
idle({join, UserData}, _From, #state{owner_user_id = OwnerUserId, owner_is_join = OwnerIsJoin, room_id = RoomId, seat_list = SeatList} = State) ->
	JoinUserId = UserData:get(user_id),
	if
		OwnerIsJoin =:= false andalso JoinUserId =/= OwnerUserId ->
			%%管理员还没有进入，进入失败
			?FILE_LOG_WARNING("join room[~p] failed, owner_is_join=~p, owner_user_id=~p, join_user_id=~p", [RoomId, OwnerIsJoin, OwnerUserId, JoinUserId]),
			{reply, failed, idle, State};
		OwnerIsJoin =:= false andalso JoinUserId =:= OwnerUserId ->
			%%管理员进入,进入0号位置
			undefined = proplists:get_value(0, SeatList),
			NewSeatList = add_user_to_seat(0, UserData, SeatList),
			?FILE_LOG_WARNING("join room[~p] success, owner_user_id=~p, join_user_id=~p", [RoomId, OwnerUserId, JoinUserId]),
			{reply, {success, {0, false, []}}, idle, State#state{owner_is_join = true, seat_list = NewSeatList}};
		true ->
			%%普通玩家进入
			case find_idle_seat(SeatList) of
				false ->
					%%没有空闲座位了
					?FILE_LOG_WARNING("join room[~p] failed, not idle seat, join_user_id=~p", [RoomId, JoinUserId]),
					{reply, failed, idle, State};
				{true, IdleSeatNum} ->
					%%找到座位号了
					%%获取其他位置的玩家信息
					RoomUsers = extract_room_users(SeatList),
					%%编码进入房间的消息广播给房间里的其他用户
					Push = #qp_join_room_push{
						room_user = #qp_room_user{
							user_data = #qp_user_data{
								user_id = UserData:get(user_id),
								gold = UserData:get(gold),
								avatar_url = UserData:get(avatar_url),
								nick_name = UserData:get(nick_name)
							},
							seat_number = IdleSeatNum,
							is_ready = false
						}
					},
					PushBin = qp_proto:encode_qp_packet(Push),
					broadcast(SeatList, {room_bin_msg, PushBin}),
					NewSeatList = add_user_to_seat(IdleSeatNum, UserData, SeatList),
					?FILE_LOG_WARNING("join room[~p] success, owner_user_id=~p, join_user_id=~p", [RoomId, OwnerUserId, JoinUserId]),
					{reply, {success, {IdleSeatNum, false, RoomUsers}}, idle, State#state{seat_list = NewSeatList}}
			end
	end;
idle({ready, {UserKey, SeatNum, ReadyState}}, _From, #state{seat_list = SeatList, room_type = RoomType, room_id = RoomId, game_private_data = GamePrivateData} = State) ->
	case proplists:get_value(SeatNum, SeatList, none) of
		undefined ->
			?FILE_LOG_WARNING("user_id[~p] ready seat_num[~p] undefined", [UserKey:get(user_id), SeatNum]),
			{reply, failed, idle, State};
		none ->
			?FILE_LOG_WARNING("user_id[~p] ready seat_num[~p] none", [UserKey:get(user_id), SeatNum]),
			{reply, failed, idle, State};
		SeatData when is_record(SeatData,seat_data) ->
			SeatUserData = SeatData#seat_data.user_data,
			SeatUserIsReady = SeatData#seat_data.is_ready,
			case UserKey:compare(SeatUserData) of
				true ->
					if
						ReadyState =:= SeatUserIsReady ->
							%%状态相同不做转发通知了
							?FILE_LOG_DEBUG("user_id[~p] ready_state=seat_user_is_ready not Forward", [UserKey:get(user_id)]),
							ok;
						true ->
							%%转发消息
							ReadyPush = #qp_ready_push{seat_number = SeatNum, ready_state = ReadyState},
							PushBin = qp_proto:encode_qp_packet(ReadyPush),
							broadcast(SeatList, {room_bin_msg, PushBin}, fun(UserUserItem) -> UserKey:compare(UserUserItem) end ),
							ok
					end,
					NewSeatData = SeatData#seat_data{is_ready = ReadyState},
					NewSeatList = update_seat_user(SeatNum, NewSeatData, SeatList),
					%%检测是否全部就位
					case check_all_ready(NewSeatList) of
						true ->
							%%全部准备好了
							?FILE_LOG_DEBUG("check_all_ready true", []),
							broadcast(NewSeatList, {change_game_state, RoomId}),
							LogicMod = qp_logic_cfg:get_logic_mod(RoomType),
							{continue, {NewGamePrivateData, SendGameData}} = LogicMod:game_start(GamePrivateData),
							send_game_data(SendGameData, NewSeatList),
							%%所有玩家切换到游戏状态
							{reply, {success, ReadyState}, game, State#state{seat_list = NewSeatList, game_private_data = NewGamePrivateData}};
						false ->
							%%没有准备好
							{reply, {success, ReadyState}, idle, State#state{seat_list = NewSeatList}}
					end;
				false ->
					%%不是同一个人
					?FILE_LOG_WARNING(
						"user[~p, ~p] ready seat_num[~p], ready_user_id[~p,~p]",
						[UserKey:get(user_id), UserKey:get(user_pid), SeatNum, SeatUserData:get(user_id), SeatUserData:get(user_pid)]),
					{reply, failed, idle, State}
			end
	end;
idle({quit, {UserKey, SeatNum}}, _From, #state{seat_list = SeatList, room_id = RoomId} = State) ->
	case quit_user_from_seat(UserKey, SeatNum, SeatList) of
		failed -> {reply, failed, idle, State};
		{success, NewSeatList} ->
			NewState = State#state{seat_list = NewSeatList},
			%%广播给其他人
			RoomUsers = extract_room_users(NewSeatList),
			if
				length(RoomUsers) =:= 0 ->
					%%房间没人了
					?FILE_LOG_DEBUG("room_id[~p] user_id[~p] exit_room, room empty.", [RoomId, UserKey:get(user_id)]),
					{stop, normal, success, NewState};
				true ->
					if
						SeatNum =:= 0 ->
							%%房主退出了，房间解散
							%%发送房间解散的消息
							?FILE_LOG_DEBUG("room_id[~p] banker_user_Id[~p] exit_room, room dismiss.", [RoomId, UserKey:get(user_id)]),
							broadcast(NewSeatList, {room_dismiss, RoomId}),
							{stop, normal, success, NewState};
						true ->
							%%广播其他用户，有人退出房间了
							?FILE_LOG_DEBUG("room_id[~p] user_id[~p] exit_room.", [RoomId, UserKey:get(user_id)]),
							ExitPushBin = qp_proto:encode_qp_packet(#qp_exit_room_push{seat_number = SeatNum}),
							broadcast(NewSeatList, {room_bin_msg, ExitPushBin}),
							{reply, success, idle, NewState}
					end
			end
	end;
idle(Event, _From, State) ->
	?FILE_LOG_WARNING("room [idle] event=~p", [Event]),
	{reply, failed, idle, State}.

game({game_data, {UserKey, SeatNum, GameData}}, _From, #state{seat_list = SeatList, room_type = RoomType, game_private_data = GamePrivateData} = State) ->
	case proplists:get_value(SeatNum, SeatList, none) of
		undefined ->
			?FILE_LOG_WARNING("user_id[~p] game_data seat_num[~p] undefined", [UserKey:get(user_id), SeatNum]),
			{reply, failed, game, State};
		none ->
			?FILE_LOG_WARNING("user_id[~p] game_data seat_num[~p] none", [UserKey:get(user_id), SeatNum]),
			{reply, failed, game, State};
		SeatData when is_record(SeatData,seat_data) ->
			SeatUserData = SeatData#seat_data.user_data,
			case UserKey:compare(SeatUserData) of
				true ->
					LogicMod = qp_logic_cfg:get_logic_mod(RoomType),
					case LogicMod:game_oper(GamePrivateData, SeatNum, GameData) of
						{continue, {NewGamePrivateData, SendGameData}} ->
							send_game_data(SendGameData, SeatList),
							?FILE_LOG_DEBUG("game_continue.", []),
							{reply, success, game, State#state{game_private_data = NewGamePrivateData}};
						{game_end, {NewGamePrivateData1, BrocastBin}} ->
							broadcast(SeatList, {room_bin_msg, BrocastBin}),
							NewSeatList =
								lists:map(
									fun({TmpSeatNumber, TmpSeatData}) ->
										{TmpSeatNumber, TmpSeatData#seat_data{is_ready = false}}
									end, SeatList),
							?FILE_LOG_DEBUG("game_end.", []),
							{reply, success, idle, State#state{game_private_data = NewGamePrivateData1, seat_list = NewSeatList}}
					end;
				false ->
					%%不是同一个人
					?FILE_LOG_WARNING(
						"user[~p, ~p] game_data seat_num[~p], ready_user_id[~p,~p]",
						[UserKey:get(user_id), UserKey:get(user_pid), SeatNum, SeatUserData:get(user_id), SeatUserData:get(user_pid)]),
					{reply, failed, game, State}
			end
	end;
game({quit, {UserKey, SeatNum}}, _From, #state{seat_list = SeatList, room_id = RoomId, room_type = RoomType, game_private_data = GamePrivateData} = State) ->
	case quit_user_from_seat(UserKey, SeatNum, SeatList) of
		failed -> {reply, failed, game, State};
		{success, NewSeatList} ->
			%%广播游戏数据
			LogicMod = qp_logic_cfg:get_logic_mod(RoomType),
			{BrocastBin, NewGamePrivateData} = LogicMod:game_quit(GamePrivateData, SeatNum),
			broadcast(NewSeatList, {room_bin_msg, BrocastBin}),
			%%广播退出房间的数据
			?FILE_LOG_DEBUG("room_id[~p] user_id[~p] exit_room state[game=>idle].", [RoomId, UserKey:get(user_id)]),
			ExitPushBin = qp_proto:encode_qp_packet(#qp_exit_room_push{seat_number = SeatNum}),
			broadcast(NewSeatList, {room_bin_msg, ExitPushBin}),

			NewSeatList1 =
				lists:map(
					fun({TmpSeatNumber, TmpSeatData}) ->
						{TmpSeatNumber, TmpSeatData#seat_data{is_ready = false}}
					end, NewSeatList),
			{reply, success, idle, State#state{seat_list = NewSeatList1, game_private_data = NewGamePrivateData}}
%%			%%广播给其他人
%%			RoomUsers = extract_room_users(NewSeatList),
%%			if
%%				length(RoomUsers) =:= 0 ->
%%					%%房间没人了
%%					?FILE_LOG_DEBUG("room_id[~p] user_id[~p] exit_room, room empty.", [RoomId, UserKey:get(user_id)]),
%%					{stop, normal, success, NewState};
%%				true ->
%%					if
%%						SeatNum =:= 0 ->
%%							%%房主退出了，房间解散
%%							%%发送房间解散的消息
%%							?FILE_LOG_DEBUG("room_id[~p] banker_user_Id[~p] exit_room, room dismiss.", [RoomId, UserKey:get(user_id)]),
%%							broadcast(NewSeatList, {room_dismiss, RoomId}),
%%							{stop, normal, success, NewState};
%%						true ->
%%							%%广播其他用户，有人退出房间了
%%							?FILE_LOG_DEBUG("room_id[~p] user_id[~p] exit_room.", [RoomId, UserKey:get(user_id)]),
%%							ExitPushBin = qp_proto:encode_qp_packet(#qp_exit_room_push{seat_number = SeatNum}),
%%							broadcast(NewSeatList, {room_bin_msg, ExitPushBin}),
%%							{reply, success, idle, NewState}
%%					end
%%			end
	end;
game(Event, _From, State) ->
	?FILE_LOG_WARNING("room [game] event=~p", [Event]),
	{reply, failed, game, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_event(Event :: term(), StateName :: atom(),
	StateData :: #state{}) ->
	{next_state, NextStateName :: atom(), NewStateData :: #state{}} |
	{next_state, NextStateName :: atom(), NewStateData :: #state{},
		timeout() | hibernate} |
	{stop, Reason :: term(), NewStateData :: #state{}}).
handle_event(_Event, StateName, State) ->
	{next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_sync_event(Event :: term(), From :: {pid(), Tag :: term()},
	StateName :: atom(), StateData :: term()) ->
	{reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term()} |
	{reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term(),
		timeout() | hibernate} |
	{next_state, NextStateName :: atom(), NewStateData :: term()} |
	{next_state, NextStateName :: atom(), NewStateData :: term(),
		timeout() | hibernate} |
	{stop, Reason :: term(), Reply :: term(), NewStateData :: term()} |
	{stop, Reason :: term(), NewStateData :: term()}).
handle_sync_event(_Event, _From, StateName, State) ->
	Reply = ok,
	{reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: term(), StateName :: atom(),
	StateData :: term()) ->
	{next_state, NextStateName :: atom(), NewStateData :: term()} |
	{next_state, NextStateName :: atom(), NewStateData :: term(),
		timeout() | hibernate} |
	{stop, Reason :: normal | term(), NewStateData :: term()}).
handle_info(_Info, StateName, State) ->
	{next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: normal | shutdown | {shutdown, term()}
| term(), StateName :: atom(), StateData :: term()) ->
	term()).
terminate(_Reason, _StateName, #state{room_id = RoomId, seat_list = SeatList}) ->
	?FILE_LOG_DEBUG("room_id[~p] [~p] terminate, reason=~p", [RoomId, _StateName, _Reason]),
	qp_room_manager:destroy_room(RoomId),
	broadcast(SeatList, {room_dismiss, RoomId}),
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, StateName :: atom(),
	StateData :: #state{}, Extra :: term()) ->
	{ok, NextStateName :: atom(), NewStateData :: #state{}}).
code_change(_OldVsn, StateName, State, _Extra) ->
	{ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

check_all_ready([]) -> true;
check_all_ready([{_, undefined}|_]) -> false;
check_all_ready([{_, SeatData}|T]) ->
	if
		SeatData#seat_data.is_ready =:= true ->
			check_all_ready(T);
		true -> false
	end.



broadcast(SeatList, Msg) ->
	lists:foreach(
		fun({_, SeatData}) ->
			if
				SeatData =/= undefined ->
					UserData = SeatData#seat_data.user_data,
					UserData:send_room_msg(Msg);
				true -> ok
			end
		end, SeatList).

broadcast(SeatList, Msg, FilterFun) when is_function(FilterFun) ->
	lists:foreach(
		fun({_, SeatData}) ->
			if
				SeatData =/= undefined ->
					UserData = SeatData#seat_data.user_data,
					case FilterFun(UserData) of
						true -> ok;
						false -> UserData:send_room_msg(Msg)
					end;
				true -> ok
			end
		end, SeatList).


send_game_data({BroadcastHeadBinData, SendSeatData, BroadcastTailBinData}, SeatList) ->
	broadcast(SeatList, {room_bin_msg, BroadcastHeadBinData}),
	lists:foreach(
		fun({SeatNumber, SeatBinData}) ->
			case proplists:get_value(SeatNumber, SeatList, none) of
				none -> ok;
				undefined -> ok;
				SeatData when is_record(SeatData, seat_data) ->
					UserData = SeatData#seat_data.user_data,
					UserData:send_room_msg({room_bin_msg, SeatBinData})
			end
		end,  SendSeatData),
	broadcast(SeatList, {room_bin_msg, BroadcastTailBinData}),
	ok.