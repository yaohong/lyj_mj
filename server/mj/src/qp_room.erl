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
-include("../include/mj_pb.hrl").
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
-export([join/2, ready/4]).
-export([dismiss/1]).
-define(SERVER, ?MODULE).

-record(state, {
    owner_user_id :: integer(),         %%房间管理员的ID
    owner_is_join :: boolean(),         %%管理员是否进入
    room_id :: integer(),               %%房间ID
    room_type :: integer(),             %%房间类型(麻将玩法类型)
    seat_tree :: gb_trees:tree()        %%描述四个座位
}).

-record(seat_data, {user_data=undefined, is_ready=false}).

%%%===================================================================
%%% API
%%%===================================================================
join(RoomPid, UserData) ->
    gen_fsm:sync_send_event(RoomPid, {join, UserData}).


ready(RoomPid, UserKey, SeatNum, ReadyState) when is_boolean(ReadyState) ->
    gen_fsm:sync_send_event(RoomPid, {ready, {UserKey, SeatNum, ReadyState}}).

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
    EmptyTree = gb_trees:empty(),
    Tree0 = gb_trees:insert(0, undefined, EmptyTree),
    Tree1 = gb_trees:insert(1, undefined, Tree0),
    Tree2 = gb_trees:insert(2, undefined, Tree1),
    Tree3 = gb_trees:insert(3, undefined, Tree2),
    {ok, idle, #state{owner_user_id = OwnerUserId, owner_is_join = false, room_id = RoomId,room_type = RoomType, seat_tree = Tree3}}.

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
idle(dis_miss, #state{seat_tree = SeatTree} = State) ->
    %%这个时候房间不应该有人
    [] = extract_room_users(SeatTree),
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
find_idle_seat([], _SeatTree) ->
    false;
find_idle_seat([SeatNumer|T], SeatTree) ->
    SeatData = gb_trees:get(SeatNumer, SeatTree),
    if
        SeatData =:= undefined -> {true, SeatNumer};
        true -> find_idle_seat(T, SeatTree)
    end.

%%提取房间里的所有玩家
extract_room_users(SeatTree) ->
    extract_room_users([0,1,2,3], SeatTree, []).
extract_room_users([], _,  Users) -> Users;
extract_room_users([SeatNum|T], SeatTree, Users) ->
    SeatData = gb_trees:get(SeatNum, SeatTree),
    if
        SeatData =:= undefined ->
            %%座位上没有人
            extract_room_users(T, SeatTree, Users);
        true ->
            RoomUser = {SeatData#seat_data.user_data, SeatNum, SeatData#seat_data.is_ready},
            extract_room_users(T, SeatTree, [RoomUser|Users])
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
idle({join, UserData}, _From, #state{owner_user_id = OwnerUserId, owner_is_join = OwnerIsJoin, room_id = RoomId, seat_tree = SeatTree} = State) ->
    JoinUserId = UserData:get(user_id),
    if
        OwnerIsJoin =:= false andalso JoinUserId =/= OwnerUserId ->
            %%管理员还没有进入，进入失败
            ?FILE_LOG_WARNING("join room[~p] failed, owner_is_join=~p, owner_user_id=~p, join_user_id=~p", [RoomId, OwnerIsJoin, OwnerUserId, JoinUserId]),
            {reply, failed, idle, State};
        OwnerIsJoin =:= false andalso JoinUserId =:= OwnerUserId ->
            %%管理员进入,进入0号位置
            SeatData = gb_trees:get(0, SeatTree),
            true = (SeatData =:= undefined),
            NewSeatTree = gb_trees:update(0, #seat_data{user_data = UserData}, SeatTree),
            ?FILE_LOG_WARNING("join room[~p] success, owner_user_id=~p, join_user_id=~p", [RoomId, OwnerUserId, JoinUserId]),
            {reply, {success, {0, false, []}}, idle, State#state{owner_is_join = true, seat_tree = NewSeatTree}};
        true ->
            %%普通玩家进入
            case find_idle_seat([1,2,3], SeatTree) of
                false ->
                    %%没有空闲座位了
                    ?FILE_LOG_WARNING("join room[~p] failed, not idle seat, join_user_id=~p", [RoomId, JoinUserId]),
                    {reply, failed, idle, State};
                {true, IdleSeatNum} ->
                    %%找到座位号了
                    %%获取其他位置的玩家信息
                    RoomUsers = extract_room_users(SeatTree),
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
                    lists:foreach(
                        fun({RoomUserData, _, _}) ->
                            RoomUserData:send_room_bin_msg(PushBin)
                        end, RoomUsers),
                    NewSeatTree = gb_trees:update(IdleSeatNum, #seat_data{user_data = UserData}, SeatTree),
                    ?FILE_LOG_WARNING("join room[~p] success, owner_user_id=~p, join_user_id=~p", [RoomId, OwnerUserId, JoinUserId]),
                    {reply, {success, {IdleSeatNum, false, RoomUsers}}, idle, State#state{seat_tree = NewSeatTree}}
            end
    end;
idle({ready, {UserKey, SeatNum, ReadyState}}, _From, #state{seat_tree = SeatTree} = State) ->
    case gb_trees:lookup(SeatNum, SeatTree) of
        {value, undefined} ->
            ?FILE_LOG_WARNING("user_id[~p] ready seat_num[~p] undefined", [UserKey:get(user_id), SeatNum]),
            {reply, failed, idle, State};
        {value, SeatData} when is_record(SeatData,seat_data) ->
            SeatUserData = SeatData#seat_data.user_data,
            SeatUserIsReady  =SeatData#seat_data.is_ready,
            case UserKey:compare(SeatUserData) of
                true ->
                    if
                        ReadyState =:= SeatUserIsReady ->
                            %%状态相同不做转发通知了
                            ok;
                        true ->
                            %%转发消息
                            ReadyPush = #qp_ready_push{seat_number = SeatNum, ready_state = ReadyState},
                            PushBin = qp_proto:encode_qp_packet(ReadyPush),
                            RoomUsers = extract_room_users(SeatTree),
                            lists:foreach(
                                fun({RoomUserData, _, _}) ->
                                    case UserKey:compare(SeatUserData) of
                                        true -> ok; %%自己不转发
                                        false -> RoomUserData:send_room_bin_msg(PushBin)
                                    end
                                end, RoomUsers)
                    end,
                    NewSeatData = SeatData#seat_data{is_ready = ReadyState},
                    NewSeatTree = gb_trees:update(SeatNum, NewSeatData, SeatTree),
                    {reply, {success, ReadyState}, idle, State#state{seat_tree = NewSeatTree}};
                false ->
                    %%不是同一个人
                    ?FILE_LOG_WARNING(
                        "user[~p, ~p] ready seat_num[~p], ready_user_id[~p,~p]",
                        [UserKey:get(user_id), UserKey:get(user_pid), SeatNum, SeatUserData:get(user_id), SeatUserData:get(user_pid)]),
                    {reply, failed, idle, State}
            end
    end;
idle(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, idle, State}.


game(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, game, State}.

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
terminate(_Reason, _StateName, #state{room_id = RoomId}) ->
    ?FILE_LOG_DEBUG("room_id[~p] [~p] terminate", [RoomId, _StateName]),
    qp_room_manager:destroy_room(RoomId),
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
