%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 十月 2016 19:49
%%%-------------------------------------------------------------------
-module(qp_user).
-author("yaohong").

-behaviour(gen_fsm).

-include("qp_type.hrl").
-include("../deps/file_log/include/file_log.hrl").
-include("qp_proto.hrl").
-include("../include/mj_pb.hrl").
%% API

%% gen_fsm callbacks
-export([init/1,
  wait_login/2,              %%等待登陆
  hall/2,                    %%大厅
  room/2,                    %%房间
  game/2,                    %%游戏
  handle_event/3,
  handle_sync_event/4,
  handle_info/3,
  terminate/3,
  code_change/4]).
-export([head_len/2,
  closed/1,
  complete_packet/2,
  timer_callback/2
]).
-export([
  start/3,
  start_link/2
]).
-define(SERVER, ?MODULE).
-define(TIMER_SPACE, 3).
-define(RECV_TIMEOUT, 10).
-record(user_data, {user_id, gold, nickname, avatar_url}).
-record(room_data, {room_id, seat_num, room_pid}).
-record(state, {
  receiveMonitor,
  sockModule,
  sockData,
  last_recv_packet_time,
  user_data=undefined,
  room_data=undefined
}).

%%%===================================================================
%%% API
%%%===================================================================
head_len(HeadBin, _HeadLen) when is_binary(HeadBin) ->
  %%读取头
  <<PacketSize:?BIG_UINT32>> = HeadBin,
  PacketSize.

closed(Pid) when is_pid(Pid) ->
  Pid ! closed.

complete_packet(Pid, Bin) when is_pid(Pid) andalso is_binary(Bin) ->
  gen_fsm:send_all_state_event(Pid, {complete_packet, Bin}).

timer_callback(_Ref, Pid) ->
  Pid ! timeout_check.




%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------


-spec start(UserSup :: atom(), SockModule :: module(), Socket :: tcp_socket:socket()) ->
  {ok, pid()}.
start(UserSup, SockModule, SocketData) ->
  supervisor:start_child(UserSup, [SockModule, SocketData]).

start_link(SockModule, Socket) ->
  gen_fsm:start_link(?MODULE, [SockModule, Socket], []).

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
init([SockModule, SocketData]) ->
  ReceiveMonitor = SockModule:monitor(SocketData),
  case catch SockModule:peername(SocketData) of
    {ok, _} ->
      CurrentTime = qp_util:timestamp(),
      timer_manager:addDelayTask(
        CurrentTime,
        CurrentTime + ?TIMER_SPACE,
        qp_user, timer_callback, [self()]),
      {ok,
        wait_login,
        #state{
          receiveMonitor = ReceiveMonitor,
          sockModule = SockModule,
          sockData = SocketData,
          last_recv_packet_time = CurrentTime
        }};
    Other ->
      ?FILE_LOG_ERROR("socket init peername fail reason=[~p]", [Other]),
      {stop, normal}
  end.


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
-spec(wait_login(Event :: term(), State :: #state{}) ->
  {next_state, NextStateName :: atom(), NextState :: #state{}} |
  {next_state, NextStateName :: atom(), NextState :: #state{},
    timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
wait_login(Event, State) ->
  ?FILE_LOG_WARNING("~p", [Event]),
  {next_state, wait_login, State}.


hall(Event, State) ->
  ?FILE_LOG_WARNING("~p", [Event]),
  {next_state, hall, State}.


room({room_bin_msg, Bin}, State) ->
  send_bin(Bin, State),
  {next_state, room, State};
room(Event, State) ->
  ?FILE_LOG_WARNING("~p", [Event]),
  {next_state, room, State}.


game({room_bin_msg, Bin}, State) ->
  send_bin(Bin, State),
  {next_state, game, State};
game(Event, State) ->
  ?FILE_LOG_WARNING("~p", [Event]),
  {next_state, game, State}.

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
%%-spec(state_name(Event :: term(), From :: {pid(), term()},
%%                 State :: #state{}) ->
%%                    {next_state, NextStateName :: atom(), NextState :: #state{}} |
%%                    {next_state, NextStateName :: atom(), NextState :: #state{},
%%                     timeout() | hibernate} |
%%                    {reply, Reply, NextStateName :: atom(), NextState :: #state{}} |
%%                    {reply, Reply, NextStateName :: atom(), NextState :: #state{},
%%                     timeout() | hibernate} |
%%                    {stop, Reason :: normal | term(), NewState :: #state{}} |
%%                    {stop, Reason :: normal | term(), Reply :: term(),
%%                     NewState :: #state{}}).
%%state_name(_Event, _From, State) ->
%%    Reply = ok,
%%    {reply, Reply, state_name, State}.

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
handle_event({complete_packet, Bin}, StateName, #state{last_recv_packet_time = OldLastRecvPacketTime} = State) ->
  try
    Request = qp_proto:decode_qp_packet(Bin),
    {NewStateName, NewState, IsUpdate} = packet_handle(Request, StateName, State),
    true = is_record(NewState, state),
    true = (IsUpdate =:= true orelse IsUpdate =:= false),
    state_name_check(NewStateName),

    NewLastRecvPacketTime =
      if
        IsUpdate =:= true -> qp_util:timestamp();
        true -> OldLastRecvPacketTime
      end,
    {next_state, NewStateName, NewState#state{last_recv_packet_time = NewLastRecvPacketTime}}
  catch
    What:Type ->
      ?FILE_LOG_ERROR("~p, ~p, ~p", [What, Type, erlang:get_stacktrace()]),
      {stop, normal, State}
  end;
handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.


state_name_check(wait_login) -> ok;
state_name_check(hall) -> ok;
state_name_check(room) -> ok;
state_name_check(game) -> ok.

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
handle_info(closed, _StateName, State) ->
  ?FILE_LOG_DEBUG("qp_user socket close", []),
  {stop, normal, State};
handle_info(timeout_check, StateName, #state{last_recv_packet_time = LastRecvPackTime} = State) ->
  CurrentTime = qp_util:timestamp(),
  SpacheTime = CurrentTime - LastRecvPackTime,
  if
    SpacheTime > ?RECV_TIMEOUT ->
      {stop,normal,State};
    true ->
      timer_manager:addDelayTask(
        CurrentTime,
        CurrentTime + ?TIMER_SPACE,
        qp_user, timer_callback, [self()]),
      {next_state, StateName, State}
  end;
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
terminate(_Reason, _StateName, #state{user_data = UserData, room_data = RoomData} = State) ->
  ?FILE_LOG_DEBUG("qp_user terminate", []),
  if
    UserData =/= undefined ->
      ?FILE_LOG_DEBUG("user_id[~p] [~p] terminate.", [UserData#user_data.user_id, _StateName]);
    true -> ok
  end,
  (State#state.sockModule):close(State#state.sockData),
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

send_packet(Packet, State) when is_record(State, state) ->
  RspBin = qp_proto:encode_qp_packet(Packet),
  send_bin(RspBin, State).
send_bin(Bin, State) when is_binary(Bin) andalso is_record(State, state)->
  (State#state.sockModule):send(State#state.sockData, Bin).

packet_handle(#qp_login_req{account = Account}, wait_login, #state{user_data = undefined, room_data = undefined} = State) ->
  ?FILE_LOG_DEBUG("acc=~p qp_login_req", [Account]),
  {success, {UserId, Gold, NickName, AvatarUrl}} = qp_db:load_user_data_by_acc(Account),
  ProtoUserData = #qp_user_data{user_id = UserId, gold = Gold, avatar_url = AvatarUrl, nick_name = NickName},
  ?FILE_LOG_DEBUG("login_success, acc=~p user_id=~p", [Account, UserId]),
  Rsp = #qp_login_rsp{state = 0, data = ProtoUserData},
  send_packet(Rsp, State),
  StateUserData = #user_data{user_id = UserId, gold = Gold, nickname = NickName, avatar_url = AvatarUrl},
  {hall, State#state{user_data = StateUserData}, true};
packet_handle(Request, wait_login, State) ->
  ?FILE_LOG_WARNING("wait_login request=~p", [Request]),
  {wait_login, State, false};


packet_handle(#qp_create_room_req{room_type = RoomType}, hall, #state{user_data = UserData, room_data = undefined} = State) ->
  #user_data{user_id = UserId, gold = Gold, nickname = NickName, avatar_url = AvatarUrl} = UserData,
  ?FILE_LOG_DEBUG("user_id[~p] [hall] qp_create_room_req", [UserId]),
  case qp_room_manager:create_room(UserId, RoomType) of
    {success, {RoomId, RoomPid}} ->
      ?FILE_LOG_DEBUG("user_id[~p] [hall] create_room success, room_id=~p, room_pid=~p", [UserId, RoomId, RoomPid]),
      case qp_room:join(RoomPid, qp_user_data:new(UserId, self(), Gold, NickName, AvatarUrl)) of
        {success, {SeatNum, false, []}} ->
          Rsp = #qp_create_room_rsp{state = 0, room_id = RoomId, seat_id = SeatNum},
          send_packet(Rsp, State),
          RoomData = #room_data{room_id = RoomId, seat_num = SeatNum, room_pid = RoomPid},
          {room, State#state{room_data = RoomData}, true};
        failed ->
          %%进入失败
          ?FILE_LOG_DEBUG("user_id=~p [hall] create_room success, join failed", [UserId]),
          %%这个时候让房间退出
          qp_room:dismiss(RoomPid),
          send_packet(#qp_create_room_rsp{state = -2}, State),
          {hall, State, true}
      end;
    failed ->
      %%创建房间失败
      ?FILE_LOG_WARNING("user_id[~p] [hall] create_room failed", [UserId]),
      send_packet(#qp_create_room_rsp{state = -1}, State),
      {hall, State, true}
  end;
packet_handle(#qp_join_room_req{room_id = RoomId}, hall, #state{user_data = UserData, room_data = undefined} = State) ->
  #user_data{user_id = UserId, gold = Gold, nickname = NickName, avatar_url = AvatarUrl} = UserData,
  ?FILE_LOG_DEBUG("user_id[~p] [hall] qp_join_room_req", [UserId]),
  case qp_room_manager:get_room_pid(RoomId) of
    failed ->
      ?FILE_LOG_WARNING("user_id[~p] [hall] join_room[~p] failed.", [UserId, RoomId]),
      {hall, State, true};
    {success, RoomPid} ->
      case qp_room:join(RoomPid, qp_user_data:new(UserId, self(), Gold, NickName, AvatarUrl)) of
        {success, {SeatNum, false, RoomUsers}} ->
          PbRoomUsers=
            lists:map(
              fun(RoomUser) ->
                {RoomUserData, RoomUserSeatNum, IsReady} = RoomUser,
                #qp_room_user{
                  user_data =
                  #qp_user_data{
                    user_id = RoomUserData:get(user_id),
                    gold = RoomUserData:get(gold),
                    avatar_url = RoomUserData:get(avatar_url),
                    nick_name = RoomUserData:get(nick_name)},
                  seat_number = RoomUserSeatNum,
                  is_ready = IsReady
                }
              end, RoomUsers),
          Rsp = #qp_join_room_rsp{result = 0, seat_number = SeatNum, is_ready = false, room_user = PbRoomUsers},
          send_packet(Rsp, State),
          RoomData = #room_data{room_id = RoomId, seat_num = SeatNum, room_pid = RoomPid},
          ?FILE_LOG_WARNING("user_id[~p] [hall] join_room[~p] success, seat_num=~p.", [UserId, RoomId, SeatNum]),
          {room, State#state{room_data = RoomData}, true};
        failed ->
          %%进入失败
          ?FILE_LOG_DEBUG("user_id[~p] [hall] get_room_pid[~p] failed", [UserId, RoomId]),
          send_packet(#qp_join_room_rsp{result = -1}, State),
          {hall, State, true}
      end
  end;
packet_handle(#qp_ping_req{seat_number = SeatNumber}, hall, #state{room_data = undefined} = State) ->
  send_packet(#qp_ping_rsp{seat_number = SeatNumber}, State),
  {hall, State, true};
packet_handle(Request, hall, #state{user_data = UserData} = State) ->
  #user_data{user_id = UserId} = UserData,
  ?FILE_LOG_DEBUG("user_id[~p] [hall] request=~p", [UserId, Request]),
  {hall, State, false};


packet_handle(#qp_ready_req{ready_state = ReadyState}=Request, room, #state{room_data = RoomData, user_data = UserData} = State) ->
  #room_data{room_id = _, seat_num = SeatNum, room_pid = RoomPid} = RoomData,
  #user_data{user_id = UserId} = UserData,
  ?FILE_LOG_DEBUG("user_id[~p] [room] qp_ready_req", [UserId]),
  Rsp =
    case qp_room:ready(RoomPid, qp_user_key:new(UserId, self()), SeatNum, ReadyState) of
      {success, ReadyState} ->
        %%成功了
        ?FILE_LOG_DEBUG("user_id[~p] [room] ready success readyStae=~p", [UserId, ReadyState]),
        #qp_ready_rsp{state = 0, ready_state = ReadyState};
      failed ->
        %%失败了
        ?FILE_LOG_DEBUG("user_id[~p] [room] ready failed", [UserId]),
        #qp_ready_rsp{state = -1}
    end,
  send_packet(Rsp, State),
  {room, State, true};
packet_handle(#qp_ping_req{seat_number = SeatNumber}, room, State) ->
  send_packet(#qp_ping_rsp{seat_number = SeatNumber}, State),
  {room, State, true};
packet_handle(#qp_exit_room_req{} = Request, room, State) ->
  ?FILE_LOG_DEBUG("room request=~p", [Request]),

  {hall, State, true};
packet_handle(Request, room, State) ->
  ?FILE_LOG_DEBUG("room request=~p", [Request]),
  {room, State, false};


packet_handle(#qp_game_data{}=Request, game, State) ->
  ?FILE_LOG_DEBUG("game request=~p", [Request]),
  {game, State, true};
packet_handle(#qp_ping_req{seat_number = SeatNumber}, game, State) ->
  send_packet(#qp_ping_rsp{seat_number = SeatNumber}, State),
  {game, State, true};
packet_handle(#qp_exit_room_req{} = Request, game, State) ->
  ?FILE_LOG_DEBUG("game request=~p", [Request]),
  {hall, State, true};
packet_handle(Request, game, State) ->
  ?FILE_LOG_DEBUG("game request=~p", [Request]),
  {game, State, false}.




