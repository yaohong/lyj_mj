%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 十一月 2016 16:24
%%%-------------------------------------------------------------------
-module(hh_mj).
-author("yaohong").
-include("hh_mj.hrl").
-include("../../include/mj_pb.hrl").
-include("../../deps/file_log/include/file_log.hrl").
-export([test_game_start/0, test_game_oper/4]).
%% API
-export([
		 init_private_data/0,
		 game_start/1,
		 game_oper/3]).
-define(GAME_TYPE, 0).

init_private_data() ->
	undefined.

game_start(undefined) ->
	{success, GameBin} = mj_nif:game_start(?GAME_TYPE, 0, qp_util:timestamp()),
	game_start1(GameBin);
game_start(OldGameBin) when is_binary(OldGameBin) ->
	Logic = hh_mj_util:generate_main_logic(OldGameBin),
	_OldBankerSeatNumber = Logic#hh_main_logic.banker_seat_number,
	{success, GameBin} = mj_nif:game_start(?GAME_TYPE, 0, qp_util:timestamp()),
	game_start1(GameBin).

game_start1(GameBin) when is_binary(GameBin) ->
	Logic = hh_mj_util:generate_main_logic(GameBin),
	Seat0 = Logic#hh_main_logic.seat0,
	Seat1 = Logic#hh_main_logic.seat1,
	Seat2 = Logic#hh_main_logic.seat2,
	Seat3 = Logic#hh_main_logic.seat3,
	SendSeatData = encode_seat_data(Logic, [{0, Seat0},{1, Seat1},{2, Seat2},{3, Seat3}]),
	{success, {GameBin, {<<>>, SendSeatData, <<>>}}}.

encode_seat_data(BankerSeatNum, SeatList) ->
	encode_seat_data(BankerSeatNum, SeatList, []).

encode_seat_data(_, [], Out) -> Out;
encode_seat_data(Logic, [{SeatNum, SeatData}|T], Out) ->
	Rsp =
		if
			Logic#hh_main_logic.banker_seat_number =:= SeatNum ->
				#qp_mj_game_start_notify{
					pai = SeatData#hh_seat.pai,
					banker_seat_number = Logic#hh_main_logic.banker_seat_number,
					oper_flag = Logic#hh_main_logic.next#hh_next_oper.flag};
			true ->
				#qp_mj_game_start_notify{
					pai = SeatData#hh_seat.pai,
					banker_seat_number = Logic#hh_main_logic.banker_seat_number}
		end,

	Bin = hh_mj_proto:encode_packet(Rsp),
	encode_seat_data(Logic, T, [{SeatNum, Bin}|Out]).



game_oper(GameBin, SeatNum, OperBin) when is_binary(GameBin) andalso is_integer(SeatNum) andalso is_binary(OperBin) ->
	#qp_mj_oper_req{type = Type, v1 = V1, v2 = V2} = hh_mj_proto:decode_packet(OperBin),
	?FILE_LOG_DEBUG("game_data seat_num=~p, type=~p, v1=~p, v2=~p", [SeatNum, Type, V1, V2]),
	{success} = mj_nif:game_oper(GameBin, ?GAME_TYPE, SeatNum, Type ,undefine_transform(V1), undefine_transform(V2)),
	Logic = hh_mj_util:generate_main_logic(GameBin),
	Old = Logic#hh_main_logic.old,
	Next = Logic#hh_main_logic.next,
	Notify1 =
		#qp_mj_oper_notify{
			seat_numer = Old#hh_old_oper.seat_number,
			type = Old#hh_old_oper.type,
			v1 = Old#hh_old_oper.value1,
			v2 = Old#hh_old_oper.value2
		},
	SendSeatData = encode_oper_seat_data(Next, Notify1, [0,1,2,3], []),
	{success, {<<>>, SendSeatData, <<>>}}.

encode_oper_seat_data(_Next, _Notify, [], Out) -> Out;
encode_oper_seat_data(Next, Notify, [SeatNum|T], Out) when Next#hh_next_oper.seat_number =:= SeatNum ->
	NewNotify = Notify#qp_mj_oper_notify{
		next_oper_seat_num = Next#hh_next_oper.seat_number,
		next_oper_flag = Next#hh_next_oper.flag,
		next_oper_value1 = Next#hh_next_oper.value1,
		next_oper_value2 = Next#hh_next_oper.value2},
	Bin = hh_mj_proto:encode_packet(NewNotify),
	encode_oper_seat_data(Next, Notify, T, [{SeatNum, Bin}|Out]);
encode_oper_seat_data(Next, Notify, [SeatNum|T], Out) ->
	Bin = hh_mj_proto:encode_packet(Notify),
	encode_oper_seat_data(Next, Notify, T, [{SeatNum, Bin}|Out]).


undefine_transform(undefined) -> 0;
undefine_transform(V) -> V.


test_game_start() ->
	{success, GameBin} = mj_nif:game_start(?GAME_TYPE, 0, qp_util:timestamp()),
	Logic = hh_mj_util:generate_main_logic(GameBin),
	?FILE_LOG_DEBUG("logic=~p", [Logic]),
	hh_mj_util:print(Logic),
	put(game_bin, GameBin).


test_game_oper(SeatNumber, Type, V1, V2) ->
    GameBin = get(game_bin),
	true = GameBin =/= undefined,
    mj_nif:game_oper(GameBin, ?GAME_TYPE, SeatNumber, Type, V1, V2),
	Logic = hh_mj_util:generate_main_logic(GameBin),
	hh_mj_util:print(Logic).
