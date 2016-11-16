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
	SendSeatData = encode_seat_data(Logic#hh_main_logic.banker_seat_number, [{0, Seat0},{1, Seat1},{2, Seat2},{3, Seat3}]),
	{success, {GameBin, {<<>>, SendSeatData, <<>>}}}.

encode_seat_data(BankerSeatNum, SeatList) ->
	encode_seat_data(BankerSeatNum, SeatList, []).

encode_seat_data(_, [], Out) -> Out;
encode_seat_data(BankerSeatNum, [{SeatNum, SeatData}|T], Out) ->
	N1 = #qp_mj_game_start_notify{pai = SeatData#hh_seat.pai, banker_seat_number = BankerSeatNum},
	Rsp =
		if
			BankerSeatNum =:= SeatNum ->
				N1#qp_mj_game_start_notify{oper_flag = SeatData#hh_seat.oper_flag};
			true -> N1
		end,
	Bin = hh_mj_proto:encode_packet(Rsp),
	encode_seat_data(BankerSeatNum, T, [{SeatNum, Bin}|Out]).



game_oper(GameBin, SeatNum, OperBin) when is_binary(GameBin) andalso is_integer(SeatNum) andalso is_binary(OperBin) ->
	#qp_mj_oper_req{type = Type, v1 = V1, v2 = V2} = hh_mj_proto:decode_packet(OperBin),
	?FILE_LOG_DEBUG("game_data seat_num=~p, type=~p, v1=~p, v2=~p", [SeatNum, Type, V1, V2]),
	success = mj_nif:game_oper(GameBin, ?GAME_TYPE, SeatNum, Type ,undefine_transform(V1), undefine_transform(V2)),
	ok.

undefine_transform(undefined) -> 0.