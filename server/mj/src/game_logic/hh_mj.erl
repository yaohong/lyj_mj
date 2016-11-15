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
-export([game_start/1]).
-define(GAME_TYPE, 0).

game_start([undefined, RandSeed]) ->
	{success, GameBin} = mj_nif:game_start(?GAME_TYPE, 0, RandSeed),
	game_start(GameBin);
game_start([#hh_main_logic{banker_seat_number = _OldBankerSeatNumber}, RandSeed]) ->
	{success, GameBin} = mj_nif:game_start(?GAME_TYPE, 0, RandSeed),
	game_start(GameBin);
game_start(GameBin) when is_binary(GameBin) ->
	Logic = hh_mj_util:generate_main_logic(GameBin),
	Seat0 = Logic#hh_main_logic.seat0,
	Seat1 = Logic#hh_main_logic.seat1,
	Seat2 = Logic#hh_main_logic.seat2,
	Seat3 = Logic#hh_main_logic.seat3,
	SendSeatData = encode_seat_data(Logic#hh_main_logic.banker_seat_number, [{0, Seat0},{1, Seat1},{2, Seat2},{3, Seat3}]),
	{success, {Logic, {<<>>, SendSeatData, <<>>}}}.

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