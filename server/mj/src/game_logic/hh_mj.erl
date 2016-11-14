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
	{success, GameBin} = mj_nif:game_start(?GAME_TYPE, -1, RandSeed),
	game_start(GameBin);
game_start([#hh_main_logic{banker_seat_number = OldBankerSeatNumber}, RandSeed]) ->
	{success, GameBin} = mj_nif:game_start(?GAME_TYPE, OldBankerSeatNumber, RandSeed),
	game_start(GameBin);
game_start(GameBin) when is_binary(GameBin) ->
	Logic = hh_mj_util:generate_main_logic(GameBin),
	Seat0 = Logic#hh_main_logic.seat0,
	Seat1 = Logic#hh_main_logic.seat1,
	Seat2 = Logic#hh_main_logic.seat2,
	Seat3 = Logic#hh_main_logic.seat3,
	Seat0Bin = hh_mj_proto:encode_packet(#qp_mj_game_start_notify{pai = Seat0#hh_seat.pai, banker_seat_number = Logic#hh_main_logic.banker_seat_number}),
	Seat1Bin = hh_mj_proto:encode_packet(#qp_mj_game_start_notify{pai = Seat1#hh_seat.pai, banker_seat_number = Logic#hh_main_logic.banker_seat_number}),
	Seat2Bin = hh_mj_proto:encode_packet(#qp_mj_game_start_notify{pai = Seat2#hh_seat.pai, banker_seat_number = Logic#hh_main_logic.banker_seat_number}),
	Seat3Bin = hh_mj_proto:encode_packet(#qp_mj_game_start_notify{pai = Seat3#hh_seat.pai, banker_seat_number = Logic#hh_main_logic.banker_seat_number}),
	?FILE_LOG_DEBUG("~p", [Logic]),
	{success, {Logic, {<<>>, [{0, Seat0Bin}, {1, Seat1Bin}, {2, Seat2Bin}, {3, Seat3Bin}], <<>>}}}.

