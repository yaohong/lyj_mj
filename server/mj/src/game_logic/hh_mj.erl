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
-include("hh_mj.hrl")
%% API
-export([game_start/1]).
-define(GAME_TYPE, 0).

game_start([undefined, RandSeed]) ->
	{success, GameBin} = mj_nif:game_start(?GAME_TYPE, -1, RandSeed);
game_start([#hh_main_logic{banker_seat_number = OldBankerSeatNumber}, RandSeed]) ->
	{success, GameBin} = mj_nif:game_start(?GAME_TYPE, OldBankerSeatNumber, RandSeed).

game_start(GameBin) ->
	Logic = hh_mj_util:generate_main_logic(GameBin),
	{success, {Logic, {<<>>, [], <<>>}}}.

