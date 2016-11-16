-module(mj_nif).

-export([init/0]).
-export([game_start/3, game_oper/6]).
init() ->
	erlang:load_nif("./mj_nif", 0).

-define(HH_POOL_COUNT, 120).


game_start(GameType, BankerNumber, RandSeed) when is_integer(GameType) andalso is_integer(BankerNumber) andalso is_integer(RandSeed) ->
	exception.



game_oper(GameBin, GameType, OperSeatNum, OperType, V1, V2)
	when is_binary(GameBin) andalso is_integer(GameType) andalso is_integer(OperSeatNum) andalso is_integer(OperType) andalso is_integer(V1) andalso is_integer(V2) ->
	exception.
