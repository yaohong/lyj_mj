-module(mj_nif).

-export([init/0]).
-export([game_start/3, game_oper/2]).
init() ->
	erlang:load_nif("./mj_nif", 0).

-define(HH_POOL_COUNT, 120).


game_start(GameType, BankerNumber, RandSeed) when is_integer(GameType) andalso is_integer(BankerNumber) andalso is_integer(RandSeed) ->
	exception.




game_oper(V1, V2) ->
	exception.
