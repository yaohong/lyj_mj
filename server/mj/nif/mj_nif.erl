-module(mj_nif).


-export([init/0]).
-export([game_start/3, test_func/0]).
init() ->
	erlang:load_nif("./mj_nif", 0).


game_start(GameType, BankerNumber, RandSeed) ->
	exception.

test_func() ->
	exception.