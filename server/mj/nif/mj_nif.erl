-module(mj_nif).


-export([init/0]).
-export([game_start/1, test_func/0]).
init() ->
	erlang:load_nif("./mj_nif", 0).


game_start(BankerNumber) ->
	exception.

test_func() ->
	exception.