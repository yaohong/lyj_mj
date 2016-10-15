-module(test).


-export([init/0]).
-export([game_start/1]).
init() ->
	erlang:load_nif("./mj_nif", 0).


game_start(BankerNumber) ->
	exception.