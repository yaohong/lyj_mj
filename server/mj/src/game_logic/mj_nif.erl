-module(mj_nif).

-export([init/0]).
-export([game_start/3, game_oper/6]).

-on_load(init/0).

init() ->
	case erlang:load_nif("./nif/mj_nif", 0) of
		ok -> ok;
		{error, {reload, _}} -> ok
	end.

-define(HH_POOL_COUNT, 120).


game_start(GameType, BankerNumber, RandSeed) when is_integer(GameType) andalso is_integer(BankerNumber) andalso is_integer(RandSeed) ->
	exception.




game_oper(GameBin, GameType, OperSeatNumber, OperType, OperValue1, OperValue2) ->
	exception.
