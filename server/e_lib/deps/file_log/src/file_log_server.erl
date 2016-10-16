%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. 五月 2015 20:20
%%%-------------------------------------------------------------------
-module(file_log_server).
-author("yaohong").

%% API
-export([get/0, set/1]).
-export([send/6]).

get() ->
	%%
	log_filter:cutoff_level().


set(Level) when is_atom(Level) ->
	log_filter_codegen:set_cutoff_level(Level),
	log4erl:change_log_level(Level).

send(Level, Pid, Mod, Line, Format, Arguments) ->
	if
		Level =:= debug ->
			log4erl:debug({{Pid, Mod, Line}, Format}, Arguments);
		Level =:= info ->
			log4erl:info({{Pid, Mod, Line}, Format}, Arguments);
		Level =:= warn ->
			log4erl:warn({{Pid, Mod, Line}, Format}, Arguments);
		Level =:= error ->
			log4erl:error({{Pid, Mod, Line}, Format}, Arguments)
	end.
