%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 十月 2016 7:25
%%%-------------------------------------------------------------------
-module(qp_user_key).
-author("yaohong").

%% API
-export([]).
-export([
	new/2,
	compare/2,
	get/2
]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
new(UserId, UserPid) when is_integer(UserId) andalso is_pid(UserPid) ->
	{?MODULE, [UserId, UserPid]}.

compare(UserData, {?MODULE, [UserId, UserPid]}) ->
	case {UserData:get(user_id) =:= UserId, UserData:get(user_pid) =:= UserPid} of
		{true, true} -> true;
		_ -> false
	end.

get(user_id, {?MODULE, [UserId, _UserPid]}) -> UserId;
get(user_pid, {?MODULE, [_UserId, UserPid]}) -> UserPid.