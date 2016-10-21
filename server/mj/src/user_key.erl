%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 十月 2016 18:47
%%%-------------------------------------------------------------------
-module(user_key).
-author("yaohong").

%% API
-export([new/2,
         get/2]).

new(UserId, UserPid) when is_integer(UserId) andalso is_pid(UserPid) ->
    {?MODULE, [UserId, UserPid]}.


get(user_id, {?MODULE, [UserId, _UserPid]}) -> UserId;
get(user_pid, {?MODULE, [_UserId, UserPid]}) -> UserPid.