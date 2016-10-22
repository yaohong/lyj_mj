%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 十月 2016 18:47
%%%-------------------------------------------------------------------
-module(qp_user_data).
-author("yaohong").

%% API
-export([new/5,
         get/2]).

new(UserId, UserPid, Gold, Nickname, AvatarUrl) when is_integer(UserId) andalso is_pid(UserPid) ->
    {?MODULE, [UserId, UserPid, Gold, Nickname, AvatarUrl]}.


get(user_id, {?MODULE, [UserId, _UserPid, _Gold, _Nickname, _AvatarUrl]}) -> UserId;
get(user_pid, {?MODULE, [_UserId, UserPid, _Gold, _Nickname, _AvatarUrl]}) -> UserPid;
get(gold, {?MODULE, [_UserId, _UserPid, Gold, _Nickname, _AvatarUrl]}) -> Gold;
get(nick_name, {?MODULE, [_UserId, _UserPid, _Gold, Nickname, _AvatarUrl]}) -> Nickname;
get(avatar_url, {?MODULE, [_UserId, _UserPid, _Gold, _Nickname, AvatarUrl]}) -> AvatarUrl.