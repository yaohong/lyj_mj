%%%---------------------------------------------avatar_url----------------------
%%% @author yaohong
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 十月 2016 17:44
%%%-------------------------------------------------------------------
-module(qp_db).
-author("yaohong").
-include("../deps/yhsql/include/yhsql.hrl").
-include("../deps/file_log/include/file_log.hrl").
%% API
-export([
    load_user_data_by_acc/1,
    load_user_data_by_acc/2]).

load_user_data_by_acc(Acc) ->
    load_user_data_by_acc(Acc, true).
load_user_data_by_acc(Acc, IsCreate) ->
    SelectSql = "select user_id, gold, nickname, avatar_url from account where `acc`='" ++ Acc ++ "'",
    case yhsql:fetch(sql:pool_name(), SelectSql) of
        {data, #yhsql_result{rows = []}} ->
            if
                IsCreate =:= true ->
                    InsertSql = yhsql_util:insert_query("account", ["acc"], [Acc]),
                    case yhsql:fetch(sql:pool_name(), InsertSql) of
                        {updated, #yhsql_result{affectedrows = 1, insertid = UserId}} -> {success, {UserId, 0, "", ""}};
                        Other1 ->
                            ?FILE_LOG_ERROR("~p", [Other1]),
                            failed
                    end;
                true -> failed
            end;
        {data, #yhsql_result{rows = [UserData]}} ->
            [UserId, Gold, NickName, AvatarUrl] = UserData,
            {success,
             {
                 qp_util:to_integer(UserId),
                 qp_util:to_integer(Gold),
                 qp_util:to_list(NickName),
                 qp_util:to_list(AvatarUrl)
             }};
        Other ->
            ?FILE_LOG_ERROR("~p", [Other]),
            failed
    end.