-module(mysql_util).
-author('erlangonrails@gmail.com').
-include("mysql.hrl").
-export([ensure_app_started/1,
         mysql_to_odbc/1,
         escape/1,
         escape_like/1,
         to_list/1,
         md5/1]).

-export([select_query/3,
         insert_query/3,
         insert_ignore_query/3,
         replace_query/3,
         delete_query/2,
         update_query/3,
         update_query/4]).
-export([update_query_nowhere/2, update_query_nowhere/3]).

-spec ensure_app_started(App :: atom()) -> 
    ok.
ensure_app_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

mysql_to_odbc({updated, MySQLRes}) when is_record(MySQLRes, mysql_result) ->
    {updated, mysql:get_result_affected_rows(MySQLRes)};
mysql_to_odbc({data, MySQLRes}) when is_record(MySQLRes, mysql_result) ->
    mysql_item_to_odbc(mysql:get_result_field_info(MySQLRes),
                       mysql:get_result_rows(MySQLRes));
mysql_to_odbc({error, MySQLRes}) ->
    mysql:get_result_reason(MySQLRes).

mysql_item_to_odbc(Columns, Recs) ->
    {selected,
     [element(2, Column) || Column <- Columns],
     [list_to_tuple(Rec) || Rec <- Recs]}.

%% 例子:
%% > Query1 = mysql_util:select_query("tbl_user", ["username", "password"], "").
%% ["SELECT ","username, password"," FROM ","tbl_user",";"]
%% > iolist_to_binary(Query1).
%% <<"SELECT username, password FROM tbl_user;">>
%%
%% > Query2 = mysql_util:select_query("tbl_user", ["username", "password"], "username = 'user1'"). 
%% ["SELECT ","username, password"," FROM ","tbl_user",
%%  " WHERE ","username = 'user1'",";"]
%% > iolist_to_binary(Query2).                                                                    
%% <<"SELECT username, password FROM tbl_user WHERE username = 'user1';">>
-spec select_query(
        Table :: string() | binary(),
        Fields :: [string() | binary()],
        Where :: iolist()) ->
    iodata().
select_query(Table, Fields, "") ->
    ["SELECT ", string:join([to_list(TI) || TI <- Fields], ", "),
     " FROM ", Table, ";"];
select_query(Table, Fields, <<>>) ->
    ["SELECT ", string:join([to_list(TI) || TI <- Fields], ", "),
     " FROM ", Table, ";"];
select_query(Table, Fields, Where) ->
    ["SELECT ", string:join([to_list(TI) || TI <- Fields], ", "),
     " FROM ", Table, " WHERE ", Where, ";"].

%% 例子:
%% > Query = mysql_util:insert_query("tbl_user", ["username", "password"], ["user1", "pwd"]).
%% ["INSERT INTO ","tbl_user","(","username, password",
%%  ") VALUES ('","user1', 'pwd","');"]
%% > iolist_to_binary(Query).
%% <<"INSERT INTO tbl_user(username, password) VALUES ('user1', 'pwd');">>
-spec insert_query(
        Table :: string() | binary(),
        Fields :: [string() | binary()],
        Vals :: [string() | binary()]) ->
    iodata().
insert_query(Table, Fields, Vals) ->
    ["INSERT INTO ", Table, "(", string:join([to_list(TI) || TI <- Fields], ", "),
        ") VALUES ('", string:join([to_list(VI) || VI <- Vals], "', '"), "');"].


%% 例子:
%% > Query = mysql_util:replace_query("tbl_user", ["username", "password"], ["user1", "pwd"]).
%% ["REPLACE INTO ","tbl_user","(","username, password",
%%  ") VALUES ('","user1', 'pwd","');"]
%% > iolist_to_binary(Query).
%% <<"REPLACE INTO tbl_user(username, password) VALUES ('user1', 'pwd');">>
-spec replace_query(
    Table :: string() | binary(),
    Fields :: [string() | binary()],
    Vals :: [string() | binary()]) ->
    iodata().
replace_query(Table, Fields, Vals) ->
    ["REPLACE INTO ", Table, "(", string:join([to_list(TI) || TI <- Fields], ", "),
        ") VALUES ('", string:join([to_list(VI) || VI <- Vals], "', '"), "');"].

%% 例子:
%% > Query = mysql_util:insert_ignore_query("tbl_user", ["username", "password"], ["user1", "pwd"]).
%% ["INSERT IGNORE INTO ","tbl_user","(","username, password",
%%  ") VALUES ('","user1', 'pwd","');"]
%% > iolist_to_binary(Query).
%% <<"INSERT IGNORE INTO tbl_user(username, password) VALUES ('user1', 'pwd');">>
-spec insert_ignore_query(
    Table :: string() | binary(),
    Fields :: [string() | binary()],
    Vals :: [string() | binary()]) ->
    iodata().
insert_ignore_query(Table, Fields, Vals) ->
    ["INSERT IGNORE INTO ", Table, "(", string:join([to_list(TI) || TI <- Fields], ", "),
        ") VALUES ('", string:join([to_list(VI) || VI <- Vals], "', '"), "');"].

%% 例子:
%% > Query1 = mysql_util:delete_query("tbl_user", "").                  
%% ["DELETE FROM ","tbl_user",";"]
%% > iolist_to_binary(Query1).
%% <<"DELETE FROM tbl_user;">>
%%
%% > Query2 = mysql_util:delete_query("tbl_user", "username = 'user1'").
%% ["DELETE FROM ","tbl_user"," WHERE ","username = 'user1'", ";"]
%% > iolist_to_binary(Query2).
%% <<"DELETE FROM tbl_user WHERE username = 'user1';">>
-spec delete_query(
        Table :: string() | binary(),
        Where :: iolist()) ->
    iodata().
delete_query(Table, "") ->
    ["DELETE FROM ", Table, ";"];
delete_query(Table, <<>>) ->
    ["DELETE FROM ", Table, ";"];
delete_query(Table, Where) ->
    ["DELETE FROM ", Table, " WHERE ", Where, ";"].

%% 例子:
%% > Query = mysql_util:update_query("tbl_user", [{"password", "newpwd"}], "username = 'user1'").
%% ["UPDATE ","tbl_user"," SET ","password='newpwd'"," WHERE ",
%%  "username = 'user1'",";"]
%% 
%% > Query = mysql_util:update_query("tbl_user", ["password"], ["newpwd"], "username = 'user1'").
%% ["UPDATE ","tbl_user"," SET ","password='newpwd'"," WHERE ",
%%  "username = 'user1'",";"]
%% > iolist_to_binary(Query).
%% <<"UPDATE tbl_user SET password='newpwd' WHERE username = 'user1';">>
-spec update_query(
        Table :: string() | binary(), 
        KVs :: [{K :: string() | binary(), V :: string() | binary()}], 
        Where :: iolist()) ->
    iodata().
update_query(Table, KVs, Where) ->
    {Fields, Vals} = lists:unzip(KVs),
    update_query(Table, Fields, Vals, Where).

%% 参考update_query/3
-spec update_query(
        Table :: string() | binary(), 
        Fields :: [string() | binary()], 
        Vals :: [string() | binary()], 
        Where :: iolist()) ->
    iodata().
update_query(Table, Fields, Vals, Where) ->
    UPairs = lists:zipwith(fun(A, B) -> to_list(A) ++ "='" ++ to_list(B) ++ "'" end,
                           Fields, Vals),
    ["UPDATE ", Table, " SET ", string:join(UPairs, ", "),
     " WHERE ", Where, ";"].

update_query_nowhere(Table, KVs) ->
	{Fields, Vals} = lists:unzip(KVs),
	update_query_nowhere(Table, Fields, Vals).
update_query_nowhere(Table, Fields, Vals) ->
	UPairs = lists:zipwith(fun(A, B) -> to_list(A) ++ "='" ++ to_list(B) ++ "'" end,
		Fields, Vals),
	["UPDATE ", Table, " SET ", string:join(UPairs, ", "),";"].


-spec escape(string() | binary()) -> iolist().
escape(S) when is_list(S) ->
    [escape_internal(C) || C <- S];
escape(S) when is_binary(S) ->
    escape(binary_to_list(S)).

%% 相比escape/1, 多处理$%, %_这两个字符
-spec escape_like(string() | binary()) -> iolist().
escape_like(S) when is_list(S) ->
    [escape_like(C) || C <- S];
escape_like(S) when is_binary(S) ->
    escape_like(binary_to_list(S));
escape_like($%) -> "\\%";
escape_like($_) -> "\\_";
escape_like(C)  -> escape_internal(C).

%% 下列字符受影响：
%% \x00
%% \n
%% \r
%% \
%% '
%% "
%% \x1a
-spec escape_internal(char()) -> iolist().
escape_internal(0)   -> [$\\, 0];
escape_internal($\n) -> [$\\, $\n];
escape_internal($\r) -> [$\\, $\r];
escape_internal($\\) -> [$\\, $\\];
escape_internal(39)  -> [$\\, 39]; %% $'
escape_internal(34)  -> [$\\, 34]; %% $"
escape_internal(26)  -> [$\\, $Z];
escape_internal(C)   -> C.


to_list(A) when is_atom(A) ->
    atom_to_list(A);
to_list(A) when is_integer(A) ->
    integer_to_list(A);
to_list(A) when is_binary(A) ->
    binary_to_list(A);
to_list(A) when is_list(A) ->
    A.

-spec md5(IoData :: iodata()) ->
    string().
md5(IoData) ->
    relax_hex:to_hex(erlang:md5(IoData)).
