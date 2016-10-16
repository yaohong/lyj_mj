-module(relax_header).
-author('erlangonrails@gmail.com').
-export([empty/0, 
         from_binary/1,

         from_list/1,          %% insert构造方式   - merge   (所有的value集合)
         enter_from_list/2,    %% enter构造方式    - replace (覆盖老的value)
         default_from_list/2,  %% default构造方式  - default (保留老的value)

         insert/3, 
         enter/3, 
         default/3,

         get_primary_value/2,
         get_value/2,          %% 对set-cookie的格式做了统一处理
         lookup/2,             %% 对set-cookie的格式做了统一处理
         delete_any/2,

         to_list/1,            %% 注意: 返回值对多个set-cookie的情况做了特殊处理
                               %%       只有这一个API对set-cookie做了特殊处理
         make/1]).


%% HTTP Protocol Header
%% 每一个HTTP Header Filed的结构都是:
%% Key + ":" + (0-n个)空格 + Val + CRLF
%% 其中Key是大小写无关的.
%%
%% 后面一个只包含CRLF的行表示所有HTTP Header的结束.
%%
%%
%% 针对Set-Cookie的特殊处理:
%% a. 对于lookup/2和get_value/2, 返回的结果是统一处理的.
%% b. 对于to_list/1, 返回的结果做了特殊处理.
%%
%%
%% <1> H1 = relax_header:make([{"set-cookie", v1}, {"set-cookie", v2}, {"set-cookie", v3}]).
%% <2> H2 = relax_header:make([{"k", v1}, {"k", v2}, {"k", v3}]).   
%% <3> relax_header:to_list(H1).                                 
%%     [{"set-cookie","v1"},
%%      {"set-cookie","v2"},
%%      {"set-cookie","v3"}]
%% <4> relax_header:to_list(H2).
%%     [{"k","v1, v2, v3"}]
%% <5> relax_header:lookup("set-cookie", H1).
%%     {value,{"set-cookie","v1, v2, v3"}}
%% <6> relax_header:lookup("k", H2).         
%%     {value,{"k","v1, v2, v3"}}
%% <7> relax_header:get_value("set-cookie", H1).
%%     "v1, v2, v3"
%% <8> relax_header:get_value("k", H2).
%%     "v1, v2, v3"


-type http_headers() :: gb_trees:tree().
-type http_header_key() :: atom() | binary() | string().
-type http_header_value() :: atom() | binary() | string() | integer().


-spec empty() -> http_headers().
empty() ->
    gb_trees:empty().

%% 构造一个http_headers struct.
-spec make(
        A :: [{http_header_key(), http_header_value()}] |
             http_headers()) ->
    http_headers().
make(L) when is_list(L) ->
    from_list(L);
make(T) ->
    T.

%% 把一个二进制的HTTP Header转换成http_headers struct.
%% 可以处理下面两种数据格式的HTTP Header, 要注意, 传入的二进制数据
%% 必须是完整的HTTP Header.
%%
%% 1) 一个string或者binary类型的完整的HTTP Header, 并且以CRLF结尾.
%%    例如:
%%    ....
%%    "Content-Length: 47\r\nContent-Type: text/plain\r\n\r\n"
%%    <<"Content-Length: 47\r\nContent-Type: text/plain\r\n\r\n">>
%%    '''
%%
%% 2) 一个string或者binary组成的list, 每个元素表示一个原始的HTTP Header Line.
%%    例如:
%%    ....
%%    [<<"Content-Length: 47\r\n">>, <<"Content-Type: text/plain\r\n">>]
%%    ["Content-Length: 47\r\n", "Content-Type: text/plain\r\n"]
%%    ["Content-Length: 47\r\n", <<"Content-Type: text/plain\r\n">>]
%%    '''
-spec from_binary(A :: iolist()) -> http_headers().
from_binary(RawHttpHeader) when is_binary(RawHttpHeader) ->
    from_binary(RawHttpHeader, []);
from_binary(RawHttpHeaderList) ->
    from_binary(iolist_to_binary([RawHttpHeaderList, "\r\n"])). %% 针对2), 添加CRLF

from_binary(RawHttpHeader, Acc) ->
    case erlang:decode_packet(httph, RawHttpHeader, []) of
        {ok, {http_header, _, H, _, V}, Rest} ->
            from_binary(Rest, [{H, V} | Acc]);
        _ ->
            make(Acc)
    end.


%% 这种构造方式, 如果老的K存在, 新/老Value会做Merge, 都保存下来.
-spec from_list(
        L :: [{http_header_key(), http_header_value()}]) ->
    http_headers().
from_list(List) ->
    lists:foldl(fun ({K, V}, T) -> insert(K, V, T) end, empty(), List).

%% 增量构造
%% 这种构造方式, 如果K存在, 老的Value会被替换.   
-spec enter_from_list(
        L :: [{http_header_key(), http_header_value()}],
        T :: http_headers()) ->
    http_headers().
enter_from_list(List, T) ->
    lists:foldl(fun ({K, V}, T1) -> enter(K, V, T1) end, T, List).

%% 增量构造
%% 这种构造方式, 如果K存在, 会保留老的Value.
-spec default_from_list(
        L :: [{http_header_key(), http_header_value()}],
        T :: http_headers()) ->
    http_headers().
default_from_list(List, T) ->
    lists:foldl(fun ({K, V}, T1) -> default(K, V, T1) end, T, List).

%% 把一个http_headers()结构转换成K/V list.
%% 
%% 注意:
%% 针对K = "set-cookie"做了特殊处理:
%% i. 对于其它的K, 如果调用insert/3插入多次, 转换成K/V list之后是
%%    [{K0, V0 ++ ", " ++ V1 ++ ", " ++ V2}]
%% ii. 对于K="set-cookie", 转换成K/V list之后是
%%    [{K0, V0}, {K0, V1}, {K0, V2}] 
%%    例如: [{"Set-Cookie", V0}, {"Set-Cookie", V1}, {"Set-Cookie", V2}]
-spec to_list(T :: http_headers()) ->
    [{K :: http_header_key(), V :: string()}].
to_list(T) ->
    F = fun({K, {array, L}}, Acc) ->
               L1 = lists:reverse(L),
               lists:foldl(fun (V, Acc1) -> [{K, V} | Acc1] end, Acc, L1);
           (Pair, Acc) ->
               [Pair | Acc]
        end,
    lists:reverse(lists:foldl(F, [], gb_trees:values(T))).

-spec get_value(
        K :: http_header_key(), 
        T :: http_headers()) ->
    string() | undefined.
get_value(K, T) ->
    case lookup(K, T) of
        {value, {_, V}} ->
            expand(V);
        none ->
            undefined
    end.

%% 返回value中第一个由分号;分隔的部分.
%%
%% 例如:
%% "a;b;c" -> "a"
-spec get_primary_value(
        K :: http_header_key(), 
        T :: http_headers()) ->
    string() | undefined.
get_primary_value(K, T) ->
    case get_value(K, T) of
        undefined ->
            undefined;
        V ->
            lists:takewhile(fun (C) -> C =/= $; end, V)
    end.

%% 注意:
%% 返回的{value, {RetK, RetV}}中, RetK是没有经过格式化的原始的K,
%% 如果经过insert/3插入过多次, 则返回的RetK是第一次插入时候的原始的K.
-spec lookup(
        K :: http_header_key(), 
        T :: http_headers()) ->
    {value, {RetK :: http_header_key(), RetV :: string()}} | 
    none.
lookup(K, T) ->
    case gb_trees:lookup(normalize(K), T) of
        {value, {K0, V}} ->
            {value, {K0, expand(V)}};
        none ->
            none
    end.


%% 插入K/V, 如果老的K的数据存在, 则保留老的数据.
-spec default(
        K :: http_header_key(),
        V :: http_header_value(),
        T :: http_headers()) ->
    http_headers().
default(K, V, T) ->
    K1 = normalize(K),
    V1 = any_to_list(V),
    try gb_trees:insert(K1, {K, V1}, T)
    catch
        error:{key_exists, _} ->
            T
    end.


%% 插入K/V, 如果老的K的数据已经存在, 则自动替换.
-spec enter(
        K :: http_header_key(),
        V :: http_header_value(),
        T :: http_headers()) ->
    http_headers().
enter(K, V, T) ->
    K1 = normalize(K),
    V1 = any_to_list(V),
    gb_trees:enter(K1, {K, V1}, T).

%% 插入新的数据到headers中, 如果存在老的数据, 则自动merge.
%%
%% 规则:
%% 1) 如果K以前不存在, 则实际存储数据的格式是
%%    normalize(K)  - {K, normalize(V)}
%%
%% 2) 如果K以前存在, 则实际存储数据的格式是
%%    (K0是第一次插入的时候, 没有经过格式化处理的K)
%%    i. normalize(K) =:= "set-cookie"
%%       normalize(K) - {K0, {array, [V2, V1, V0]}
%%    ii. normalize(K) =/= "set-cookie"
%%       normalize(K) - {K0, V0 ++ ", " ++ V1 ++ ", " ++ V2}
-spec insert(
        K :: http_header_key(),
        V :: http_header_value(),
        T :: http_headers()) ->
    http_headers().
insert(K, V, T) ->
    K1 = normalize(K),
    V1 = any_to_list(V),
    try gb_trees:insert(K1, {K, V1}, T)
    catch
        error:{key_exists, _} ->
            {K0, V0} = gb_trees:get(K1, T),
            V2 = merge(K1, V1, V0),
            gb_trees:update(K1, {K0, V2}, T)
    end.

-spec delete_any(
        K :: http_header_key(), 
        T :: http_headers()) ->
    http_headers().
delete_any(K, T) ->
    K1 = normalize(K),
    gb_trees:delete_any(K1, T).

%% Internal API:

%% 针对K="set-cookie"做统一标准化处理
-spec expand(V :: list() | {array, list()}) -> string().
expand({array, L}) ->
    relax_util:join(lists:reverse(L), ", ");
expand(V) ->
    V.

%% 针对K = "set-cookie"做特殊处理  -> {array, [V2, V1, V0]}  老的Val在后面
%% 其它的K -> V0 ++ ", " ++ V1 ++ ", " ++ V2                 老的Val在前面
-spec merge(K :: string(), NewV :: string(), OldV :: string() | {array, list()}) -> 
    string() | 
    {array, list()}. %% "set-cookie"
merge("set-cookie", V1, {array, L}) ->
    {array, [V1 | L]};
merge("set-cookie", V1, V0) ->
    {array, [V1, V0]};
merge(_, V1, V0) ->
    V0 ++ ", " ++ V1.

%% 格式化HTTP Headers Key: - string()
%% 由于HTTP Headers的Key不区分大小写, 所以我们把Key
%% 都格式化成小写的string().
-spec normalize(K :: http_header_key()) -> string().
normalize(K) when is_list(K) ->
    string:to_lower(K);
normalize(K) when is_atom(K) ->
    normalize(atom_to_list(K));
normalize(K) when is_binary(K) ->
    normalize(binary_to_list(K)).

%% 格式化HTTP Headers Value: - string()
-spec any_to_list(V :: http_header_value()) -> string().
any_to_list(V) when is_list(V) ->
    V;
any_to_list(V) when is_atom(V) ->
    atom_to_list(V);
any_to_list(V) when is_binary(V) ->
    binary_to_list(V);
any_to_list(V) when is_integer(V) ->
    integer_to_list(V).
