-module(relax_cookie).
-author('erlangonrails@gmail.com').
-export([parse_cookie/1,                  %% Parse: 解析一个Client -> Server的Cookie (有多个k/v对)
         cookie/3, cookie/2,              %% Gen: 生成一个Server -> Client的Set-Cookie (只包含一个k/v, 可能包含属性)
         rfc2109_cookie_expires_date/1]).

%% HTTP Cookie parsing and generating (RFC 2109, RFC 2965).

%% Cookie分为两种:
%% 1. Server -> Client -- Set-Cookie
%%
%%    例如:
%%    HTTP/1.1 200 OK
%%    Content-type: text/html
%%    Set-Cookie: name=value
%%    Set-Cookie: name2=value2; Expires=Wed, 09 Jun 2021 10:18:14 GMT
%%
%%    每个Set-Cookie只能设置一对k/v, 可以包含一些属性.
%%
%% 2. Client -> Server -- Cookie
%%    
%%    例如:
%%    GET /spec.html HTTP/1.1
%%    Host: www.example.org
%%    Cookie: name=value; name2=value2
%%    Accept: */*
%%
%%    一个Cookie可以设置多对k/v, 中间以分号;或者逗号,分隔, 不包含属性.
%%    建议使用的分隔符是分号;
%%
%%
%%    我们支持的4种格式的Cookie(Client -> Server)
%%    (分隔符可以是分号;或者逗号,, Value可以被双引号引起来)
%%    k= "value"; k2 = "value2" 
%%    k= value; k2 = value2
%%    k= "value", k2 = "value2"
%%    k = value, k2 = value2
%%
%%    例如:
%%    parse_cookie("k=value; k2= value2").
%%    [{"k","value"},{"k2","value2"}]
%%    parse_cookie("k=\"value\"; k2= \"value2\"").
%%    [{"k","value"},{"k2","value2"}]
%%    parse_cookie("k=value, k2= value2").        
%%    [{"k","value"},{"k2","value2"}]
%%    parse_cookie("k=\"value\", k2= \"value2\"").
%%    [{"k","value"},{"k2","value2"}]
%%
%%
%%    注意 - 1: 
%%    客户端传递来的Cookie没有正确的使用分隔符, 可能会导致Cookie中后续的k/v丢失. 
%%    '错误'的Cookie格式:
%%    parse_cookie("k=value k2= value2").    
%%    [{"k","value"}]
%%    解析之后, k2和value2会被忽略, 因为两对k/v之间没有分隔符(\s不是分隔符),
%%    所以后续的cookie内容都会被丢弃.
%%    详细的说明见skip_past_separator/1
%% 
%%   注意 - 2:
%%   在解析Cookie的时候, 以$开头的Key对应的K/V对会被忽略跳过.
%%   parse_cookie("k1=v1;k2=v2").
%%   [{"k1","v1"},{"k2","v2"}]
%%   parse_cookie("$k1=v1;k2=v2"). 以$开头的key会被忽略跳过
%%   [{"k2","v2"}]

-type http_cookie() :: {string(), string()}.
-type http_cookies() :: [http_cookie()].

-define(QUOTE, $"). %% $\" =:= $"

-define(IS_WHITESPACE(C),
        (C =:= $\s orelse C =:= $\t orelse C =:= $\r orelse C =:= $\n)).

%% RFC 2616 separators (called tspecials in RFC 2068)
-define(IS_SEPARATOR(C),
        (C < 32 orelse
         C =:= $\s orelse C =:= $\t orelse
         C =:= $( orelse C =:= $) orelse C =:= $< orelse C =:= $> orelse
         C =:= $@ orelse C =:= $, orelse C =:= $; orelse C =:= $: orelse
         C =:= $\\ orelse C =:= $" orelse C =:= $/ orelse
         C =:= $[ orelse C =:= $] orelse C =:= $? orelse C =:= $= orelse
         C =:= ${ orelse C =:= $})). %% $\" =:= $"

%% 生成一个默认的Set-Cookie header field tuple.
-spec cookie(
        Key :: atom() | string() | binary() | integer(),
        Value :: atom() | string() | binary() | integer()) ->
    http_cookie().
cookie(Key, Value) ->
    cookie(Key, Value, []).

%% 生成一个Set-Cookie header field tuple.
%% 
%% 支持的属性:
%% {max_age, integer()} %% 秒
%% {local_time, erlang:datetime()}
%% {domain, string()}
%% {path, string()}
%% {secure, boolean()}
%% {http_only, boolean()}
%%
%% 注意:
%% max_age和local_time配合使用, 如果设置了max_age, local_time才起作用.
%%
%% 1. Expire and Max-Age
%% 表示cookie的过期时间, 告诉浏览器什么时候删除这些cookie. 如果没有指定过期时间,
%% 在用户关闭浏览器之后, cookie就会失效.
%%
%% Expire的格式是: Wdy, DD-Mon-YYYY HH:MM:SS GMT
%% Max-Age单位是秒, 表示经过多少秒后cookie失效.
%%
%% 2. Domain and Path
%% 这两个属性定义了cookie的适用范围, 告诉浏览器在什么时候需要把cookie传递回给server.
%% 如果不指定, 则用默认的domain和path.
%%
%% 3. Secure and HttpOnly
%% Secure和HttpOnly是两个boolean值, 可以设置或者不设.
%%
%% Secure告诉浏览器, 只有经过加密的通道才能使用这个cookie, 所以web server也只有在加密的通道
%% 的情况下, 才设置该属性.
%%
%% HttpOnly
%% 告诉浏览器这个cookie仅适用于http protocol.
%%
%% 例如:
%% cookie("a", "b", [{max_age, 100}, 
%%                   {domain, "domain.com"}, 
%%                   {path, "/login"}, 
%%                   {http_only, true}, 
%%                   {secure, true}]).
%% {"Set-Cookie",
%%  "a=b; Version=1; Expires=Sat, 26-Mar-2011 06:53:42 GMT; Max-Age=100; Secure; 
%%   Domain=domain.com; Path=/login; HttpOnly"}
%%
%% 注意(参考quote/1):
%% 如果Value中有不兼容的字符, 将抛出error: {cookie_quoting_required, V :: string()}
-spec cookie(
        Key :: atom() | string() | binary() | integer(),
        Value :: atom() | string() | binary() | integer(),
        Options :: [{term(), term()}]) ->
    http_cookie().
cookie(Key, Value, Options) ->
    Cookie = [any_to_list(Key), "=", quote(Value), "; Version=1"],

    ExpiresPart =
        case proplists:get_value(max_age, Options) of
            undefined ->
                "";
            RawAge ->
                When = case proplists:get_value(local_time, Options) of
                           undefined ->
                               calendar:local_time();
                           LocalTime ->
                               LocalTime
                       end,
                Age = case RawAge < 0 of
                          true ->
                              0;
                          false ->
                              RawAge
                      end,
                ["; Expires=", age_to_cookie_date(Age, When),
                 "; Max-Age=", quote(Age)]
        end,

    SecurePart =
        case proplists:get_value(secure, Options) of
            true ->
                "; Secure";
            _ ->
                ""
        end,

    DomainPart =
        case proplists:get_value(domain, Options) of
            undefined ->
                "";
            Domain ->
                ["; Domain=", quote(Domain)]
        end,

    PathPart =
        case proplists:get_value(path, Options) of
            undefined ->
                "";
            Path ->
                ["; Path=", quote(Path)]
        end,

    HttpOnlyPart =
        case proplists:get_value(http_only, Options) of
            true ->
                "; HttpOnly";
            _ ->
                ""
        end,

    CookieParts = [Cookie, ExpiresPart, SecurePart, DomainPart, PathPart, HttpOnlyPart],
    {"Set-Cookie", lists:flatten(CookieParts)}.


%% 对Set-Cookie中Value的约束, 不能包含SEPARATOR字符(可以包含$/).
%%
%% 由于各个浏览器处理quoted strings的方式并不兼容, 我们强制约束
%% relaxweb cookie只包含所有浏览器都兼容的字符. 不能包含特殊字符.
%% 
%% 注意:
%% 如果Value中有不兼容的字符, 将抛出error: {cookie_quoting_required, V :: string()}
-spec quote(V :: string() | atom() | binary() | integer()) -> string().
quote(V0) ->
    V = any_to_list(V0),
    lists:all(fun(Ch) -> Ch =:= $/ orelse not ?IS_SEPARATOR(Ch) end, V)
        orelse erlang:error({cookie_quoting_required, V}),
    V.


%% 返回的时间格式:
%% Wdy, DD-Mon-YYYY HH:MM:SS GMT
%% 
%% 协议: 
%% rfc2109: 10.1.2
%%
%% 例如:
%% rfc2109_cookie_expires_date(erlang:localtime()).
%% "Sat, 26-Mar-2011 05:57:08 GMT"
-spec rfc2109_cookie_expires_date(LocalTime :: erlang:datetime()) -> string().
rfc2109_cookie_expires_date(LocalTime) ->
    {{YYYY,MM,DD},{Hour,Min,Sec}} =
        case calendar:local_time_to_universal_time_dst(LocalTime) of
            [Gmt]   -> Gmt;
            [_,Gmt] -> Gmt
        end,
    DayNumber = calendar:day_of_the_week({YYYY,MM,DD}),
    lists:flatten(
      io_lib:format("~s, ~2.2.0w-~3.s-~4.4.0w ~2.2.0w:~2.2.0w:~2.2.0w GMT",
                    [httpd_util:day(DayNumber),DD,httpd_util:month(MM),YYYY,Hour,Min,Sec])).

add_seconds(Secs, LocalTime) ->
    Greg = calendar:datetime_to_gregorian_seconds(LocalTime),
    calendar:gregorian_seconds_to_datetime(Greg + Secs).

age_to_cookie_date(Age, LocalTime) ->
    rfc2109_cookie_expires_date(add_seconds(Age, LocalTime)).


%% 把Cookie解析成一个{K, V}的proplist.
%% (忽略cookie的属性)
%%
%% 注意:
%% 在解析的时候, 以$开头的Key对应的K/V对会被忽略跳过.
%%
%% 例如:
%% parse_cookie("k1=v1;k2=v2").
%% [{"k1","v1"},{"k2","v2"}]
%% parse_cookie("$k1=v1;k2=v2"). 以$开头的key会被忽略跳过
%% [{"k2","v2"}]
-spec parse_cookie(S :: string()) -> http_cookies().
parse_cookie("") ->
    [];
parse_cookie(Cookie) ->
    parse_cookie(Cookie, []).

%% Internal API

-spec parse_cookie(S :: string(), Acc :: []) -> [{K :: string(), V :: string()}].
parse_cookie([], Acc) ->
    lists:reverse(Acc);
parse_cookie(String, Acc) ->
    {{Token, Value}, Rest} = read_pair(String),
    Acc1 = case Token of
               "" ->
                   Acc;
               "$" ++ _ ->   %% 注意: 以$开头的Key会被跳过忽略
                   Acc;
               _ ->
                   [{Token, Value} | Acc]
           end,
    parse_cookie(Rest, Acc1).

-spec read_pair(String :: string()) ->
    {{K :: string(), V :: string()}, Remain :: string()}.
read_pair(String) ->
    {Token, Rest} = read_token(skip_whitespace(String)),
    {Value, Rest1} = read_value(skip_whitespace(Rest)),
    {{Token, Value}, skip_past_separator(Rest1)}.


%% 读取一个Cookie的value.
%% 我们支持4种格式的Client->Server的Cookie:
%% (分隔符可以是分号;或者逗号,, Value可以被双引号引起来)
%% k= "value"; k2 = "value2" 
%% k= value; k2 = value2
%% k= "value", k2 = "value2"
%% k= value, k2 = value2
-spec read_value(A :: string()) -> {Value :: string(), Remain :: string()}.
read_value([$= | Value]) ->
    Value1 = skip_whitespace(Value),
    case Value1 of
        [?QUOTE | _] ->
            read_quoted(Value1); %% 双引号格式的value: K= "value";
        _ ->
            read_token(Value1)   %% 使用分隔符分隔的value: K= value;等
    end;
read_value(String) ->
    {"", String}.

%% 读取一对引号内的内容, 输出的第一个string,
%% 第一个字符必须是$".
%%
%% read_quoted("\"abcde\\\"fg\"123").
%% {"abcde\"fg","123"}
%% read_quoted("\"abcdefg\"123").    
%% {"abcdefg","123"}
%% read_quoted("\"abcd\tefg\"123").
%% {"abcd\tefg","123"}
%% read_quoted("\"abcd\sefg\"123"). 
%% {"abcd efg","123"}
%% read_quoted("\"abcd\\sefg\"123").
%% {"abcdsefg","123"}
-spec read_quoted(A :: string()) -> {Value :: string(), Remain :: string()}.
read_quoted([?QUOTE | String]) ->
    read_quoted(String, []).

read_quoted([], Acc) ->
    {lists:reverse(Acc), []};
read_quoted([?QUOTE | Rest], Acc) ->
    {lists:reverse(Acc), Rest};
read_quoted([$\\, Any | Rest], Acc) -> %% 对转意字符做特殊处理
    read_quoted(Rest, [Any | Acc]);
read_quoted([C | Rest], Acc) ->
    read_quoted(Rest, [C | Acc]).


%% 跳过开头的'空白'
%%
%% 例如:
%% skip_whitespace("\s\t\sa\sb\sc") 
%% "a\sb\sc"
-spec skip_whitespace(String :: string()) -> string().
skip_whitespace(String) ->
    F = fun (C) -> ?IS_WHITESPACE(C) end,
    lists:dropwhile(F, String).


%% 读取一个token
%% 返回读取到的token和剩下的string().
%%
%% 例如:
%% read_token("abc"). 
%% {"abc",[]}
%% read_token("\s\sabc").
%% {[],"\sabc"}
%% read_token("abc\s123").
%% {"abc"," 123"}
-spec read_token(String :: string()) -> string().
read_token(String) ->
    F = fun (C) -> not ?IS_SEPARATOR(C) end,
    lists:splitwith(F, String).



%% 跳到分号';'和逗号','分隔符, 返回之后的内容.
%% 如果没有这两个分割符, 则返回[].
%%
%% 注意:
%% 在Client -> Server Cookie的多个k/v中间必须有分隔符, 否则后续的
%% k/v对会被解析器跳过.
%%
%% 因为parse_cookie/3每解析出一个k/v对之后, 就会调用该函数一次.
-spec skip_past_separator(A :: string()) -> string().
skip_past_separator([]) ->
    [];
skip_past_separator([$; | Rest]) ->
    Rest;
skip_past_separator([$, | Rest]) ->
    Rest;
skip_past_separator([_ | Rest]) ->
    skip_past_separator(Rest).


-spec any_to_list(V :: string() | atom() | binary() | integer()) -> string().
any_to_list(V) when is_list(V) ->
    V;
any_to_list(V) when is_atom(V) ->
    atom_to_list(V);
any_to_list(V) when is_binary(V) ->
    binary_to_list(V);
any_to_list(V) when is_integer(V) ->
    integer_to_list(V).
