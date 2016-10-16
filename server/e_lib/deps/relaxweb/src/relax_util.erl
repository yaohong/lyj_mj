-module(relax_util).
-author('erlangonrails@gmail.com').

-export([urlsplit/1, urlsplit_path/1, urlunsplit/1, urlunsplit_path/1]). %% URL解析相关APIs
-export([quote_plus/1, unquote/1, urlencode/1, parse_qs/1]).             %% urlencoding编&解码相关APIs
-export([ensure_app_started/1,
         join/2,
         to_iolist/1]).

-define(PERCENT, 37).  % $\%
-define(FULLSTOP, 46). % $\.
-define(IS_HEX(C), ((C >= $0 andalso C =< $9) orelse
                    (C >= $a andalso C =< $f) orelse
                    (C >= $A andalso C =< $F))).
-define(QS_SAFE(C), ((C >= $a andalso C =< $z) orelse
                     (C >= $A andalso C =< $Z) orelse
                     (C >= $0 andalso C =< $9) orelse
                     (C =:= ?FULLSTOP orelse C =:= $- orelse C =:= $~ orelse
                      C =:= $_))).

hexdigit(C) when C < 10 -> $0 + C;
hexdigit(C) when C < 16 -> $A + (C - 10).

unhexdigit(C) when C >= $0, C =< $9 -> C - $0;
unhexdigit(C) when C >= $a, C =< $f -> C - $a + 10;
unhexdigit(C) when C >= $A, C =< $F -> C - $A + 10.


%% URL格式:
%% http://www.example.com:8000/path/subfold/a?k1=v1;k2=v2#tag1
%% |scheme| ++ |netloc| ++ |path| ++ |query| ++ |fragment|
%%
%% scheme = http
%% netloc = www.example.com:8000
%% path = /path/subfold/a
%% query = k1=v1;k2=v2
%% fragment = tag1

%% URL encoding & decoding
%% URL encoding把不安全的字符替换成%开头的16进制表示方式.
%% (安全的字符有: [0-9a-zA-Z], '.', '-', '_', '~')
%% 注意:
%% 还有一个特殊字符\s, 也就是<space>, 既可以编码成%20也可以编码成'+', 
%% 我们在这里编码成+.
%%
%% 例如:
%% 1. quote_plus("\"hello world\"").
%%    "%22hello+world%22"   
%%    (空格\s被编码成$+)
%% 2. unquote("%22hello+world%22").                       
%%    "\"hello world\""
%%
%% 如何编码中文?
%% 1. A = unicode:characters_to_binary("李强", utf8).
%%    <<230,157,142,229,188,186>>
%% 2. B = quote_plus(A).
%%    "%E6%9D%8E%E5%BC%BA"
%% 3. unquote(B).
%%    [230,157,142,229,188,186]

%% HTTP Get & Post的原理
%% http请求的4部分:
%% request-line
%% headers(0个或者多个)
%% /r/n
%% body (只针对post数据有效)
%%
%% http响应的4部分:
%% status-line
%% headers(0个或者多个)
%% /r/n
%% body
%%
%% 1. Get
%%    get的数据是放在url之中(以?开头, 拼接在url之后, 中间用&或者;分割多个k/v对), 
%%    经过url encoding编码之后, 传递给服务器的, 传递的是k/v对.
%%
%%    例如:
%%    GET /books/?sex=man&name=Professional HTTP/1.1
%%
%% 2. Post
%%    post把提交的数据放在是http request body中, 因此必须将Content-type设置为
%%    application/x-www-form-urlencoded.
%%    post设计用来支持web窗体上的用户字段, 其参数也是作为key/value对传输.
%%
%%    例如:
%%    POST /book HTTP/1.1
%%    Host: www.example.com
%%    User-Agent: Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.7.6)
%%    Gecko/20050225 Firefox/1.0.1
%%    Content-Type: application/x-www-form-urlencoded
%%    Content-Length: 40
%%    Connection: Keep-Alive
%%    /r/n
%%    name=Professional%20Ajax&publisher=Wiley
%%


%% 把一个URL分隔成一个'五元组', 参数必须是一个完整的URL
%% (注意: 不对%开头的url编码做特殊处理)
-spec urlsplit(Url :: string()) ->
    {Schema :: string(),
     Netloc :: string(),
     Path :: string(),
     Query :: string(),
     Fragment :: string()}.
urlsplit(Url) ->
    {Scheme, Url1} = urlsplit_scheme(Url),
    {Netloc, Url2} = urlsplit_netloc(Url1),
    {Path, Query, Fragment} = urlsplit_path(Url2),
    {Scheme, Netloc, Path, Query, Fragment}.


%% 解析schema, 如果schema不存在, 则返回{"", Url}
%%
%% schema的定义:
%% The scheme name consists of a letter followed by any combination 
%% of letters, digits, and the plus ("+"), period ("."), or hyphen ("-") 
%% characters. It is followed by a colon (":").
-spec urlsplit_scheme(Url :: string()) -> {Schema :: string(), Rest :: string()}.
urlsplit_scheme(Url) ->
    case urlsplit_scheme(Url, []) of
        no_scheme ->
            {"", Url};
        Res ->
            Res
    end.

urlsplit_scheme([C | Rest], Acc) when ((C >= $a andalso C =< $z) orelse
                                       (C >= $A andalso C =< $Z) orelse
                                       (C >= $0 andalso C =< $9) orelse
                                       C =:= $+ orelse C =:= $- orelse
                                       C =:= $.) ->
    urlsplit_scheme(Rest, [C | Acc]);
urlsplit_scheme([$: | Rest], Acc=[_ | _]) ->
    {string:to_lower(lists:reverse(Acc)), Rest};
urlsplit_scheme(_Rest, _Acc) ->
    no_scheme.

urlsplit_netloc("//" ++ Rest) ->
    urlsplit_netloc(Rest, []);
urlsplit_netloc(Path) ->
    {"", Path}.

urlsplit_netloc("", Acc) ->
    {lists:reverse(Acc), ""};
urlsplit_netloc(Rest=[C | _], Acc) when C =:= $/; C =:= $?; C =:= $# ->
    {lists:reverse(Acc), Rest};
urlsplit_netloc([C | Rest], Acc) ->
    urlsplit_netloc(Rest, [C | Acc]).


%% 把一个path|url转换成一个'三元组'.
%% (注意: 不对%开头的url编码做特殊处理)
%%
%% 例如:
%% 1. urlsplit_path("http://www.example.com/a?k=v#tag").
%%    {"http://www.example.com/a","k=v","tag"}
%% 2. urlsplit_path("a?k=v#tag").                 
%%    {"a","k=v","tag"}
%% 3. urlsplit_path("/a?k=v#tag"). 
%%    {"/a","k=v","tag"}
-spec urlsplit_path(P :: string()) ->
    {Path :: string(), Query :: string(), Fragment :: string()}.
urlsplit_path(Path) ->
    urlsplit_path(Path, []).

urlsplit_path("", Acc) ->
    {lists:reverse(Acc), "", ""};
urlsplit_path("?" ++ Rest, Acc) ->
    {Query, Fragment} = urlsplit_query(Rest),
    {lists:reverse(Acc), Query, Fragment};
urlsplit_path("#" ++ Rest, Acc) ->
    {lists:reverse(Acc), "", Rest};
urlsplit_path([C | Rest], Acc) ->
    urlsplit_path(Rest, [C | Acc]).

urlsplit_query(Query) ->
    urlsplit_query(Query, []).

urlsplit_query("", Acc) ->
    {lists:reverse(Acc), ""};
urlsplit_query("#" ++ Rest, Acc) ->
    {lists:reverse(Acc), Rest};
urlsplit_query([C | Rest], Acc) ->
    urlsplit_query(Rest, [C | Acc]).


%% 将一个'五元组'组合成一个url
%% (注意: 不涉及%开头的url编码)
-spec urlunsplit(
        {Scheme :: string(),
         Netloc :: string(),
         Path :: string(),
         Query :: string(),
         Fragment :: string()}) -> string().
urlunsplit({Scheme, Netloc, Path, Query, Fragment}) ->
    lists:flatten([case Scheme of "" -> "";  _ -> [Scheme, "://"] end,
                   Netloc,
                   urlunsplit_path({Path, Query, Fragment})]).

%% 将一个'三元组'组合成一个path | url.
%% (注意: 不涉及%开头的url编码)
-spec urlunsplit_path(
        {Path :: string(),
         Query :: string(),
         Fragment :: string()}) -> string().
urlunsplit_path({Path, Query, Fragment}) ->
    lists:flatten([Path,
                   case Query of "" -> ""; _ -> [$? | Query] end,
                   case Fragment of "" -> ""; _ -> [$# | Fragment] end]).

%% urlencoding
-spec quote_plus(A :: atom() |
                      integer() |
                      float() |
                      string() |
                      binary()) ->
    string().
quote_plus(Atom) when is_atom(Atom) ->
    quote_plus(atom_to_list(Atom));
quote_plus(Int) when is_integer(Int) ->
    quote_plus(integer_to_list(Int));
quote_plus(Binary) when is_binary(Binary) ->
    quote_plus(binary_to_list(Binary));
quote_plus(Float) when is_float(Float) ->
    quote_plus(ieee754:digits(Float));
quote_plus(String) ->
    quote_plus(String, []).

quote_plus([], Acc) ->
    lists:reverse(Acc);
quote_plus([C | Rest], Acc) when ?QS_SAFE(C) ->
    quote_plus(Rest, [C | Acc]);
quote_plus([$\s | Rest], Acc) ->
    quote_plus(Rest, [$+ | Acc]);
quote_plus([C | Rest], Acc) ->
    <<Hi:4, Lo:4>> = <<C>>,
    quote_plus(Rest, [hexdigit(Lo), hexdigit(Hi), ?PERCENT | Acc]).


%% 解码urlencoding编码的数据
-spec unquote(A :: string() | binary()) -> string().
unquote(Binary) when is_binary(Binary) ->
    unquote(binary_to_list(Binary));
unquote(String) ->
    qs_revdecode(lists:reverse(String)).

qs_revdecode(S) ->
    qs_revdecode(S, []).

qs_revdecode([], Acc) ->
    Acc;
qs_revdecode([$+ | Rest], Acc) ->
    qs_revdecode(Rest, [$\s | Acc]);
qs_revdecode([Lo, Hi, ?PERCENT | Rest], Acc) when ?IS_HEX(Lo), ?IS_HEX(Hi) ->
    qs_revdecode(Rest, [(unhexdigit(Lo) bor (unhexdigit(Hi) bsl 4)) | Acc]);
qs_revdecode([C | Rest], Acc) ->
    qs_revdecode(Rest, [C | Acc]).


-spec urlencode(
        [{Key :: atom() | integer() | float() | string() | binary(), 
          Val :: atom() | integer() | float() | string() | binary()}]) -> 
    string().
urlencode(Props) ->
    Pairs = lists:foldr(
              fun ({K, V}, Acc) ->
                  [quote_plus(K) ++ "=" ++ quote_plus(V) | Acc]
              end, [], Props),
    string:join(Pairs, "&").

%% 解析一个application/x-www-form-urlencoded编码的数据
%% 或者一个query string.
%%
%% 上面的两种数据都是urlencoding的, 解析后返回的k/v对已经对数据进行了解码.
-spec parse_qs(A :: string() | binary()) ->
    [{K :: string(), V :: string()}].
parse_qs(Binary) when is_binary(Binary) ->
    parse_qs(binary_to_list(Binary));
parse_qs(String) ->
    parse_qs(String, []).

parse_qs([], Acc) ->
    lists:reverse(Acc);
parse_qs(String, Acc) ->
    {Key, Rest} = parse_qs_key(String),
    {Value, Rest1} = parse_qs_value(Rest),
    parse_qs(Rest1, [{Key, Value} | Acc]).

parse_qs_key(String) ->
    parse_qs_key(String, []).

%% 以;或者&分隔k/v对
parse_qs_key([], Acc) ->
    {qs_revdecode(Acc), ""};
parse_qs_key([$= | Rest], Acc) ->
    {qs_revdecode(Acc), Rest};
parse_qs_key(Rest=[$; | _], Acc) ->
    {qs_revdecode(Acc), Rest};
parse_qs_key(Rest=[$& | _], Acc) ->
    {qs_revdecode(Acc), Rest};
parse_qs_key([C | Rest], Acc) ->
    parse_qs_key(Rest, [C | Acc]).

parse_qs_value(String) ->
    parse_qs_value(String, []).

parse_qs_value([], Acc) ->
    {qs_revdecode(Acc), ""};
parse_qs_value([$; | Rest], Acc) ->
    {qs_revdecode(Acc), Rest};
parse_qs_value([$& | Rest], Acc) ->
    {qs_revdecode(Acc), Rest};
parse_qs_value([C | Rest], Acc) ->
    parse_qs_value(Rest, [C | Acc]).


-spec ensure_app_started(App :: atom()) -> ok.
ensure_app_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% 在一个'binary或者string'组成的list中插入分隔符(string/char/binary),
%% 结果是扁平化的. 
%%
%% 例子:
%% join(["a", "b", "c"], "1").
%% "a1b1c"
%% join(["a", "b", "c"], <<"1">>).
%% [97,<<"1">>,98,<<"1">>,99]
%% join(["a", "b", <<"c">>], "1").
%% [97,49,98,49,<<"c">>]
-spec join(Source :: iolist(), Separator :: iolist()) -> iolist().
join([], _Separator) ->
    [];
join([S], _Separator) ->
    lists:flatten(S);
join(Strings, Separator) ->
    lists:flatten(revjoin(lists:reverse(Strings), Separator, [])).

revjoin([], _Separator, Acc) ->
    Acc;
revjoin([S | Rest], Separator, []) ->
    revjoin(Rest, Separator, [S]);
revjoin([S | Rest], Separator, Acc) ->
    revjoin(Rest, Separator, [S, Separator | Acc]).


-spec to_iolist(A :: atom() | integer() | list() | binary()) -> 
    iodata().
to_iolist(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
to_iolist(Integer) when is_integer(Integer) ->
    integer_to_list(Integer);
to_iolist(Io) when is_list(Io); is_binary(Io) ->
    Io.
