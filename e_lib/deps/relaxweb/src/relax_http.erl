-module(relax_http).
-author('erlangonrails@gmail.com').
-export([start/1,        %% 有监控树
         start_link/1,   %% 没有监控树
         stop/1]).

-export([loop/2]).

-define(REQUEST_RECV_TIMEOUT, 300000).   %% timeout waiting for request line
-define(HEADERS_RECV_TIMEOUT, 30000).    %% timeout waiting for headers
-define(MAX_HEADERS, 1000).              %% HTTP Headers中Fields的最大数量


%% 和erlang:decode_packet/3的协议定义一致.
%%
%% 部分1: HTTP请求的第一行
%% HttpRequest = {http_request, HttpMethod, HttpUri, HttpVersion}
%%
%% a. HttpMethod
%% HttpMethod = HttpMethodAtom | HttpString
%% HttpMethodAtom = 'OPTIONS' | 'GET' | 'HEAD' | 'POST' | 'PUT' | 'DELETE' | 'TRACE'
%% HttpString = string() | binary()
%%
%% b. HttpUri
%% HttpUri = '*' | 
%%           {absoluteURI, http|https, Host=HttpString, Port=integer()|undefined, Path=HttpString} | 
%%           {scheme, Scheme=HttpString, HttpString} | 
%%           {abs_path, HttpString} | 
%%           HttpString
%%
%% c. HttpVersion
%% HttpVersion = {Major=integer(), Minor=integer()}
%%
%% 部分2: HTTP请求的Header Field
%% HttpHeader = {http_header, integer(), HttpField, Reserved=term(), Value=HttpString}
%%
%% HttpField = HttpFieldAtom | HttpString
%% HttpFieldAtom = 'Cache-Control' | 'Connection' | 'Date' | 'Pragma' | 'Transfer-Encoding' | 'Upgrade' | 
%%                 'Via' | 'Accept' | 'Accept-Charset' | 'Accept-Encoding' | 'Accept-Language' | 'Authorization' | 
%%                 'From' | 'Host' | 'If-Modified-Since' | 'If-Match' | 'If-None-Match' | 'If-Range' | 'If-Unmodified-Since' | 
%%                 'Max-Forwards' | 'Proxy-Authorization' | 'Range' | 'Referer' | 'User-Agent' | 'Age' | 'Location' | 
%%                 'Proxy-Authenticate' | 'Public' | 'Retry-After' | 'Server' | 'Vary' | 'Warning' | 'Www-Authenticate' | 
%%                 'Allow' | 'Content-Base' | 'Content-Encoding' | 'Content-Language' | 'Content-Length' | 
%%                 'Content-Location' | 'Content-Md5' | 'Content-Range' | 'Content-Type' | 'Etag' | 'Expires' | 
%%                 'Last-Modified' | 'Accept-Ranges' | 'Set-Cookie' | 'Set-Cookie2' | 'X-Forwarded-For' | 'Cookie' | 
%%                 'Keep-Alive' | 'Proxy-Connection'
%% HttpString = string() | binary()

%% HttpRequest = {http_request, http_method(), http_uri(), http_version()}
-type http_version() :: {Major :: integer(), Minor :: integer()}.
-type http_string() :: string() | binary().
-type http_method_atom() :: 'OPTIONS' | 'GET' | 'HEAD' | 'POST' | 'PUT' | 'DELETE' | 'TRACE'.
-type http_method() :: http_method_atom() | http_string().
-type http_uri() :: '*' | 
                    {absoluteURI, http | https, Host :: http_string(), Port :: integer() | undefined, Path :: http_string()} | 
                    {scheme, Scheme :: http_string(), http_string()} | 
                    {abs_path, http_string()} | 
                    http_string().

%% HttpHeader = {http_header, integer(), http_field(), Reserved = term(), Value = http_string()}
-type http_field_atom() :: 'Cache-Control' | 'Connection' | 'Date' | 'Pragma' | 'Transfer-Encoding' | 'Upgrade' | 
                           'Via' | 'Accept' | 'Accept-Charset' | 'Accept-Encoding' | 'Accept-Language' | 'Authorization' | 
                           'From' | 'Host' | 'If-Modified-Since' | 'If-Match' | 'If-None-Match' | 'If-Range' | 'If-Unmodified-Since' | 
                           'Max-Forwards' | 'Proxy-Authorization' | 'Range' | 'Referer' | 'User-Agent' | 'Age' | 'Location' | 
                           'Proxy-Authenticate' | 'Public' | 'Retry-After' | 'Server' | 'Vary' | 'Warning' | 'Www-Authenticate' | 
                           'Allow' | 'Content-Base' | 'Content-Encoding' | 'Content-Language' | 'Content-Length' | 
                           'Content-Location' | 'Content-Md5' | 'Content-Range' | 'Content-Type' | 'Etag' | 'Expires' | 
                           'Last-Modified' | 'Accept-Ranges' | 'Set-Cookie' | 'Set-Cookie2' | 'X-Forwarded-For' | 'Cookie' | 
                           'Keep-Alive' | 'Proxy-Connection'.
-type http_field() :: http_field_atom() | http_string().


%% 将自定义的HTTP Loop回调函数, 替换成relax_socket_server
%% 需要的Socket层面的回调函数.
-spec parse_options(Options :: list()) -> list().
parse_options(Options) ->
    {loop, HttpLoop} = proplists:lookup(loop, Options),
    Loop = {?MODULE, loop, [HttpLoop]},
    [{loop, Loop} | proplists:delete(loop, Options)].

-spec loop(
        Socket :: relax_socket:socket(), 
        Body :: fun() | {M :: atom(), F :: atom()} | {M :: atom(), F :: atom(), A :: list()}) ->
    ignore.
loop(Socket, Body) ->
    ok = relax_socket:setopts(Socket, [{packet, http}]),
    request(Socket, Body).

%% 执行自定义的HTTP Loop回调函数(第一个参数是Req)
%% 这个回调函数在HTTP Header Fields解析完成之后调用.
%%
%% 注意:
%% 此时只解析了客户端传递过来的HTTP Header Fields信息, 
%% 并没有开始解析Body, 也就是目前我们并不关心Body的格式, 我们可以调用其它插件, 
%% 如MultiPart插件来解析, 或者使用默认内置的UrlEncoding的方式来解析Body, 
%% 甚至可以自己写插件来解析Body的内容.
call_body({M, F, A}, Req) ->
    erlang:apply(M, F, [Req | A]);
call_body({M, F}, Req) ->
    M:F(Req);
call_body(Body, Req) ->
    Body(Req).

-spec stop(Name :: atom()) -> ok.
stop(Name) ->
    relax_socket_server:stop(Name).

%% 支持的选项:
%% name :: atom()   - 必须
%% port :: relax_socket:socket_port() - 必须
%% ip :: any | inet:ip_address() 
%% loop :: fun() | {M :: atom(), F :: atom()} | {M :: atom(), F :: atom(), A :: list()} - 必须
%% nodelay :: boolean()
%% backlog :: integer()
%% acceptor_pool_size :: integer()
%% ssl :: boolean()
%% ssl_opts :: list()
%% profile_fun :: fun()
%%
%% 注意:
%% 除了loop函数以外, 其它所有的参数语义都合relax_socket_server一致.
%% <1> relax_socket_server - 自定义loop
%%     调用方式参考: relax_acceptor:call_loop/2
%%     自定义的回调函数的第一个参数是Socket :: relax_socket:socket()
%%
%% <2> relax_http          - 自定义loop
%%     调用方式参考: relax_http:call_body/2
%%     自定义的回调函数的第一个参数是Req
-spec start(Options :: list()) ->
    {ok, pid()} | {error, _}.
start(Options) ->
    relax_socket_server:start(parse_options(Options)).

-spec start_link(Options :: list()) ->
    {ok, pid()} | {error, _}.
start_link(Options) ->
    relax_socket_server:start_link(parse_options(Options)).


%% 解析HTTP Request(解析HTTP Request Line)
%%
%% HttpRequest = {http_request, HttpMethod, HttpUri, HttpVersion}
%%
%% a. HttpMethod
%% HttpMethod = HttpMethodAtom | HttpString
%% HttpMethodAtom = 'OPTIONS' | 'GET' | 'HEAD' | 'POST' | 'PUT' | 'DELETE' | 'TRACE'
%% HttpString = string() | binary()
%%
%% b. HttpUri
%% HttpUri = '*' | 
%%           {absoluteURI, http|https, Host=HttpString, Port=integer()|undefined, Path=HttpString} | 
%%           {scheme, Scheme=HttpString, HttpString} | 
%%           {abs_path, HttpString} | 
%%           HttpString
%%
%% c. HttpVersion
%% HttpVersion = {Major=integer(), Minor=integer()}
%%
%%
%% 例如:
%% GET /folder?k1=v1&k2=v2 HTTP/1.1
%%
%% 解析之后的数据是:
%% {http_request, 'GET', {abs_path,"/folder?k1=v1&k2=v2"}, {1,1}}
-spec request(
        Socket :: relax_socket:socket(), 
        Body :: fun() | {M :: atom(), F :: atom()} | {M :: atom(), F :: atom(), A :: list()}) ->
    ignore.
request(Socket, Body) ->
    ok = relax_socket:setopts(Socket, [{active, once}]),
    receive
        %% {http_request, HttpMethod, HttpUri, HttpVersion}
        {Protocol, _, {http_request, Method, Path, Version}} when Protocol == http orelse Protocol == ssl ->
            ok = relax_socket:setopts(Socket, [{packet, httph}]),
            %% 成功解析出了HTTP Request Line, 继续解析HTTP Header Fields
            headers(Socket, {Method, Path, Version}, [], Body, 0);
        {Protocol, _, {http_error, "\r\n"}} when Protocol == http orelse Protocol == ssl ->
            request(Socket, Body);
        {Protocol, _, {http_error, "\n"}} when Protocol == http orelse Protocol == ssl ->
            request(Socket, Body);
        {tcp_closed, _} ->
            relax_socket:close(Socket),
            exit(normal);
        {ssl_closed, _} ->
            relax_socket:close(Socket),
            exit(normal);
        _Other ->
            handle_invalid_request(Socket)
    after ?REQUEST_RECV_TIMEOUT ->
        relax_socket:close(Socket),
        exit(normal)
    end.

%% 解析HTTP Header Fields:
%%
%% HttpHeader = {http_header, integer(), HttpField, Reserved=term(), Value=HttpString}
%%
%% HttpField = HttpFieldAtom | HttpString
%% HttpFieldAtom = 'Cache-Control' | 'Connection' | 'Date' | 'Pragma' | 'Transfer-Encoding' | 'Upgrade' | 
%%                 'Via' | 'Accept' | 'Accept-Charset' | 'Accept-Encoding' | 'Accept-Language' | 'Authorization' | 
%%                 'From' | 'Host' | 'If-Modified-Since' | 'If-Match' | 'If-None-Match' | 'If-Range' | 'If-Unmodified-Since' | 
%%                 'Max-Forwards' | 'Proxy-Authorization' | 'Range' | 'Referer' | 'User-Agent' | 'Age' | 'Location' | 
%%                 'Proxy-Authenticate' | 'Public' | 'Retry-After' | 'Server' | 'Vary' | 'Warning' | 'Www-Authenticate' | 
%%                 'Allow' | 'Content-Base' | 'Content-Encoding' | 'Content-Language' | 'Content-Length' | 
%%                 'Content-Location' | 'Content-Md5' | 'Content-Range' | 'Content-Type' | 'Etag' | 'Expires' | 
%%                 'Last-Modified' | 'Accept-Ranges' | 'Set-Cookie' | 'Set-Cookie2' | 'X-Forwarded-For' | 'Cookie' | 
%%                 'Keep-Alive' | 'Proxy-Connection'
%% HttpString = string() | binary()
-spec headers(
        Socket :: relax_socket:socket(),
        Request :: {HttpMethod :: http_method(), HttpUri :: http_uri(), HttpVersion :: http_version()},
        Headers :: [{Name :: http_field(), Value :: http_string()}],
        Body :: fun() | {M :: atom(), F :: atom()} | {M :: atom(), F :: atom(), A :: list()},
        HeaderCount :: integer()) ->
    ignore.
headers(Socket, Request, Headers, _Body, ?MAX_HEADERS) ->
    %% Too many headers sent, bad request.
    ok = relax_socket:setopts(Socket, [{packet, raw}]),
    handle_invalid_request(Socket, Request, Headers);
headers(Socket, Request, Headers, Body, HeaderCount) ->
    ok = relax_socket:setopts(Socket, [{active, once}]),
    receive
        %% header fields解析完成, 执行回调逻辑
        {Protocol, _, http_eoh} when Protocol == http orelse Protocol == ssl ->
            Req = new_request(Socket, Request, Headers),
            call_body(Body, Req),

            %% call_body/2中的上层业务逻辑处理完成之后, 直接关闭socket, 
            %% 不支持Keep-Alive等保持TCP连接的特性
            relax_socket:close(Socket),
            exit(normal);
        %% {http_header, integer(), HttpField, Reserved=term(), Value=HttpString}
        {Protocol, _, {http_header, _, Name, _, Value}} when Protocol == http orelse Protocol == ssl ->
            headers(Socket, Request, [{Name, Value} | Headers], Body, 1 + HeaderCount);
        {tcp_closed, _} ->
            relax_socket:close(Socket),
            exit(normal);
        _Other ->
            handle_invalid_request(Socket, Request, Headers)
    after ?HEADERS_RECV_TIMEOUT ->
        relax_socket:close(Socket),
        exit(normal)
    end.


-spec handle_invalid_request(
        Socket :: relax_socket:socket()) -> 
    ignore.
handle_invalid_request(Socket) ->
    handle_invalid_request(Socket, {'GET', {abs_path, "/"}, {0,9}}, []),
    exit(normal).

%% 给客户端回复400错误
-spec handle_invalid_request(
        Socket :: relax_socket:socket(), 
        Request :: {HttpMethod :: http_method(), HttpUri :: http_uri(), HttpVersion :: http_version()}, 
        RevHeaders :: [{Name :: http_field(), Value :: http_string()}]) -> 
    ignore.
handle_invalid_request(Socket, Request, RevHeaders) ->
    Req = new_request(Socket, Request, RevHeaders),
    Req:respond({400, [], []}),
    relax_socket:close(Socket),
    exit(normal).

%% 创建一个Request对象
-spec new_request(
        Socket :: relax_socket:socket(),
        Request :: {HttpMethod :: http_method(), HttpUri :: http_uri(), HttpVersion :: http_version()},
        RecvHeaders :: [{Name :: http_field(), Value :: http_string()}]) ->
    Request :: atom().  %% 参数化模板
new_request(Socket, Request, RevHeaders) ->
    ok = relax_socket:setopts(Socket, [{packet, raw}]),
    new_request_internal({Socket, Request, lists:reverse(RevHeaders)}).

-spec new_request_internal(
       {Socket :: relax_socket:socket(),
        Request :: {HttpMethod :: http_method(), HttpUri :: http_uri(), HttpVersion :: http_version()},
        RecvHeaders :: [{Name :: http_field(), Value :: http_string()}]}) ->
    Request :: atom().  %% 参数化模板
new_request_internal({Socket, {Method, {abs_path, Uri}, Version}, Headers}) ->
    relax_request:new(Socket,
                      Method,
                      Uri,
                      Version,
                      relax_header:make(Headers));
% this case probably doesn't "exist".
new_request_internal({Socket, {Method, {absoluteURI, _Protocol, _Host, _Port, Uri}, Version}, Headers}) ->
    relax_request:new(Socket,
                      Method,
                      Uri,
                      Version,
                      relax_header:make(Headers));
%% Request-URI is "*"
%% From http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html#sec5.1.2
new_request_internal({Socket, {Method, '*'=Uri, Version}, Headers}) ->
    relax_request:new(Socket,
                      Method,
                      Uri,
                      Version,
                      relax_header:make(Headers)).

