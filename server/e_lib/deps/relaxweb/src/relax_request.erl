-module(relax_request). 
-author('erlangonrails@gmail.com').
-include_lib("kernel/include/file.hrl").
-include("file_log.hrl").

-define(RECBUF_SIZE, 8192).
-define(IODEVICE_SIZE, 8192).

-export([new/5]).
-export([send/2, recv/2, recv/3, recv_body/1, recv_body/2]).
-export([get_header_value/2, get_primary_header_value/2]).         %% 获取HTTP Header Fields相关的数据
-export([get/2]).                                                  %% 获取这次HTTP请求相关的各个item
-export([parse_qs/1, get_qs_value/2, get_qs_value/3]).             %% 获取QueryString相关的数据
-export([parse_post/1, get_post_value/2, get_post_value/3]).       %% 获取Post相关的数据
-export([parse_cookie/1, get_cookie_value/2, get_cookie_value/3]). %% 获取Cookie相关的数据
-export([respond/2, ok/2, not_found/1, not_found/2]).
-export([server_file/3, server_file/4]).

-define(SAVE_QS, relax_request_qs).                                %% URL中的QueryString对应的k/v list(数据经过了url decode)
-define(SAVE_PATH, relax_request_path).                            %% 去掉了Querystring和Fragment的Path, 并且经过url decode
-define(SAVE_BODY, relax_request_body).                            %% Http Request Body(二进制的Body原始数据)
-define(SAVE_BODY_LENGTH, relax_request_body_length).              %% Content-Length
-define(SAVE_POST, relax_request_post).                            %% Post对应的k/v list(数据经过了url decode)
-define(SAVE_COOKIE, relax_request_cookie).                        %% Cookie对应的k/v list

-define(IDLE_TIMEOUT, 300000).         %% 5 Minutes
-define(MAX_RECV_BODY, (1024 * 1024)). %% 1 MB

-type request() :: {relax_request, [term()]}.   %% 具体格式参考new/5的返回值


%% 使用tuple modules来代替老版本的parameterized modules, 需要自己实现new/N函数.
-spec new(
        Socket :: relax_socket:socket(), 
        Method :: relax_http:http_method(), 
        RawPath :: relax_http:http_string(), 
        Version :: relax_http:http_version(), 
        Headers :: relax_header:http_headers()) ->
    request().
new(Socket, Method, RawPath, Version, Headers) ->
    {?MODULE, [Socket, Method, RawPath, Version, Headers]}.

-spec send(
        Data :: iodata(),
        Request :: request()) -> 
    ok.
send(Data, {?MODULE, [Socket, _Method, _RawPath, _Version, _Headers]}) ->
    case relax_socket:send(Socket, Data) of
        ok ->
            ok;
        _ ->
            exit(normal)
    end.

-spec recv(
        Length :: integer(),
        Request :: request()) -> 
    binary().
recv(Length, {?MODULE, [_Socket, _Method, _RawPath, _Version, _Headers]} = THIS) ->
    recv(Length, ?IDLE_TIMEOUT, THIS).

-spec recv(
        Length :: integer(), 
        Timeout :: timeout(),
        Request :: request()) -> 
    binary().
recv(Length, Timeout, {?MODULE, [Socket, _Method, _RawPath, _Version, _Headers]}) ->
    case relax_socket:recv(Socket, Length, Timeout) of
        {ok, Data} ->
            Data;
        _ ->
            exit(normal)
    end.

-spec body_length(Request :: request()) -> 
    undefined | 
    {unknown_transfer_encoding, _} | 
    integer().
body_length({?MODULE, [_Socket, _Method, _RawPath, _Version, _Headers]} = THIS) ->
    case get_header_value("transfer-encoding", THIS) of
        undefined ->
            case get_header_value("content-length", THIS) of
                undefined ->
                    undefined;
                Length ->
                    list_to_integer(Length)
            end;
        Unknown ->
            {unknown_transfer_encoding, Unknown}
    end.

-spec recv_body(Request :: request()) -> 
    undefined | binary().
recv_body({?MODULE, [_Socket, _Method, _RawPath, _Version, _Headers]} = THIS) ->
    recv_body(?MAX_RECV_BODY, THIS).


%% 接收一个http Body, 限定Body的最大长度
-spec recv_body(
        MaxBodyLen :: integer(),
        Request :: request()) -> 
    undefined | binary().
recv_body(MaxBodyLen, {?MODULE, [_Socket, _Method, _RawPath, _Version, _Headers]} = THIS) ->
    case erlang:get(?SAVE_BODY) of
        undefined ->
            Body = stream_body(MaxBodyLen, THIS),
            put(?SAVE_BODY, Body),
            Body;
        Cached -> 
            Cached
    end.

-spec stream_body(
        MaxBodyLength :: integer(),
        Request :: request()) -> 
    undefined | binary().
stream_body(MaxBodyLength, {?MODULE, [_Socket, _Method, _RawPath, _Version, _Headers]} = THIS) ->
    case body_length(THIS) of
        undefined ->
            undefined;
        {unknown_transfer_encoding, Unknown} ->
            exit({unknown_transfer_encoding, Unknown});
        0 ->
            <<>>;
        Length when is_integer(Length) ->
            case MaxBodyLength < Length of
                true ->
                    exit({body_too_large, content_length});
                false ->
                    stream_body_internal(Length, [], THIS)
            end
    end.

-spec stream_body_internal(
        L :: integer(), 
        Acc :: list(),
        Request :: request()) ->
    binary().
stream_body_internal(0, Acc, _) ->
    iolist_to_binary(lists:reverse(Acc));
stream_body_internal(Length, Acc, {?MODULE, [_Socket, _Method, _RawPath, _Version, _Headers]} = THIS) when Length > 0 ->
    Size = 
      case Length > ?RECBUF_SIZE of
          true -> ?RECBUF_SIZE;
          false -> Length
      end,
    Bin = recv(Size, THIS),
    stream_body_internal(Length - Size, [Bin | Acc], THIS).


-spec get_header_value(
        K :: http_header:http_header_key(),
        Request :: request()) ->
    string() | undefined.
get_header_value(K, {?MODULE, [_Socket, _Method, _RawPath, _Version, Headers]}) ->
    relax_header:get_value(K, Headers).

-spec get_primary_header_value(
        K :: http_header:http_header_key(),
        Request :: request()) ->
    string() | undefined.
get_primary_header_value(K, {?MODULE, [_Socket, _Method, _RawPath, _Version, Headers]}) ->
    relax_header:get_primary_value(K, Headers).


%% 支持的选项:
%% socket      - relax_socket:socket()
%% scheme      - http | https
%% method      - relax_http:http_method()
%% raw_path    - relax_http:http_string()
%% version     - relax_http:http_version()
%% headers     - relax_header:http_headers()
%% peer        - string()
%% path        - string() 经过URL decode之后的path (并且去掉了QueryString和Fragment)
%% body_length - undefined | {unknown_transfer_encoding, _} | integer().
%%
%% 注意:
%% 区别raw_path和path
%% a. raw_path是原始的Url请求, 并且没有经过url decode操作
%% b. path是不包括query string和fragment并且经过urldecode之后的path, 通常用于
%%    'HTTP消息的路由', 根据path不同, 路由到响应的controller.
%%
%% 例如:
%% http://domain.com/book%20food/my?user=id#sec1
%% raw_path -> "/book%20food/my?user=id#sec1"
%% path     -> "/book food/my"
get(socket, {?MODULE, [Socket, _Method, _RawPath, _Version, _Headers]}) ->
    Socket;
get(scheme, {?MODULE, [Socket, _Method, _RawPath, _Version, _Headers]}) ->
    case relax_socket:type(Socket) of
        plain ->
            http;
        ssl ->
            https
    end;
get(method, {?MODULE, [_Socket, Method, _RawPath, _Version, _Headers]}) ->
    Method;
get(raw_path, {?MODULE, [_Socket, _Method, RawPath, _Version, _Headers]}) ->
    RawPath;
get(version, {?MODULE, [_Socket, _Method, _RawPath, Version, _Headers]}) ->
    Version;
get(headers, {?MODULE, [_Socket, _Method, _RawPath, _Version, Headers]}) ->
    Headers;
get(peer, {?MODULE, [Socket, _Method, _RawPath, _Version, _Headers]}) ->
    case relax_socket:peername(Socket) of
        {ok, {Addr, _Port}} ->
            inet_parse:ntoa(Addr);
        {error, enotconn} ->
            exit(normal)
    end;
get(path, {?MODULE, [_Socket, _Method, RawPath, _Version, _Headers]}) ->
    case erlang:get(?SAVE_PATH) of
        undefined ->
            {Path0, _, _} = relax_util:urlsplit_path(RawPath),
            Path = relax_util:unquote(Path0),
            put(?SAVE_PATH, Path),
            Path;
        Cached ->
            Cached
    end;
get(body_length, {?MODULE, [_Socket, _Method, _RawPath, _Version, _Headers]} = THIS) ->
    case erlang:get(?SAVE_BODY_LENGTH) of
        undefined ->
            BodyLength = body_length(THIS),
            put(?SAVE_BODY_LENGTH, {cached, BodyLength}),
            BodyLength;
        {cached, Cached} ->
            Cached
    end.


%% 把QueryString解析成k/v list.
-spec parse_qs(Request :: request()) -> 
    [{K :: string(), V :: string()}].
parse_qs({?MODULE, [_Socket, _Method, RawPath, _Version, _Headers]}) ->
    case erlang:get(?SAVE_QS) of
        undefined ->
            {_, QueryString, _} = relax_util:urlsplit_path(RawPath),
            Parsed = relax_util:parse_qs(QueryString),
            put(?SAVE_QS, Parsed),
            Parsed;
        Cached ->
            Cached
    end.

-spec get_qs_value(
        K :: string(),
        Request :: request()) -> 
    string() | undefined.
get_qs_value(Key, {?MODULE, [_Socket, _Method, _RawPath, _Version, _Headers]} = THIS) ->
    proplists:get_value(Key, parse_qs(THIS)).

-spec get_qs_value(
        K :: string(), 
        Default :: string(),
        Request :: request()) -> 
    string() | undefined.
get_qs_value(Key, Default, {?MODULE, [_Socket, _Method, _RawPath, _Version, _Headers]} = THIS) ->
    proplists:get_value(Key, parse_qs(THIS), Default).

%% 解析cookie
-spec parse_cookie(Request :: request()) -> 
    [{K :: string(), V :: string()}].
parse_cookie({?MODULE, [_Socket, _Method, _RawPath, _Version, _Headers]} = THIS) ->
    case erlang:get(?SAVE_COOKIE) of
        undefined ->
            Cookies = case get_header_value("cookie", THIS) of
                          undefined ->
                              [];
                          Value ->
                              relax_cookie:parse_cookie(Value)
                      end,
            put(?SAVE_COOKIE, Cookies),
            Cookies;
        Cached ->
            Cached
    end.

-spec get_cookie_value(
        K :: string(),
        Request :: request()) -> 
    string() | undefined.
get_cookie_value(Key, {?MODULE, [_Socket, _Method, _RawPath, _Version, _Headers]} = THIS) ->
    proplists:get_value(Key, parse_cookie(THIS)).

-spec get_cookie_value(
        K :: string(), 
        Default :: string(),
        Request :: request()) -> 
    string() | undefined.
get_cookie_value(Key, Default, {?MODULE, [_Socket, _Method, _RawPath, _Version, _Headers]} = THIS) ->
    proplists:get_value(Key, parse_cookie(THIS), Default).

%% 解析post data
%% (会触发recv_body/0的逻辑, 因为post的数据在body中).
-spec parse_post(Request :: request()) -> 
    [{K :: string(), V :: string()}].
parse_post({?MODULE, [_Socket, _Method, _RawPath, _Version, _Headers]} = THIS) ->
    case erlang:get(?SAVE_POST) of
        undefined ->
            Parsed = case recv_body(THIS) of
                         undefined ->
                             [];
                         Binary ->
                             case get_primary_header_value("content-type", THIS) of
                                 "application/x-www-form-urlencoded" ++ _ ->
                                     relax_util:parse_qs(Binary);
                                 _ ->
                                     []
                             end
                     end,
            put(?SAVE_POST, Parsed),
            Parsed;
        Cached ->
            Cached
    end.

-spec get_post_value(
        K :: string(),
        Request :: request()) -> 
    string() | undefined.
get_post_value(Key, {?MODULE, [_Socket, _Method, _RawPath, _Version, _Headers]} = THIS) ->
    proplists:get_value(Key, parse_post(THIS)).

-spec get_post_value(
        K :: string(), 
        Default :: string(),
        Request :: request()) -> 
    string() | undefined.
get_post_value(Key, Default, {?MODULE, [_Socket, _Method, _RawPath, _Version, _Headers]} = THIS) ->
    proplists:get_value(Key, parse_post(THIS), Default).


%% 发送数据给客户端
-spec respond(
        {Code :: integer(), 
         ResponseHeaders :: [{relax_header:http_header_key(), relax_header:http_header_value()}] | relax_header:http_headers(),
         Body :: iodata() | {file, IoDevice :: file:io_device()}},
        request()) -> 
    ok.
respond({Code, ResponseHeaders, {file, IoDevice}}, {?MODULE, [_Socket, Method, _RawPath, Version, _Headers]} = THIS) ->
    HResponse = relax_header:make(ResponseHeaders),
    HResponseWithCL = relax_header:enter("Content-Length", iodevice_size(IoDevice), HResponse),
    HResponseEnd = relax_header:default_from_list(server_headers(), HResponseWithCL),
    
    F = fun ({K, V}, Acc) ->
          [relax_util:to_iolist(K), <<": ">>, V, <<"\r\n">> | Acc]
        end,
    HeadersIo = lists:foldl(F, [<<"\r\n">>], relax_header:to_list(HResponseEnd)),
    send([make_version(Version), make_code(Code), <<"\r\n">> | HeadersIo], THIS),
    case Method of
        'HEAD' ->
            ok;
        _ ->
            %% 用户发送静态文件
            iodevice_stream(fun send/2, IoDevice, THIS)
    end;
respond({Code, ResponseHeaders, Body}, {?MODULE, [_Socket, Method, _RawPath, Version, _Headers]} = THIS) ->
    HResponse = relax_header:make(ResponseHeaders),
    HResponseWithCL = relax_header:enter("Content-Length", iolist_size(Body), HResponse),
    HResponseEnd = relax_header:default_from_list(server_headers(), HResponseWithCL),
    
    F = fun ({K, V}, Acc) ->
          [relax_util:to_iolist(K), <<": ">>, V, <<"\r\n">> | Acc]
        end,
    HeadersIo = lists:foldl(F, [<<"\r\n">>], relax_header:to_list(HResponseEnd)),
    send([make_version(Version), make_code(Code), <<"\r\n">> | HeadersIo], THIS),

    case Method of
        'HEAD' ->
            ok;
        _ ->
            send(Body, THIS)
    end.


-spec ok(
        {ContentType :: relax_header:http_header_value(), 
         ResponseHeaders :: [{relax_header:http_header_key(), relax_header:http_header_value()}],
         Body :: iodata()},
        request()) ->
    ok.
ok({Body}, {?MODULE, [_Socket, _Method, _RawPath, _Version, _Headers]} = THIS) ->
    ok({"text/plain", [], Body}, THIS);
ok({ContentType, Body}, {?MODULE, [_Socket, _Method, _RawPath, _Version, _Headers]} = THIS) ->
    ok({ContentType, [], Body}, THIS);
ok({ContentType, ResponseHeaders, Body}, {?MODULE, [_Socket, _Method, _RawPath, _Version, _Headers]} = THIS) ->
    HResponse = relax_header:make(ResponseHeaders),
    HResponseWithCT = relax_header:enter("Content-Type", ContentType, HResponse),
    respond({200, HResponseWithCT, Body}, THIS).

not_found({?MODULE, [_Socket, _Method, _RawPath, _Version, _Headers]} = THIS) ->
    not_found([], THIS).

-spec not_found(
        ExtraHeaders :: [{relax_header:http_header_key(), relax_header:http_header_value()}],
        Request :: request()) -> 
    ok.
not_found(ExtraHeaders, {?MODULE, [_Socket, _Method, _RawPath, _Version, _Headers]} = THIS) ->
    respond({404, [{"Content-Type", "text/plain"} | ExtraHeaders], <<"file_not_found.\n">>}, THIS).


server_file(Path, DocRoot, {?MODULE, [_Socket, _Method, _RawPath, _Version, _Headers]} = THIS) ->
    server_file(Path, DocRoot, [], THIS).

-spec server_file(
        Path :: string(),  
        DocRoot :: string(), 
        ExtraHeaders :: [{relax_header:http_header_key(), relax_header:http_header_value()}],
        Request :: request()) ->
    ok.
server_file(Path, DocRoot, ExtraHeaders, {?MODULE, [_Socket, _Method, _RawPath, _Version, _Headers]} = THIS) ->
    FullPath = filename:join([DocRoot, Path]),
    case filelib:is_dir(FullPath) of
        true ->
            ?FILE_LOG_DEBUG("not_found_due_to_dir:~p", [FullPath]),
            not_found(ExtraHeaders, THIS);
        false ->
            maybe_server_file(FullPath, ExtraHeaders, THIS)
   end.

-spec maybe_server_file(
        File :: file:filename(),
        ExtraHeaders :: [{relax_header:http_header_key(), relax_header:http_header_value()}],
        Request :: request()) ->
    ok.
maybe_server_file(File, ExtraHeaders, {?MODULE, [_Socket, _Method, _RawPath, _Version, _Headers]} = THIS) ->
    case file:read_file_info(File) of
        {ok, FileInfo} ->
            LastModified = httpd_util:rfc1123_date(FileInfo#file_info.mtime),
            case get_header_value("if-modified-since", THIS) of
                LastModified ->
                    %% 文件的时间与服务器的时间一致
                    respond({304, ExtraHeaders, ""}, THIS); %% 304 - "Not Modified"
                _ ->
                    case file:open(File, [raw, binary]) of
                        {ok, IoDevice} ->
                            ContentType = relax_mime:guess_mime(File),
                            ok({ContentType,
                                [{"last-modified", LastModified} | ExtraHeaders],
                                {file, IoDevice}},
                               THIS),
                            file:close(IoDevice);
                        _ ->
                            ?FILE_LOG_DEBUG("not_found_due_to_open_error:~p", [File]),
                            not_found(ExtraHeaders, THIS)
                    end
            end;
        {error, _} ->
            ?FILE_LOG_DEBUG("not_found_due_to_read_file_info_error:~p", [File]),
            not_found(ExtraHeaders, THIS)
    end.


-spec server_headers() -> 
    iodata().
server_headers() ->
    [{"Server", "RelaxWeb/1.0"},
     {"Date", httpd_util:rfc1123_date()}].

-spec make_code(X :: integer() | iodata()) -> 
    iodata().
make_code(X) when is_integer(X) ->
    [integer_to_list(X), [" " | httpd_util:reason_phrase(X)]];
make_code(Io) when is_list(Io); is_binary(Io) ->
    Io.

-spec make_version(Version :: relax_http:http_version()) ->
    iodata().
make_version({1, 0}) ->
    <<"HTTP/1.0 ">>;
make_version(_) ->
    <<"HTTP/1.1 ">>.

-spec iodevice_foldl(
        F :: fun(),
        Acc :: ok,
        IoDevice :: file:io_device(),
        BufferSize :: integer()) ->
    ok.
iodevice_foldl(F, Acc, IoDevice, BufferSize) ->
    case file:read(IoDevice, BufferSize) of
        eof ->
            Acc;
        {ok, Data} ->
            iodevice_foldl(F, F(Data, Acc), IoDevice, BufferSize)
    end.

-spec iodevice_stream(
        Callback :: fun(), 
        IoDevice :: file:io_device(),
        Request :: request()) ->
    ok.
iodevice_stream(Callback, IoDevice, {?MODULE, [_Socket, _Method, _RawPath, _Version, _Headers]} = THIS) ->
    iodevice_stream(Callback, IoDevice, ?IODEVICE_SIZE, THIS).

-spec iodevice_stream(
        Callback :: fun(), 
        IoDevice :: file:io_device(), 
        BufferSize :: integer(),
        Request :: request()) ->
    ok.
iodevice_stream(Callback, IoDevice, BufferSize, {?MODULE, [_Socket, _Method, _RawPath, _Version, _Headers]} = THIS) ->
    F = fun(Data, ok) -> Callback(Data, THIS) end,
    ok = iodevice_foldl(F, ok, IoDevice, BufferSize).


%% 返回磁盘文件的长度
-spec iodevice_size(IoDevice :: file:io_device()) ->
    integer().
iodevice_size(IoDevice) ->
    {ok, Size} = file:position(IoDevice, eof),
    {ok, 0} = file:position(IoDevice, bof),
    Size.
