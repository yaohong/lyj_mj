-module(relax_multipart).
-author('erlangonrails@gmail.com').

-export([parse_form/1, parse_form/2]).
-export([parse_multipart_request/2]).
-export([parts_to_body/3, parts_to_multipart_body/4]).
-export([default_file_handler/2]).

-define(CHUNKSIZE, 4096).

-record(mp, {state, boundary, length, buffer, callback, req}).

%% RFC1867协议简介:
%%
%% 1. 普通的POST操作
%% 
%% 例如:
%% <form action="http://www.baidu.com/" method="post">
%%   <input type="text" name="myText1" /><br />
%%   <input type="text" name="myText2" /><br />
%%   <input type="submit" />
%% </form>
%%
%% 该表达提交时会向服务器端发出这样的数据(已经去除部分不相关的头信息):
%% 
%% POST http://www.baidu.com/ HTTP/1.1 \r\n
%% Host: www.baidu.com \r\n
%% Content-Length: 74 \r\n
%% Content-Type: application/x-www-form-urlencoded \r\n
%% \r\n
%% myText1=hello+world&myText2=%E4%BD%A0%E5%A5%BD%E4%B8%96%E7%95%8C
%%
%% 对于普通的HTML POST表单, 它会在头信息里使用Content-Length注明内容长度. 头信息每行(\r\n)一条, 
%% 空行(\r\n)之后便是Body, 即'内容'. 
%% 此外, 我们可以发现它的Content-Type是application/x-www-form-urlencoded, 这意味着消息内容会经过URL编码,
%% 就像在GET请求时URL里的Query String那样.
%% 在上面的例子中, myText1里的空格被编码为加号, 而myText2, 是“你好世界”这四个汉字.
%%
%%
%% 2. RFC 1867目的便是让HTML表单可以提交文件, 它对HTML表单的扩展主要是:
%% a. 为input标记的type属性增加一个file选项
%% b. 在POST情况下, 为form标记的enctype属性定义默认值为application/x-www-form-urlencoded, 为form标记的enctype
%%    属性增加multipart/form-data选项.
%%
%% 我们要使用HTML表单提交文件, 则可以使用如下定义:
%%
%% 例如:
%% <form action="http://www.baidu.com/" method="post" enctype="multipart/form-data">
%%   <input type="text" name="myText" /><br />
%%   <input type="file" name="upload1" /><br />
%%   <input type="file" name="upload2" /><br />
%%   <input type="submit" />
%% </form>
%%
%% 为了实验所需, 我们创建两个文件file1.txt和file2.txt, 内容分别为“This is file1.”及“This is file2, it's bigger.”.
%% 在文本框里写上"hello world", 并选择这两个文件, 提交, 则会看到浏览器传递了如下数据(省略了\r\n):
%%
%% POST http://www.baidu.com/ HTTP/1.1
%% Host: www.baidu.com
%% Content-Length: 495
%% Content-Type: multipart/form-data; boundary=---------------------------7db2d1bcc50e6e
%%
%% -----------------------------7db2d1bcc50e6e
%% Content-Disposition: form-data; name="myText"
%%
%% hello world
%% -----------------------------7db2d1bcc50e6e
%% Content-Disposition: form-data; name="upload1"; filename="C:\file1.txt"
%% Content-Type: text/plain
%%
%% This is file1.
%% -----------------------------7db2d1bcc50e6e
%% Content-Disposition: form-data; name="upload2"; filename="C:\file2.txt"
%% Content-Type: text/plain
%%
%% This is file2, it's longer.
%% -----------------------------7db2d1bcc50e6e--
%%
%% 首先, 第一个空行之前自然还是HTTP头, 之后则是Body. 根据RFC 1867定义, 我们需要选择一段数据作为"分割边界", 
%% 这个"边界数据"不能在内容其他地方出现, 一般来说使用一段从概率上说"几乎不可能"的数据即可.
%% 
%% 例如，上面这段数据使用的是IE 9, 而在Chrome下则是这样的：
%%
%% POST http://www.baidu.com/ HTTP/1.1
%% Host: www.baidu.com
%% Content-Length: 473
%% Content-Type: multipart/form-data; boundary=----WebKitFormBoundaryW49oa00LU29E4c5U
%%
%% ------WebKitFormBoundaryW49oa00LU29E4c5U
%% Content-Disposition: form-data; name="myText"
%%
%% hello world
%% ------WebKitFormBoundaryW49oa00LU29E4c5U
%% Content-Disposition: form-data; name="upload1"; filename="file1.txt"
%% Content-Type: text/plain
%%
%% This is file1.
%% ------WebKitFormBoundaryW49oa00LU29E4c5U
%% Content-Disposition: form-data; name="upload2"; filename="file2.txt"
%% Content-Type: text/plain
%%
%% This is file2, it's bigger.
%% ------WebKitFormBoundaryW49oa00LU29E4c5U--
%%
%% 很显然IE & Chrome选择了不同的数据"模式"作为边界, 选择了边界之后, 便会将它放在头部的Content-Type里传递给
%% 服务器端, 实际需要传递的数据便可以分割为"段", 每段便是"一项"数据.
%%
%% 数据均无需额外编码, 直接传递即可, 例如您可以看出上面的示例中的"空格"均没有变成加号.
%% 至于这里您可以看到清晰地文字内容, 是因为我们上传了仅仅包含可视ASCII码的文本文件, 如果您上传一个普通的文件, 
%% 例如图片, 捕获到的数据则几乎完全不可读了.
%%
%% IE和Chrome在filename的选择策略上有所不同, 前者是文件的完整路径, 而后者则仅仅是文件名.
%% 数据内容以两条横线结尾, 并同样以一个换行结束.
%%
%% 协议需要注意的地方:
%% 1) Content-Type必须指定boundary, 其中前面的"---------------------------xxxx"是IE特有的标志, 其后的字符串
%%    xxxx是随机生成的字符串. Boundary是文件上传时表单中文件分割线.
%% 2) 请求报文体中的分割线
%%    "-----------------------------xxxx"比"boundary=---------------------------xxxx"中的分割线前面多"--".
%% 3) 表单中上传文件的请求头和文件数据之间的分割符是"\r\n \r\n", 与HTTP请求报文头与报文体之间的分割符相同.
%% 4) 上载文件结尾需要追加分割符"\r\n".
%% 5) 最后一个文件结尾的分割线"-----------------------------xx--", 即boundary之前和结尾都需要追加"--".
%%    最后一个分割线比较特殊, 注意不要忘记后面的"--".
%% 6) 最后请求报文结尾需要追加分割符"\r\n".
%% 7) 最后需要注意的是请求报文头中的Content-Length参数的值是整个报文体的长度, 即从分割线第一个字符开始, 
%%    到报文结尾分割符"\r\n"为止的整个长度. 注意不要漏掉计算报文结尾分割符"\r\n"的长度, 否则Content-Length参数的
%%    值不正确.



parts_to_body([{Start, End, Body}], ContentType, Size) ->
    HeaderList = [{"Content-Type", ContentType},
                  {"Content-Range",
                   ["bytes ",
                    relax_util:to_iolist(Start), "-", relax_util:to_iolist(End),
                    "/", relax_util:to_iolist(Size)]}],
    {HeaderList, Body};
parts_to_body(BodyList, ContentType, Size) when is_list(BodyList) ->
    parts_to_multipart_body(BodyList, ContentType, Size,
                            relax_hex:to_hex(crypto:rand_bytes(8))).

parts_to_multipart_body(BodyList, ContentType, Size, Boundary) ->
    HeaderList = [{"Content-Type",
                   ["multipart/byteranges; ",
                    "boundary=", Boundary]}],
    MultiPartBody = multipart_body(BodyList, ContentType, Boundary, Size),

    {HeaderList, MultiPartBody}.

multipart_body([], _ContentType, Boundary, _Size) ->
    ["--", Boundary, "--\r\n"];
multipart_body([{Start, End, Body} | BodyList], ContentType, Boundary, Size) ->
    ["--", Boundary, "\r\n",
     "Content-Type: ", ContentType, "\r\n",
     "Content-Range: ",
         "bytes ", relax_util:to_iolist(Start), "-", relax_util:to_iolist(End),
             "/", relax_util:to_iolist(Size), "\r\n\r\n",
     Body, "\r\n"
     | multipart_body(BodyList, ContentType, Boundary, Size)].

parse_form(Req) ->
    parse_form(Req, fun default_file_handler/2).

parse_form(Req, FileHandler) ->
    Callback = fun (Next) -> parse_form_outer(Next, FileHandler, []) end,
    {_, _, Res} = parse_multipart_request(Req, Callback),
    Res.

parse_form_outer(eof, _, Acc) ->
    lists:reverse(Acc);
parse_form_outer({headers, H}, FileHandler, State) ->
    {"form-data", H1} = proplists:get_value("content-disposition", H),
    Name = proplists:get_value("name", H1),
    Filename = proplists:get_value("filename", H1),
    case Filename of
        undefined ->
            fun (Next) ->
                    parse_form_value(Next, {Name, []}, FileHandler, State)
            end;
        _ ->
            ContentType = proplists:get_value("content-type", H),
            Handler = FileHandler(Filename, ContentType),
            fun (Next) ->
                    parse_form_file(Next, {Name, Handler}, FileHandler, State)
            end
    end.

parse_form_value(body_end, {Name, Acc}, FileHandler, State) ->
    Value = binary_to_list(iolist_to_binary(lists:reverse(Acc))),
    State1 = [{Name, Value} | State],
    fun (Next) -> parse_form_outer(Next, FileHandler, State1) end;
parse_form_value({body, Data}, {Name, Acc}, FileHandler, State) ->
    Acc1 = [Data | Acc],
    fun (Next) -> parse_form_value(Next, {Name, Acc1}, FileHandler, State) end.

parse_form_file(body_end, {Name, Handler}, FileHandler, State) ->
    Value = Handler(eof),
    State1 = [{Name, Value} | State],
    fun (Next) -> parse_form_outer(Next, FileHandler, State1) end;
parse_form_file({body, Data}, {Name, Handler}, FileHandler, State) ->
    H1 = Handler(Data),
    fun (Next) -> parse_form_file(Next, {Name, H1}, FileHandler, State) end.

default_file_handler(Filename, ContentType) ->
    default_file_handler_1(Filename, ContentType, []).

default_file_handler_1(Filename, ContentType, Acc) ->
    fun(eof) ->
            Value = iolist_to_binary(lists:reverse(Acc)),
            {Filename, ContentType, Value};
       (Next) ->
            default_file_handler_1(Filename, ContentType, [Next | Acc])
    end.

parse_multipart_request(Req, Callback) ->
    Length = list_to_integer(Req:get_header_value("content-length")),
    Boundary = iolist_to_binary(
                 get_boundary(Req:get_header_value("content-type"))),
    Prefix = <<"\r\n--", Boundary/binary>>,
    BS = byte_size(Boundary),
    Chunk = read_chunk(Req, Length),
    Length1 = Length - byte_size(Chunk),
    <<"--", Boundary:BS/binary, "\r\n", Rest/binary>> = Chunk,
    feed_mp(headers, flash_multipart_hack(#mp{boundary=Prefix,
                                              length=Length1,
                                              buffer=Rest,
                                              callback=Callback,
                                              req=Req})).

parse_headers(<<>>) ->
    [];
parse_headers(Binary) ->
    parse_headers(Binary, []).

parse_headers(Binary, Acc) ->
    case find_in_binary(<<"\r\n">>, Binary) of
        {exact, N} ->
            <<Line:N/binary, "\r\n", Rest/binary>> = Binary,
            parse_headers(Rest, [split_header(Line) | Acc]);
        not_found ->
            lists:reverse([split_header(Binary) | Acc])
    end.

split_header(Line) ->
    {Name, [$: | Value]} = lists:splitwith(fun (C) -> C =/= $: end,
                                           binary_to_list(Line)),
    {string:to_lower(string:strip(Name)),
     parse_header(Value)}.

read_chunk(Req, Length) when Length > 0 ->
    case Length of
        Length when Length < ?CHUNKSIZE ->
            Req:recv(Length);
        _ ->
            Req:recv(?CHUNKSIZE)
    end.

read_more(State=#mp{length=Length, buffer=Buffer, req=Req}) ->
    Data = read_chunk(Req, Length),
    Buffer1 = <<Buffer/binary, Data/binary>>,
    flash_multipart_hack(State#mp{length=Length - byte_size(Data),
                                  buffer=Buffer1}).

flash_multipart_hack(State=#mp{length=0, buffer=Buffer, boundary=Prefix}) ->
    %% Flash doesn't terminate multipart with \r\n properly so we fix it up here
    PrefixSize = size(Prefix),
    case size(Buffer) - (2 + PrefixSize) of
        Seek when Seek >= 0 ->
            case Buffer of
                <<_:Seek/binary, Prefix:PrefixSize/binary, "--">> ->
                    Buffer1 = <<Buffer/binary, "\r\n">>,
                    State#mp{buffer=Buffer1};
                _ ->
                    State
            end;
        _ ->
            State
    end;
flash_multipart_hack(State) ->
    State.

feed_mp(headers, State=#mp{buffer=Buffer, callback=Callback}) ->
    {State1, P} = case find_in_binary(<<"\r\n\r\n">>, Buffer) of
                      {exact, N} ->
                          {State, N};
                      _ ->
                          S1 = read_more(State),
                          %% Assume headers must be less than ?CHUNKSIZE
                          {exact, N} = find_in_binary(<<"\r\n\r\n">>,
                                                      S1#mp.buffer),
                          {S1, N}
                  end,
    <<Headers:P/binary, "\r\n\r\n", Rest/binary>> = State1#mp.buffer,
    NextCallback = Callback({headers, parse_headers(Headers)}),
    feed_mp(body, State1#mp{buffer=Rest,
                            callback=NextCallback});
feed_mp(body, State=#mp{boundary=Prefix, buffer=Buffer, callback=Callback}) ->
    Boundary = find_boundary(Prefix, Buffer),
    case Boundary of
        {end_boundary, Start, Skip} ->
            <<Data:Start/binary, _:Skip/binary, Rest/binary>> = Buffer,
            C1 = Callback({body, Data}),
            C2 = C1(body_end),
            {State#mp.length, Rest, C2(eof)};
        {next_boundary, Start, Skip} ->
            <<Data:Start/binary, _:Skip/binary, Rest/binary>> = Buffer,
            C1 = Callback({body, Data}),
            feed_mp(headers, State#mp{callback=C1(body_end),
                                      buffer=Rest});
        {maybe, Start} ->
            <<Data:Start/binary, Rest/binary>> = Buffer,
            feed_mp(body, read_more(State#mp{callback=Callback({body, Data}),
                                             buffer=Rest}));
        not_found ->
            {Data, Rest} = {Buffer, <<>>},
            feed_mp(body, read_more(State#mp{callback=Callback({body, Data}),
                                             buffer=Rest}))
    end.

get_boundary(ContentType) ->
    {"multipart/form-data", Opts} = parse_header(ContentType),
    case proplists:get_value("boundary", Opts) of
        S when is_list(S) ->
            S
    end.

find_in_binary(P, Data) when size(P) > 0 ->
    PS = size(P),
    DS = size(Data),
    case DS - PS of
        Last when Last < 0 ->
            partial_find(P, Data, 0, DS);
        Last ->
            case binary:match(Data, P) of
                {Pos, _} -> {exact, Pos};
                nomatch -> partial_find(P, Data, Last+1, PS-1)
            end
    end.

partial_find(_B, _D, _N, 0) ->
    not_found;
partial_find(B, D, N, K) ->
    <<B1:K/binary, _/binary>> = B,
    case D of
        <<_Skip:N/binary, B1:K/binary>> ->
            {partial, N, K};
        _ ->
            partial_find(B, D, 1 + N, K - 1)
    end.

find_boundary(Prefix, Data) ->
    case find_in_binary(Prefix, Data) of
        {exact, Skip} ->
            PrefixSkip = Skip + size(Prefix),
            case Data of
                <<_:PrefixSkip/binary, "\r\n", _/binary>> ->
                    {next_boundary, Skip, size(Prefix) + 2};
                <<_:PrefixSkip/binary, "--\r\n", _/binary>> ->
                    {end_boundary, Skip, size(Prefix) + 4};
                _ when size(Data) < PrefixSkip + 4 ->
                    %% Underflow
                    {maybe, Skip};
                _ ->
                    %% False positive
                    not_found
            end;
        {partial, Skip, Length} when (Skip + Length) =:= size(Data) ->
            %% Underflow
            {maybe, Skip};
        _ ->
            not_found
    end.


parse_header(String) ->
    [Type | Parts] = [string:strip(S) || S <- string:tokens(String, ";")],
    F = fun (S, Acc) ->
                case lists:splitwith(fun (C) -> C =/= $= end, S) of
                    {"", _} ->
                        %% Skip anything with no name
                        Acc;
                    {_, ""} ->
                        %% Skip anything with no value
                        Acc;
                    {Name, [$\= | Value]} ->
                        [{string:to_lower(string:strip(Name)),
                          unquote_header(string:strip(Value))} | Acc]
                end
        end,
    {string:to_lower(Type),
     lists:foldr(F, [], Parts)}.

unquote_header("\"" ++ Rest) ->
    unquote_header(Rest, []);
unquote_header(S) ->
    S.

unquote_header("", Acc) ->
    lists:reverse(Acc);
unquote_header("\"", Acc) ->
    lists:reverse(Acc);
unquote_header([$\\, C | Rest], Acc) ->
    unquote_header(Rest, [C | Acc]);
unquote_header([C | Rest], Acc) ->
    unquote_header(Rest, [C | Acc]).



