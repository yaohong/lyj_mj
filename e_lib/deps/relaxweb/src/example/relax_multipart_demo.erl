-module(relax_multipart_demo).
-author('erlangonrails@gmail.com').
-include("file_log.hrl").
-export([start/0,        %% 有监控树
         start_link/0,   %% 没有监控树
         stop/0, 
         handle_request/2]).

-define(RELAX_DEMO_PORT, 6789).
-define(RELAX_DEMO_DOCROOT, ".").

start() ->
    ok = relax_util:ensure_app_started(relaxweb),
    relax_http:start(options()).

start_link() ->
    ok = relax_util:ensure_app_started(relaxweb),
    relax_http:start_link(options()).

stop() ->
    relax_http:stop(?MODULE).

options() ->
    [{name, ?MODULE},
     {port, ?RELAX_DEMO_PORT},
     {ip, any},
     {loop, {?MODULE, handle_request, [?RELAX_DEMO_DOCROOT]}},
     {profile_fun, fun(A) -> ?FILE_LOG_INFO("profile: ~p", [A]) end}].


handle_request(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    Method = Req:get(method),
    {Main, Minor} = Req:get(version),
    ?FILE_LOG_INFO("~s ~s HTTP ~B.~B", [Method, Path, Main, Minor]),
    case Method of
        'GET' ->
            route_req_get(Req, Path, DocRoot);
        'POST' ->
            route_req_post(Req, Path, DocRoot);
        _ ->
            Req:respond({500,[],"relax_multipart_demo error"})
    end.

route_req_get(Req, _Path, _DocRoot) ->
    Req:respond({200, [{"Content-Type", "text/html"}], upload_page()}).

%% 使用curl来测试这个文件上传的demo:
%% curl --form upload1=@localfilename1 
%%      --form upload2=@localfilename2 
%%      --form text1=val1 
%%      --form text2=val2 http://127.0.0.1:6789
%%
%% <1> HTML页面的Form结构
%% <form action='/' method='post' enctype='multipart/form-data'>
%%   <input type='text' name='text1' /><br />
%%   <input type='text' name='text2' /><br />
%%   <input type='file' name='upload1' /><br />
%%   <input type='file' name='upload2' /><br />
%%   <input type='submit' /><br />
%% </form>
%%
%%
%% <2> 一次HTTP POST Multipart Request(省略了部分内容, 只保留关键数据)
%% Request Headers:
%% Content-Type:multipart/form-data; boundary=----WebKitFormBoundaryEeVhvw54M9c26mKu
%%
%% Payload数据:
%% ------WebKitFormBoundaryEeVhvw54M9c26mKu
%% Content-Disposition: form-data; name="text1"
%%
%% str1
%% ------WebKitFormBoundaryEeVhvw54M9c26mKu
%% Content-Disposition: form-data; name="text2"
%%
%% str2
%% ------WebKitFormBoundaryEeVhvw54M9c26mKu
%% Content-Disposition: form-data; name="upload1"; filename="file1"
%% Content-Type: application/octet-stream
%%
%%
%% ------WebKitFormBoundaryEeVhvw54M9c26mKu
%% Content-Disposition: form-data; name="upload2"; filename="file2"
%% Content-Type: application/octet-stream
%%
%%
%% ------WebKitFormBoundaryEeVhvw54M9c26mKu--
%%
%% <3> 解析出来对应的Ret
%%
%% [{"text1","str1"},
%%  {"text2","str2"},
%%  {"upload1",{"file1",{"application/octet-stream",[]},<<"file1-data\n">>}},
%%  {"upload2",{"file2",{"application/octet-stream",[]},<<"file2-data\n">>}}]
%%
%%
%% <4> 注意:
%% 如果textN为空   -> value为""   -> 文本内容是string()
%% 如果uploadN为空 -> 文件名为""  -> FileBinaries 为<<>>  文件名是string(), 文件内容是binary()
route_req_post(Req, _Path, DocRoot) ->
    Ret = relax_multipart:parse_form(Req),
    Text1 = proplists:get_value("text1", Ret),
    Text2 = proplists:get_value("text2", Ret),
    {Filename1, _ContentType1, Bin1} = proplists:get_value("upload1", Ret),
    {Filename2, _ContentType2, Bin2} = proplists:get_value("upload2", Ret),
    %% 1. 注意文件上传为空的情况
    %% 2. 使用文件内容的MD5作为文件名
    UUID1 = case Filename1 =/= "" andalso Bin1 =/= <<>>  of
                true -> 
                    relax_hex:to_hex(erlang:md5(Bin1));
                false ->
                    ""
            end,
    UUID2 = case Filename2 =/= "" andalso Bin2 =/= <<>>  of
                true -> 
                    relax_hex:to_hex(erlang:md5(Bin2));
                false ->
                    ""
            end,
    save_file(DocRoot, UUID1, Bin1),
    save_file(DocRoot, UUID2, Bin2),
    Response = format_response(Text1, Text2, Filename1, UUID1, Filename2, UUID2),
    Req:respond({200, [{"Content-Type", "text/html"}], Response}).

format_response(Text1, Text2, Upload1Target, UUID1, Upload2Target, UUID2) ->
     "<html>"
      "<head>"
        "<title>relaxweb fileupload service test page:</title>"
        "<meta http-equiv='Content-Type' content='text/html; charset=utf-8'/>"
      "</head>"

      "<body>"
        "<h2>relaxweb fileupload result:</h2>"
        "text1:" ++ Text1 ++ "<br />"
        "text2:" ++ Text2 ++ "<br />"
        "upload1: " ++ Upload1Target ++ ", uuid1 -> " ++ UUID1 ++"<br />"
        "upload2: " ++ Upload2Target ++ ", uuid2 -> " ++ UUID2 ++"<br />"
      "</body>"
    "</html>".

    


upload_page() ->
    "<html>"
      "<head>"
        "<title>relaxweb fileupload service test page:</title>"
        "<meta http-equiv='Content-Type' content='text/html; charset=utf-8'/>"
      "</head>"

      "<body>"
        "<h2>relaxweb fileupload service:</h2>"
        "<form action='/' method='post' enctype='multipart/form-data'>"
          "text1: <input type='text' name='text1' /><br />"
          "text2: <input type='text' name='text2' /><br />"
          "upload1: <input type='file' name='upload1' /><br />"
          "upload2: <input type='file' name='upload2' /><br />"
          "<input type='submit' /><br />"
        "</form>"
      "</body>"
    "</html>".

save_file(_UploadFileRoot, "", _Bin) ->
    ignreo;
save_file(UploadFileRoot, Filename, Bin) ->
    {ok, File} = file:open(filename:join(UploadFileRoot, Filename), [raw, write]),
    file:write(File, Bin),
    file:close(File).

