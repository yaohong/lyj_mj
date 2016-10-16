-module(relax_basic_auth).
-author('erlangonrails@gmail.com').
-export([parse_req/1,
         parse_auth/1,
         unauth_respond/1]).

%% 实现HTTP Basic Auth协议
%% http://en.wikipedia.org/wiki/Basic_authentication_scheme

-spec parse_req(Req :: module()) -> 
    undefined | {Username :: string(), Password :: string()}.
parse_req(Req) ->
    case Req:get_header_value("authorization") of
        undefined -> 
            undefined;
        Authorization ->
            parse_auth(Authorization)
    end.

%% Auth = "Basic " ++ base64:encode_to_string("user1:password1").    
%% "Basic dXNlcjE6cGFzc3dvcmQx"
-spec parse_auth(Authorzation :: string()) ->
    undefined | {Username :: string(), Password :: string()}.
parse_auth(Authorization) when is_list(Authorization) ->
    case string:tokens(Authorization, [$\s]) of
        ["Basic", Base64Str] ->
            DecodeBase64 = base64:decode_to_string(Base64Str),
            case string:tokens(DecodeBase64, ":") of
                [Username, Password] -> 
                    {Username, Password};
                _ ->
                    undefined
            end;
        ["basic", Base64Str] ->
            DecodeBase64 = base64:decode_to_string(Base64Str),
            case string:tokens(DecodeBase64, ":") of
                [Username, Password] -> 
                    {Username, Password};
                _ ->
                    undefined
            end;
        _ ->
            undefined
    end.

-spec unauth_respond(Req :: module()) -> ok.
unauth_respond(Req) ->
    Req:respond({401, [{"Content-Type","text/html"}, 
                       {"WWW-Authenticate", "Basic realm='Secure Area'"}], 
                "<HTML><HEAD><TITLE>Error</TITLE></HEAD><BODY>401 Unauthorized.</BODY></HTML>"}).

