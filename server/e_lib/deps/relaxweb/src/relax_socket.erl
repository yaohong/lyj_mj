-module(relax_socket).
-author('erlangonrails@gmail.com').
-export([listen/4, 
         accept/1, 
         recv/3, 
         send/2, 
         close/1, 
         port/1, 
         sockname/1,
         peername/1,
         setopts/2, 
         type/1]).

-define(ACCEPT_TIMEOUT, 2000).

-type socket() :: gen_tcp:socket() | ssl:sslsocket().
-type socket_type() :: plain | ssl.
-type socket_port() :: 0..65535.

-spec listen(
          Ssl :: boolean(), 
          Port :: socket_port(),
          Opts :: list(),
          SslOpts :: list()) -> 
    {ok, socket()} | {error, _}.
listen(Ssl, Port, Opts, SslOpts) ->
    case Ssl of
        true ->
            case ssl:listen(Port, Opts ++ SslOpts) of
                {ok, ListenSocket} ->
                    {ok, {ssl, ListenSocket}};
                {error, _} = Err ->
                    Err
            end;
        false ->
            gen_tcp:listen(Port, Opts)
    end.

-spec accept(Socket :: socket()) ->
    {ok, socket()} | {error, _}.
accept({ssl, ListenSocket}) ->
    %% There's a bug in ssl:transport_accept/2 at the moment, which is the
    %% reason for the try...catch block. Should be fixed in OTP R14.
    try ssl:transport_accept(ListenSocket) of
        {ok, Socket} ->
            case ssl:ssl_accept(Socket) of
                ok ->
                    {ok, {ssl, Socket}};
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    catch
        error:{badmatch, {error, Reason}} ->
            {error, Reason}
    end;
accept(ListenSocket) ->
    gen_tcp:accept(ListenSocket, ?ACCEPT_TIMEOUT).

-spec recv(
        Socket :: socket(),
        Length :: integer(),
        Timeout :: timeout()) ->
    {ok, term()} | {error, _}.
recv({ssl, Socket}, Length, Timeout) ->
    ssl:recv(Socket, Length, Timeout);
recv(Socket, Length, Timeout) ->
    gen_tcp:recv(Socket, Length, Timeout).

-spec send(Socket :: socket(), Data :: iodata()) -> 
    ok | {error, _}.
send({ssl, Socket}, Data) ->
    ssl:send(Socket, Data);
send(Socket, Data) ->
    gen_tcp:send(Socket, Data).

-spec close(Socket :: socket()) -> ok | {error, _}.
close({ssl, Socket}) ->
    ssl:close(Socket);
close(Socket) ->
    gen_tcp:close(Socket).

%% local port
-spec port(Socket :: socket()) ->
    {ok, socket_port()} | {error, _}.
port({ssl, Socket}) ->
    case ssl:sockname(Socket) of
        {ok, {_, Port}} ->
            {ok, Port};
        {error, _} = Err ->
            Err
    end;
port(Socket) ->
    inet:port(Socket).

-spec sockname(Socket :: socket()) ->
    {ok, {inet:ip_address(), socket_port()}} | {error, _}.
sockname({ssl, Socket}) ->
    ssl:sockname(Socket);
sockname(Socket) ->
    inet:sockname(Socket).

-spec peername(Socket :: socket()) ->
    {ok, {inet:ip_address(), socket_port()}} | {error, _}.
peername({ssl, Socket}) ->
    ssl:peername(Socket);
peername(Socket) ->
    inet:peername(Socket).

-spec setopts(Socket :: socket(), Opts :: list()) ->
    ok | {error, _}.
setopts({ssl, Socket}, Opts) ->
    ssl:setopts(Socket, Opts);
setopts(Socket, Opts) ->
    inet:setopts(Socket, Opts).

-spec type(Socket :: socket()) -> 
    socket_type().
type({ssl, _}) ->
    ssl;
type(_) ->
    plain.

