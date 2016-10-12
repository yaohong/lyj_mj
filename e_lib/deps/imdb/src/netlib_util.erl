-module(netlib_util).
-author('erlangonrails@gmail.com').
-include("netlib.hrl").
-export([md5/1,
         get_uuid_algorithm/0,
         get_log/0,
         address_to_binary/1,
         address_to_atom/1,
         binary_to_hex/1]).

-spec md5(Data :: netlib_iolist()) -> 
          Digest::binary().
md5(Data) ->
    try crypto:hash(md5, Data) catch error:_ -> erlang:md5(Data) end.

-spec get_uuid_algorithm() ->
    netlib_uuid_algorithm().
get_uuid_algorithm() ->
    case application:get_env('netlib', 'uuid_algorithm') of
        {ok, 'random'} ->     'random';
        {ok, 'utc_random'} -> 'utc_random';
        {ok, 'sequential'} -> 'sequential';
        _ -> ?NETLIB_DEFAULT_UUID_ALGORITHM
    end.

-spec get_log() ->
    {LogPath :: netlib_string(),
     LogLevel :: netlib_log_level(),
     RotateInterval :: netlib_timeout()}.
get_log() ->
    case application:get_env('netlib', 'log') of
        undefined ->
            {?NETLIB_DEFAULT_LOG,
             ?NETLIB_DEFAULT_LOG_LEVEL};
        {ok, Proplists} ->
            {proplists:get_value(
               path, Proplists, ?NETLIB_DEFAULT_LOG),
             proplists:get_value(
               level, Proplists, ?NETLIB_DEFAULT_LOG_LEVEL),
             proplists:get_value(
               rotate_interval, Proplists, ?NETLIB_DEFAULT_LOG_ROTATE_INTERVAL)}
    end.

%% convert netlib_address() to netlib_binary()
-spec address_to_binary(Address :: netlib_address()) ->
    netlib_binary().
address_to_binary({{A, B, C, D}, Port}) ->
    IoList = io_lib:format("~p.~p.~p.~p:~p", 
                           [A, B, C, D, Port]),
    iolist_to_binary(IoList);
address_to_binary({{A, B, C, D, E, F, G, H}, Port}) ->
    IoList = io_lib:format("~p.~p.~p.~p.~p.~p.~p.~p:~p", 
                           [A, B, C, D, E, F, G, H, Port]),
    iolist_to_binary(IoList).

%% convert netlib_address() to netlib_atom()
-spec address_to_atom(Address :: netlib_address()) ->
    netlib_atom().
address_to_atom(Address) ->
    AddrBin = address_to_binary(Address),
    list_to_atom(binary_to_list(AddrBin)).

%% 将二进制字节流转换成16进制字符串,
%% 每个Byte转换成2个字符, 所以返回的数据的长度是参数长度的2倍.
%%
%% 例如:
%% netlib_util:to_hex(<<0,255,1,255>>).
%% 返回: "00ff01ff
-spec binary_to_hex(Data :: netlib_binary()) ->
    netlib_string().
binary_to_hex([]) ->
    [];
binary_to_hex(Bin) when is_binary(Bin) ->
    binary_to_hex(binary_to_list(Bin));
binary_to_hex([H|T]) ->
    [to_digit(H div 16),                     %% 处理一个Byte的高四位
     to_digit(H rem 16) | binary_to_hex(T)]. %% 处理一个Byte的低四位

to_digit(N) when N < 10 -> $0 + N;
to_digit(N)             -> $a + N-10.
    
