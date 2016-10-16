-module(relax_hex).
-author('erlangonrails@gmail.com').
-export([to_hex/1, 
         to_bin/1, 
         to_int/1, 
         dehex/1, 
         hexdigit/1]).

-type hex_char() :: char(). %% [$a-$f | $A - $F | $0 - $9]
-type hex_string() :: [hex_char()].

%% 将iolist/integer转换成16进制字符串,
%% 每个Byte转换成2个字符.
%%
%% 例如:
%% to_hex(<<0,255,1,255>>).
%% 返回: "00ff01ff
-spec to_hex(integer() | iolist()) -> hex_string().
to_hex(0) ->
    "0";
to_hex(I) when is_integer(I), I > 0 ->
    to_hex_int(I, []);
to_hex(B) ->
    to_hex(iolist_to_binary(B), []).


%% 将一个'合法的'hex string转换成binary().
%% (如果转换失败, 则抛出异常)
%%
%% 例如:
%% to_bin("00ff01ff"). 
%% <<0,255,1,255>>
-spec to_bin(hex_string()) -> binary().
to_bin(L) ->
    to_bin(L, []).

%% 将一个'合法的'hex string转换成integer().
%% (如果转换失败, 则抛出异常)
%%
%% 例如:
%% to_int("00ff01ff").
%% 16712191
%% to_int("ff").      
%% 255
-spec to_int(hex_string()) -> integer().
to_int(L) ->
    erlang:list_to_integer(L, 16).

%% 如果参数不是一个'合法的'hex char, 则抛出异常.
-spec dehex(hex_char()) -> integer().
dehex(C) when C >= $0, C =< $9 ->
    C - $0;
dehex(C) when C >= $a, C =< $f ->
    C - $a + 10;
dehex(C) when C >= $A, C =< $F ->
    C - $A + 10.

%% 参数的范围是0-15, 否则抛出异常.
-spec hexdigit(integer()) -> hex_char().
hexdigit(C) when C >= 0, C =< 9 ->
    C + $0;
hexdigit(C) when C =< 15 ->
    C + $a - 10.

%% Internal API

to_hex(<<>>, Acc) ->
    lists:reverse(Acc);
to_hex(<<C1:4, C2:4, Rest/binary>>, Acc) ->
    to_hex(Rest, [hexdigit(C2), hexdigit(C1) | Acc]).

to_hex_int(0, Acc) ->
    Acc;
to_hex_int(I, Acc) ->
    to_hex_int(I bsr 4, [hexdigit(I band 15) | Acc]).

to_bin([], Acc) ->
    iolist_to_binary(lists:reverse(Acc));
to_bin([C1, C2 | Rest], Acc) ->
    to_bin(Rest, [(dehex(C1) bsl 4) bor dehex(C2) | Acc]).

