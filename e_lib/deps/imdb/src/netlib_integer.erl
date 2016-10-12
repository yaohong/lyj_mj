-module(netlib_integer).
-author('erlangonrails@gmail.com').
-include("netlib.hrl").
-export([encode_varint/1,
         decode_varint/1]).

-spec encode_varint(I :: netlib_non_neg_integer()) -> 
    netlib_binary().
encode_varint(I) ->
    encode_varint(I, []).

encode_varint(I, Acc) when I =< 16#7f ->
    iolist_to_binary(lists:reverse([I | Acc]));
encode_varint(I, Acc) ->
    Last_Seven_Bits = (I - ((I bsr 7) bsl 7)), 
    First_X_Bits = (I bsr 7),
    With_Leading_Bit = Last_Seven_Bits bor 16#80,
    encode_varint(First_X_Bits, [With_Leading_Bit|Acc]).

%% 发生错误抛出异常
-spec decode_varint(Bytes :: netlib_binary()) -> 
    {Ret :: netlib_non_neg_integer(), Rest :: netlib_binary()}.
decode_varint(Bytes) ->    
    decode_varint(Bytes, []).

decode_varint(<<0:1, I:7, Rest/binary>>, Acc) ->
    Acc1 = [I|Acc],
    Result = 
        lists:foldl(
            fun(X, Acc0) ->
                (Acc0 bsl 7 bor X)
            end, 0, Acc1),
    {Result, Rest};
decode_varint(<<1:1, I:7, Rest/binary>>, Acc) ->
    decode_varint(Rest, [I | Acc]).
