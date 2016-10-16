-module(protobuffs).
-author('erlangonrails@gmail.com').
-include("protobuffs.hrl").
-export([start/0, 
         stop/0]).

-export([encode/3, 
         decode/2, 
         decode_value/3]).

-export([encode_field_tag/2,
         read_field_num_and_wire_type/1, 
         encode_varint/1,
         decode_varint/1]).


%% FieldType, 也称为WireType, 在序列化的时候会用下面的方式和FieldID编码到一起
%%
%% 编码方式:
%% (FieldID << 3) | FiledType
-define(TYPE_VARINT, 0).
-define(TYPE_64BIT, 1).
-define(TYPE_STRING, 2).
-define(TYPE_START_GROUP, 3).
-define(TYPE_END_GROUP, 4).
-define(TYPE_32BIT, 5).

%% 1. Wire Type
%% --------------------------------------------------------------------------------------
%% | Type | Meaning          | Used for                                                 |
%% --------------------------------------------------------------------------------------
%% | 0    | Varint           | int32, int64, uint32, uint64, sint32, sint64, bool, enum |
%% --------------------------------------------------------------------------------------
%% | 1    | 64-bit           | fixed64, sfixed64, double                                |
%% --------------------------------------------------------------------------------------
%% | 2    | Length-delimited | string, bytes, embedded messages, packed repeated fields |
%% --------------------------------------------------------------------------------------
%% | 3    | Start group      | groups (deprecated)                                      |
%% --------------------------------------------------------------------------------------
%% | 4    | End group        | groups (deprecated)                                      |
%% --------------------------------------------------------------------------------------
%% | 5    | 32-bit           | fixed32, sfixed32, float                                 |
%% --------------------------------------------------------------------------------------
%%
%% 2. Varint
%% 每个字节的第1位称为msb, 如果为1表示后续还有数据, 为0表示结束,
%% 将所有的字节去掉msb位倒序合并后就表示其真实数据.
%%
%% 例子1:
%% 300 - <<172,2>>
%% 1010 1100 0000 0010
%%  010 1100  000 0010 
%%  000 0010 + 010 1100 -> 100101100 -> 256 + 32 + 8 + 4 = 300
%%
%% 例子2:
%% 150 - <<150,1>>
%% 1001 0110  0000 0001
%%  001 0110   000 0001
%%  000 0001 + 001 0110 -> 10010110 -> 128 + 16 +  4 + 2 = 150
%%
%% 3. sint32和sint64类型的编码(ZigZag)
%% sint32和sint64类型的编码采用ZigZag编码方式, 最后一位表示正负情况.
%%
%% 可以用下面方式运算:
%% sint32 -> (n << 1) ^ (n >> 31)
%% sint64 -> (n << 1) ^ (n >> 63)
%%
%% 例子:
%% ------------------------------
%% | 原始值       | 编码为      |
%% ------------------------------
%% | 0            | 0           |
%% ------------------------------
%% | -1           | 1           |
%% ------------------------------
%% | 1            | 2           |
%% ------------------------------
%% | -2           | 3           |
%% ------------------------------
%% | 2147483647   | 4294967294  |
%% ------------------------------
%% | -2147483648  | 4294967295  |
%% ------------------------------
%%
%% 4. string, bytes
%% 都使用varint的长度 + 具体内容的编码方式
%%
%% 例子:
%% message Test {
%%   required string b = 2;
%% }
%% 18,7,116,101,115,116,105,110,103
%%
%% 18 - FieldID & WireType的组合
%% 7  - varint长度
%% 116,101,115,116,105,110,103 - testing
%%
-spec start() -> 
    ok | {error, term()}.
start() ->
    application:start(protobuffs).

-spec stop() -> 
    ok | {error, term()}.
stop() ->
    application:stop(protobuffs).

%% Encode an Erlang data structure into a Protocol Buffers value.
-spec encode(
        FieldID :: integer(), 
        Value :: term(), 
        Type :: proto_type()) ->
    binary().
encode(FieldID, Value, Type) ->
    iolist_to_binary(encode_internal(FieldID, Value, Type)).
    
encode_internal(FieldID, false, bool) ->
    encode_internal(FieldID, 0, int32);
encode_internal(FieldID, true, bool) ->
    encode_internal(FieldID, 1, int32);
encode_internal(FieldID, Integer, enum) ->
    encode_internal(FieldID, Integer, uint32);
encode_internal(FieldID, Integer, int32) when Integer >= -16#80000000, Integer < 0 ->
    encode_internal(FieldID, Integer, int64);
encode_internal(FieldID, Integer, int64) when Integer >= -16#8000000000000000, Integer < 0 ->
    encode_internal(FieldID, Integer + (1 bsl 64), uint64);
encode_internal(FieldID, Integer, int32) when Integer >= -16#80000000, Integer =< 16#7fffffff ->
    encode_varint_field(FieldID, Integer);
encode_internal(FieldID, Integer, uint32) when Integer band 16#ffffffff =:= Integer ->
    encode_varint_field(FieldID, Integer);    
encode_internal(FieldID, Integer, int64) when Integer >= -16#8000000000000000, Integer =< 16#7fffffffffffffff ->
    encode_varint_field(FieldID, Integer);
encode_internal(FieldID, Integer, uint64) when Integer band 16#ffffffffffffffff =:= Integer ->
    encode_varint_field(FieldID, Integer);
encode_internal(FieldID, Integer, bool) when Integer band 1 =:= 1 ->
    encode_varint_field(FieldID, Integer);
encode_internal(FieldID, Integer, sint32) when Integer >= -16#80000000, Integer < 0 ->
    encode_varint_field(FieldID, bnot (Integer bsl 1));
encode_internal(FieldID, Integer, sint64) when Integer >= -16#8000000000000000, Integer < 0 ->
    encode_varint_field(FieldID, bnot (Integer bsl 1));
encode_internal(FieldID, Integer, sint32) when Integer >= 0, Integer =< 16#7fffffff ->
    encode_varint_field(FieldID, Integer bsl 1);
encode_internal(FieldID, Integer, sint64) when Integer >= 0, Integer =< 16#7fffffffffffffff ->
    encode_varint_field(FieldID, Integer bsl 1);
encode_internal(FieldID, Integer, fixed32) when Integer band 16#ffffffff =:= Integer ->
    [encode_field_tag(FieldID, ?TYPE_32BIT), <<Integer:32/little-integer>>];
encode_internal(FieldID, Integer, sfixed32) when Integer >= -16#80000000, Integer =< 16#7fffffff ->
    [encode_field_tag(FieldID, ?TYPE_32BIT), <<Integer:32/little-integer>>];
encode_internal(FieldID, Integer, fixed64) when Integer band 16#ffffffffffffffff =:= Integer ->
    [encode_field_tag(FieldID, ?TYPE_64BIT), <<Integer:64/little-integer>>];
encode_internal(FieldID, Integer, sfixed64) when Integer >= -16#8000000000000000, Integer =< 16#7fffffffffffffff ->
    [encode_field_tag(FieldID, ?TYPE_64BIT), <<Integer:64/little-integer>>];
encode_internal(FieldID, String, string) when is_list(String) ->
    encode_internal(FieldID, iolist_to_binary(String), string);
encode_internal(FieldID, String, string) when is_binary(String) ->
    encode_internal(FieldID, String, bytes);
encode_internal(FieldID, String, bytes) when is_list(String) ->
    encode_internal(FieldID, iolist_to_binary(String), bytes);
encode_internal(FieldID, Bytes, bytes) when is_binary(Bytes) ->
    [encode_field_tag(FieldID, ?TYPE_STRING), encode_varint(size(Bytes)), Bytes];
encode_internal(FieldID, String, bytes) when is_list(String) ->
    encode_internal(FieldID, iolist_to_binary(String), bytes);
encode_internal(FieldID, Float, float) when is_integer(Float) ->
    encode_internal(FieldID, Float + 0.0, float);
encode_internal(FieldID, Float, float) when is_float(Float) ->
    [encode_field_tag(FieldID, ?TYPE_32BIT), <<Float:32/little-float>>];
encode_internal(FieldID, Float, double) when is_integer(Float) ->
    encode_internal(FieldID, Float + 0.0, double);
encode_internal(FieldID, Float, double) when is_float(Float) ->
    [encode_field_tag(FieldID, ?TYPE_64BIT), <<Float:64/little-float>>].

read_field_num_and_wire_type(Bytes) ->
    {Tag, Rest} = decode_varint(Bytes),
    FieldID = Tag bsr 3,
    WireType = Tag band 7,
    {{FieldID, WireType}, Rest}.
 
   
-spec decode(
        Bytes :: binary(),
        ExpectedType :: proto_type()) ->
    {{FieldID :: integer(), Val :: term()}, Rest :: binary()}.
decode(Bytes, ExpectedType) ->
    {{FieldID, WireType}, Rest} = read_field_num_and_wire_type(Bytes),
    {Value, Rest1} = decode_value(Rest, WireType, ExpectedType),
    {{FieldID, Value}, Rest1}.

decode_value(Bytes, ?TYPE_VARINT, ExpectedType) ->
    {Value, Rest} = decode_varint(Bytes),
    {typecast(Value, ExpectedType), Rest};
decode_value(Bytes, ?TYPE_STRING, ExpectedType) when ExpectedType =:= string; ExpectedType =:= bytes ->
    {Length, Rest} = decode_varint(Bytes),
    split_binary(Rest, Length);
decode_value(<<Value:64/little-unsigned-integer, Rest/binary>>, ?TYPE_64BIT, fixed64) ->
    {Value, Rest};
decode_value(<<Value:32/little-unsigned-integer, _:32, Rest/binary>>, ?TYPE_64BIT, fixed32) ->
    {Value, Rest};
decode_value(<<Value:64/little-signed-integer, Rest/binary>>, ?TYPE_64BIT, sfixed64) ->
    {Value, Rest};
decode_value(<<Value:32/little-signed-integer, _:32, Rest/binary>>, ?TYPE_64BIT, sfixed32) ->
    {Value, Rest};
decode_value(<<Value:32/little-unsigned-integer, Rest/binary>>, ?TYPE_32BIT, Type) when Type =:= fixed32; Type =:= fixed64 ->
    {Value, Rest};
decode_value(<<Value:32/little-signed-integer, Rest/binary>>, ?TYPE_32BIT, Type) when Type =:= sfixed32; Type =:= sfixed64 ->
    {Value, Rest};
decode_value(<<Value:32/little-float, Rest/binary>>, ?TYPE_32BIT, float) ->
    {Value + 0.0, Rest};
decode_value(<<Value:64/little-float, Rest/binary>>, ?TYPE_64BIT, double) ->
    {Value + 0.0, Rest};
decode_value(_, WireType, ExpectedType) ->
    exit({error, {unexpected_value, WireType, ExpectedType}}).

typecast(Value, SignedType) when SignedType =:= int32; SignedType =:= int64 ->
    if
        Value band 16#8000000000000000 =/= 0 -> Value - 16#10000000000000000;
        true -> Value
    end;
typecast(Value, SignedType) when SignedType =:= sint32; SignedType =:= sint64 ->
    (Value bsr 1) bxor (-(Value band 1));
typecast(Value, Type) when Type =:= bool ->
    case Value of
        1 -> true;
        _ -> false
    end;
typecast(Value, _) ->
    Value.

%% 编码方式:
%% (FieldID << 3) | FiledType
-spec encode_field_tag(
        FieldID :: integer(), 
        FieldType :: integer()) ->
    binary().
encode_field_tag(FieldID, FieldType) when FieldID band 16#3fffffff =:= FieldID ->
    encode_varint((FieldID bsl 3) bor FieldType).

-spec encode_varint_field(
        FieldID :: integer(),
        Integer :: integer()) ->
    iodata().
encode_varint_field(FieldID, Integer) ->
    [encode_field_tag(FieldID, ?TYPE_VARINT), encode_varint(Integer)].


-spec encode_varint(I :: integer()) ->
    binary().
encode_varint(I) ->
    encode_varint(I, []).

encode_varint(I, Acc) when I =< 16#7f ->
    iolist_to_binary(lists:reverse([I | Acc]));
encode_varint(I, Acc) ->
    Last_Seven_Bits = (I - ((I bsr 7) bsl 7)),
    First_X_Bits = (I bsr 7),
    With_Leading_Bit = Last_Seven_Bits bor 16#80,
    encode_varint(First_X_Bits, [With_Leading_Bit|Acc]).


-spec decode_varint(Bytes :: binary()) ->
    {I :: integer(), Rest :: binary()}.
decode_varint(Bytes) ->
    decode_varint(Bytes, []).
decode_varint(<<0:1, I:7, Rest/binary>>, Acc) ->
    Acc1 = [I | Acc],
    Result = 
        lists:foldl(
            fun(X, Acc0) ->
                (Acc0 bsl 7 bor X)
            end, 0, Acc1),
    {Result, Rest};
decode_varint(<<1:1, I:7, Rest/binary>>, Acc) ->
    decode_varint(Rest, [I | Acc]).


