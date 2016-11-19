%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 十一月 2016 14:28
%%%-------------------------------------------------------------------
-module(hh_mj_util).
-author("yaohong").
-include("hh_mj.hrl").
-include("../qp_type.hrl").
-include("../../deps/file_log/include/file_log.hrl").
%% API
-export([generate_main_logic/1, print/1]).

%%生成hh_main_logic
generate_main_logic(Bin) ->
	<<
		PaiPool:120/binary, PoolHeadReadIndex:?BIG_UINT8, PoolTailReadIndex:?BIG_UINT8,
		Seat0:27/binary, Seat1:27/binary,Seat2:27/binary,Seat3:27/binary,
		BrankerNumber:?BIG_UINT8,SpecialData:10/binary,OldData:5/binary,
		NextData:4/binary,StateFlag:?BIG_UINT8,ErrorFlag:?BIG_UINT8,
		LastTime:?BIG_UINT32,ErrorLogData:516/binary
	>> = Bin,
	HeadLen = PoolHeadReadIndex,
	TailLen = ?HH_POOL_COUNT - 1 - PoolTailReadIndex,
	ValidPoolLen = ?HH_POOL_COUNT - HeadLen - TailLen,
	<<_:HeadLen/binary, ValidPool:ValidPoolLen/binary, _:TailLen/binary>> = PaiPool,
	ValidPoolList = binary_to_list(ValidPool),
	#hh_main_logic{
		pool = ValidPoolList,
		seat0 = generate_seat(Seat0),
		seat1 = generate_seat(Seat1),
		seat2 = generate_seat(Seat2),
		seat3 = generate_seat(Seat3),
		banker_seat_number = BrankerNumber,
		special = generage_special(SpecialData),
		old = generage_old(OldData),
		next = generage_next(NextData),
		state_flag = StateFlag,
		error_flag = ErrorFlag,
		last_time = LastTime ,
		error_log = generage_error_log(ErrorLogData)
	}.


%%根据二进制数据生成seat
generate_seat(Bin) ->
	<<
		C0:?BIG_UINT8, C1:?BIG_UINT8, C2:?BIG_UINT8, C3:?BIG_UINT8,
		P0:?BIG_UINT8, P1:?BIG_UINT8, P2:?BIG_UINT8, P3:?BIG_UINT8,
		G0:?BIG_UINT8, G1:?BIG_UINT8, G2:?BIG_UINT8, G3:?BIG_UINT8,
		PaiBin:14/binary, WriteIndex:?BIG_UINT8
	>> = Bin,
	Chi = generate_seat_pai(C0, C1, C2, C3),
	Peng = generate_seat_pai(P0, P1, P2, P3),
	Gang = generate_seat_pai(G0, G1, G2, G3),
	ValidPaiLen = WriteIndex,
	<<ValidPoolBin:ValidPaiLen/binary, _/binary>> = PaiBin,
	ValidPoolList = binary_to_list(ValidPoolBin),
	#hh_seat {
		chi = Chi,
		peng = Peng,
		gang = Gang,
		pai = ValidPoolList
	}.

generate_seat_pai(0, _, _, _) -> [];
generate_seat_pai(P1, 0, _, _) -> [P1];
generate_seat_pai(P1,P2, 0, _) -> [P1, P2];
generate_seat_pai(P1,P2, P3, 0) -> [P1, P2, P3];
generate_seat_pai(P1,P2, P3, P4) -> [P1, P2, P3, P4].


generage_special(SpecialData) ->
	<<
		Oper0SeatNum:?BIG_UINT8,Oper0Flag:?BIG_UINT8,
		Oper1SeatNum:?BIG_UINT8,Oper1Flag:?BIG_UINT8,
		Oper2SeatNum:?BIG_UINT8,Oper2Flag:?BIG_UINT8,
		OperCount:?BIG_UINT8, OperIndex:?BIG_UINT8,
		ChuPaiSeatNum:?BIG_INT8, ChuPaiValue:?BIG_UINT8
	>> = SpecialData,
    L1 = [{Oper0SeatNum, Oper0Flag},{Oper1SeatNum, Oper1Flag},{Oper2SeatNum, Oper2Flag}],
	VaildQueue = lists:split(OperCount, L1),
	#hh_special {
		oper_queue = VaildQueue,
		oper_index = OperIndex,
		chupai_seat_number = ChuPaiSeatNum,
		chupai_value = ChuPaiValue
	}.

generage_old(OldData) ->
	<<
		SeatNumber:?BIG_INT8,Flag:?BIG_UINT8,
		Type:?BIG_UINT8, Value1:?BIG_UINT8,
		Value2:?BIG_UINT8
	>> = OldData,
	#hh_old_oper{
		seat_number = SeatNumber,
		flag = Flag,
		type = Type,
		value1 = Value1,
		value2 = Value2
	}.

generage_next(NextData) ->
	<<
		SeatNumber:?BIG_INT8,Flag:?BIG_UINT8,
		Value1:?BIG_UINT8, Value2:?BIG_UINT8
	>> = NextData,
	#hh_next_oper{
		seat_number = SeatNumber,
		flag = Flag,
		value1 = Value1,
		value2 = Value2
	}.

str_pai(0) -> "";
str_pai(Pai)  -> str_pai(pai_type(Pai), pai_value(Pai)).

str_pai(1, V) -> integer_to_list(V) ++ "-wan";
str_pai(2, V) -> integer_to_list(V) ++ "-tiao";
str_pai(3, V) -> integer_to_list(V) ++ "-tong";
str_pai(4, 1) -> "dong-feng";
str_pai(4, 2) -> "nan-feng";
str_pai(4, 3) -> "xi-feng";
str_pai(4, 4) -> "bei-feng";
str_pai(5, 1) -> "hongzhong";
str_pai(5, 2) -> "facai";
str_pai(5, 3) -> "baiban".

str_oper(0) -> "none";
str_oper(1) -> "chi";
str_oper(2) -> "peng";
str_oper(4) -> "gang";
str_oper(8) -> "hu";
str_oper(16) -> "chu";
str_oper(32) -> "guo".

str_oper_flag(OperFlag) ->
	str_oper_flag([1,2,4,8,16,32], OperFlag, "").
str_oper_flag([], _, Str) -> Str;
str_oper_flag([Oper|T], OperFlag, Str) ->
	case Oper band OperFlag of
		1 -> str_oper_flag(T, OperFlag, Str ++"," ++ str_oper(Oper));
		0 -> str_oper_flag(T, OperFlag, Str)
	end.



pai_type(Pai) -> Pai bsl 4.
pai_value(Pai) -> Pai band 2#00001111.

generage_error_log(LogData) ->
	<<
		ErrorBuff:512/binary,LogLen:?BIG_INT32
	>> = LogData,
	#hh_error_log{
		log = lists:split(LogLen, ErrorBuff)
	}.



print(Logic) when is_record(Logic, hh_main_logic) ->
	?FILE_LOG_DEBUG("--------start print--------", []),
	?FILE_LOG_DEBUG("pai_pool:~p", [ [str_pai(Pai) || Pai <- Logic#hh_main_logic.pool] ]),
    ?FILE_LOG_DEBUG("seat0:", []),
	Seat0 = Logic#hh_main_logic.seat0,
	?FILE_LOG_DEBUG("chi:~p", [ [str_pai(Pai) || Pai <- Seat0#hh_seat.chi] ]),
	?FILE_LOG_DEBUG("peng:~p", [ [str_pai(Pai) || Pai <- Seat0#hh_seat.peng] ]),
	?FILE_LOG_DEBUG("gang:~p", [ [str_pai(Pai) || Pai <- Seat0#hh_seat.gang] ]),
	?FILE_LOG_DEBUG("pai:~p", [ [str_pai(Pai) || Pai <- Seat0#hh_seat.pai] ]),

	?FILE_LOG_DEBUG("seat1:", []),
	Seat1 = Logic#hh_main_logic.seat1,
	?FILE_LOG_DEBUG("chi:~p", [ [str_pai(Pai) || Pai <- Seat1#hh_seat.chi] ]),
	?FILE_LOG_DEBUG("peng:~p", [ [str_pai(Pai) || Pai <- Seat1#hh_seat.peng] ]),
	?FILE_LOG_DEBUG("gang:~p", [ [str_pai(Pai) || Pai <- Seat1#hh_seat.gang] ]),
	?FILE_LOG_DEBUG("pai:~p", [ [str_pai(Pai) || Pai <- Seat1#hh_seat.pai] ]),

	?FILE_LOG_DEBUG("seat2:", []),
	Seat2 = Logic#hh_main_logic.seat2,
	?FILE_LOG_DEBUG("chi:~p", [ [str_pai(Pai) || Pai <- Seat2#hh_seat.chi] ]),
	?FILE_LOG_DEBUG("peng:~p", [ [str_pai(Pai) || Pai <- Seat2#hh_seat.peng] ]),
	?FILE_LOG_DEBUG("gang:~p", [ [str_pai(Pai) || Pai <- Seat2#hh_seat.gang] ]),
	?FILE_LOG_DEBUG("pai:~p", [ [str_pai(Pai) || Pai <- Seat2#hh_seat.pai] ]),

	?FILE_LOG_DEBUG("seat3:", []),
	Seat3 = Logic#hh_main_logic.seat3,
	?FILE_LOG_DEBUG("chi:~p", [ [str_pai(Pai) || Pai <- Seat3#hh_seat.chi] ]),
	?FILE_LOG_DEBUG("peng:~p", [ [str_pai(Pai) || Pai <- Seat3#hh_seat.peng] ]),
	?FILE_LOG_DEBUG("gang:~p", [ [str_pai(Pai) || Pai <- Seat3#hh_seat.gang] ]),
	?FILE_LOG_DEBUG("pai:~p", [ [str_pai(Pai) || Pai <- Seat3#hh_seat.pai] ]),


    ?FILE_LOG_DEBUG("banker_seat_number:~p", [Logic#hh_main_logic.banker_seat_number]),

	?FILE_LOG_DEBUG("special:", []),
	Special = Logic#hh_main_logic.special,
    ?FILE_LOG_DEBUG("oper_queue:~p", [ [{SeatNumber, str_oper_flag(OperFlag)} || {SeatNumber, OperFlag} <- Special#hh_special.oper_queue] ]),
	?FILE_LOG_DEBUG("oper_index:~p", [Special#hh_special.oper_index]),
    ?FILE_LOG_DEBUG("chupai_seatnumber:~p", [Special#hh_special.chupai_seat_number]),
	?FILE_LOG_DEBUG("chupai_value:~p", [Special#hh_special.chupai_value]),

    ?FILE_LOG_DEBUG("old:", []),
	Old = Logic#hh_main_logic.old,
	?FILE_LOG_DEBUG("seat_number:~p", [Old#hh_old_oper.seat_number]),
	?FILE_LOG_DEBUG("flag:~p", [str_oper_flag(Old#hh_old_oper.flag)]),
	?FILE_LOG_DEBUG("type:~p", [str_oper(Old#hh_old_oper.type)]),
	?FILE_LOG_DEBUG("value1:~p", [str_pai(Old#hh_old_oper.value1)]),
	?FILE_LOG_DEBUG("value2:~p", [str_pai(Old#hh_old_oper.value2)]),

    ?FILE_LOG_DEBUG("next:", []),
	Next = Logic#hh_main_logic.next,
	?FILE_LOG_DEBUG("seat_number:~p", [Next#hh_next_oper.seat_number]),
	?FILE_LOG_DEBUG("flag:~p", [str_oper_flag(Next#hh_next_oper.flag)]),
	?FILE_LOG_DEBUG("value1:~p", [str_pai(Next#hh_next_oper.value1)]),
	?FILE_LOG_DEBUG("value2:~p", [str_pai(Next#hh_next_oper.value2)]),

	?FILE_LOG_DEBUG("state_flag:~p", [Logic#hh_main_logic.state_flag]),
	?FILE_LOG_DEBUG("error_flag:~p", [Logic#hh_main_logic.error_flag]),
	?FILE_LOG_DEBUG("last_time:~p", [Logic#hh_main_logic.last_time]),
	?FILE_LOG_DEBUG("error_log:~p", [Logic#hh_main_logic.error_log#hh_error_log.log]),
	ok.



