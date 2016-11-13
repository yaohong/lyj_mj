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
-export([generate_main_logic/1]).

%%生成hh_main_logic
generate_main_logic(Bin) ->
	<<
		PaiPool:120/binary, PoolHeadReadIndex:?BIG_UINT8, PoolTailReadIndex:?BIG_UINT8,
		Seat0:28/binary, Seat1:28/binary,Seat2:28/binary,Seat3:28/binary,
		BrankerNumber:?BIG_UINT8, CurrentSeatNumber:?BIG_UINT8, LastTime:?BIG_UINT32
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
		current_seat_number = CurrentSeatNumber
	}.


%%根据二进制数据生成seat
generate_seat(Bin) ->
	<<
		C0:?BIG_UINT8, C1:?BIG_UINT8, C2:?BIG_UINT8, C3:?BIG_UINT8,
		P0:?BIG_UINT8, P1:?BIG_UINT8, P2:?BIG_UINT8, P3:?BIG_UINT8,
		G0:?BIG_UINT8, G1:?BIG_UINT8, G2:?BIG_UINT8, G3:?BIG_UINT8,
		PaiBin:14/binary, WriteIndex:?BIG_UINT8, OperFlag:?BIG_UINT8
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
		pai = ValidPoolList,
		oper_flag  = OperFlag
	}.

generate_seat_pai(0, _, _, _) -> [];
generate_seat_pai(P1, 0, _, _) -> [P1];
generate_seat_pai(P1,P2, 0, _) -> [P1, P2];
generate_seat_pai(P1,P2, P3, 0) -> [P1, P2, P3];
generate_seat_pai(P1,P2, P3, P4) -> [P1, P2, P3, P4].

