%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 十一月 2016 16:24
%%%-------------------------------------------------------------------
-module(hh_mj).
-author("yaohong").
-include("hh_mj.hrl").
-include("../../include/mj_pb.hrl").
-include("../../deps/file_log/include/file_log.hrl").
-export([test_game_start/0, test_game_oper/4]).
%% API
-export([
	init_private_data/0,
	game_start/1,
	game_oper/3,
	game_quit/2]).

-define(GAME_TYPE, 0).

init_private_data() ->
	undefined.

game_start(undefined) ->
	{success, GameBin} = mj_nif:game_start(?GAME_TYPE, -1, qp_util:timestamp()),
	game_start1(GameBin);
game_start(OldGameBin) when is_binary(OldGameBin) ->
	Logic = hh_mj_util:generate_main_logic(OldGameBin),
	BankerSeatNumber =
		if
			Logic#hh_main_logic.state_flag =:= 2->
				Result = Logic#hh_main_logic.hupai_result,
				?FILE_LOG_DEBUG("game_start, old game_end state_flag=2, banker_seat_number=~p", [Result#hh_hupai_result.seat_number]),
				Result#hh_hupai_result.seat_number;
			true ->
				?FILE_LOG_DEBUG("game_start, old game_end state_flag=~p, banker_seat_number=~p", [Logic#hh_main_logic.state_flag, Logic#hh_main_logic.banker_seat_number]),
				Logic#hh_main_logic.banker_seat_number
		end,
	{success, GameBin} = mj_nif:game_start(?GAME_TYPE, BankerSeatNumber, qp_util:timestamp()),
	game_start1(GameBin).

game_start1(GameBin) when is_binary(GameBin) ->
	Logic = hh_mj_util:generate_main_logic(GameBin),
	Seat0 = Logic#hh_main_logic.seat0,
	Seat1 = Logic#hh_main_logic.seat1,
	Seat2 = Logic#hh_main_logic.seat2,
	Seat3 = Logic#hh_main_logic.seat3,
	SendSeatData = encode_seat_data(Logic, [{0, Seat0},{1, Seat1},{2, Seat2},{3, Seat3}]),
	{continue, {GameBin, {<<>>, SendSeatData, <<>>}}}.

encode_seat_data(BankerSeatNum, SeatList) ->
	encode_seat_data(BankerSeatNum, SeatList, []).

encode_seat_data(_, [], Out) -> Out;
encode_seat_data(Logic, [{SeatNum, SeatData}|T], Out) ->
	Rsp =
		if
			Logic#hh_main_logic.banker_seat_number =:= SeatNum ->
				#qp_mj_game_start_notify{
					pai = SeatData#hh_seat.pai,
					banker_seat_number = Logic#hh_main_logic.banker_seat_number,
					oper_flag = Logic#hh_main_logic.next#hh_next_oper.flag};
			true ->
				#qp_mj_game_start_notify{
					pai = SeatData#hh_seat.pai,
					banker_seat_number = Logic#hh_main_logic.banker_seat_number}
		end,

	Bin = hh_mj_proto:encode_packet(Rsp),
	encode_seat_data(Logic, T, [{SeatNum, Bin}|Out]).



game_oper(GameBin, SeatNum, OperBin) when is_binary(GameBin) andalso is_integer(SeatNum) andalso is_binary(OperBin) ->
	#qp_mj_oper_req{type = Type, v1 = V1, v2 = V2} = hh_mj_proto:decode_packet(OperBin),
	?FILE_LOG_DEBUG("game_data seat_num=~p, type=~p, v1=~p, v2=~p", [SeatNum, Type, V1, V2]),
	{success} = mj_nif:game_oper(GameBin, ?GAME_TYPE, SeatNum, Type ,undefine_transform(V1), undefine_transform(V2)),
	Logic = hh_mj_util:generate_main_logic(GameBin),
	Result = Logic#hh_main_logic.hupai_result,
	Next = Logic#hh_main_logic.next,
	hh_mj_util:print(Logic),
	if
		Logic#hh_main_logic.error_flag =:= 1 ->
			%%出错了
			ErrorRsp =
				#qp_mj_oper_error {
					error_type = Type,
					error_value1 = V1,
					error_value2 = V2,

					oper_flag = Next#hh_next_oper.flag,
					oper_value1 = Next#hh_next_oper.value1,
					oper_value2 = Next#hh_next_oper.value2
				},
			ErrorBin = hh_mj_proto:encode_packet(ErrorRsp),
			{continue, {GameBin, {<<>>, [{SeatNum, ErrorBin}], <<>>}}};
		true ->
			if
				Logic#hh_main_logic.state_flag =:= 0 ->
					Old = Logic#hh_main_logic.old,
					Notify1 =
						#qp_mj_oper_notify{
							seat_numer = Old#hh_old_oper.seat_number,
							type = Old#hh_old_oper.type,
							v1 = Old#hh_old_oper.value1,
							v2 = Old#hh_old_oper.value2,
							v3 = Logic#hh_main_logic.chupai_seatnumber
						},
					SendSeatData = encode_oper_seat_data(Next, Notify1, [0,1,2,3], []),
					{continue, {GameBin, {<<>>, SendSeatData, <<>>}}};
				Logic#hh_main_logic.state_flag =:= 1 ->
					?FILE_LOG_DEBUG("state_flag=1 game_end.", []),
					%%穿掉了
					GameValueList = compute_result(Logic),
					QpEndResult1 =
						#qp_mj_game_end_notify {
							end_type = 1,
							hupai_seatnumber = -1,
							hupai_value = 0,
							hupai_type = 0,
							fangpao_seatnumber = -1,
							seat0_pai = Logic#hh_main_logic.seat0#hh_seat.pai,
							seat1_pai = Logic#hh_main_logic.seat1#hh_seat.pai,
							seat2_pai = Logic#hh_main_logic.seat2#hh_seat.pai,
							seat3_pai = Logic#hh_main_logic.seat3#hh_seat.pai
						},
					HuPaiResultBin1 = hh_mj_proto:encode_packet(QpEndResult1),
					{game_end, {GameBin, HuPaiResultBin1}};
				Logic#hh_main_logic.state_flag =:= 2 ->
					%%游戏结束了
					?FILE_LOG_DEBUG("state_flag=2 game_end,result=~p.", [Result]),
					GameValueList = compute_result(Logic),
					%%计算分数
					QpEndResult2 =
						#qp_mj_game_end_notify {
							end_type = 2,
							hupai_seatnumber = Result#hh_hupai_result.seat_number,
							hupai_value = Result#hh_hupai_result.value,
							hupai_type = Result#hh_hupai_result.type,
							fangpao_seatnumber = Result#hh_hupai_result.fangpao_set_number,
							seat0_pai = Logic#hh_main_logic.seat0#hh_seat.pai,
							seat1_pai = Logic#hh_main_logic.seat1#hh_seat.pai,
							seat2_pai = Logic#hh_main_logic.seat2#hh_seat.pai,
							seat3_pai = Logic#hh_main_logic.seat3#hh_seat.pai
						},
					HuPaiResultBin2 = hh_mj_proto:encode_packet(QpEndResult2),
					{game_end, {GameBin, HuPaiResultBin2}}
			end
	end.

game_quit(GameBin, SeatNum) when is_binary(GameBin) andalso is_integer(SeatNum) ->
	Logic = hh_mj_util:generate_main_logic(GameBin),
	QpEndResult1 =
		#qp_mj_game_end_notify {
			end_type = 0,
			hupai_seatnumber = SeatNum,
			hupai_value = 0,
			hupai_type = 0,
			fangpao_seatnumber = -1,
			seat0_pai = Logic#hh_main_logic.seat0#hh_seat.pai,
			seat1_pai = Logic#hh_main_logic.seat1#hh_seat.pai,
			seat2_pai = Logic#hh_main_logic.seat2#hh_seat.pai,
			seat3_pai = Logic#hh_main_logic.seat3#hh_seat.pai
		},
	HuPaiResultBin = hh_mj_proto:encode_packet(QpEndResult1),
	{HuPaiResultBin, undefined}.


encode_oper_seat_data(_Next, _Notify, [], Out) -> Out;
encode_oper_seat_data(Next, Notify, [SeatNum|T], Out) when Next#hh_next_oper.seat_number =:= SeatNum ->
	NewNotify = Notify#qp_mj_oper_notify{
		next_oper_seat_num = Next#hh_next_oper.seat_number,
		next_oper_flag = Next#hh_next_oper.flag,
		next_oper_value1 = Next#hh_next_oper.value1,
		next_oper_value2 = Next#hh_next_oper.value2},
	Bin = hh_mj_proto:encode_packet(NewNotify),
	encode_oper_seat_data(Next, Notify, T, [{SeatNum, Bin}|Out]);
encode_oper_seat_data(Next, Notify, [SeatNum|T], Out) ->
	Bin = hh_mj_proto:encode_packet(Notify),
	encode_oper_seat_data(Next, Notify, T, [{SeatNum, Bin}|Out]).


undefine_transform(undefined) -> 0;
undefine_transform(V) -> V.


%%计算分数
compute_result(Logic) when is_record(Logic, hh_main_logic) ->
	T = gb_trees:empty(),
	T0 = gb_trees:insert(0, 0, T),
	T1 = gb_trees:insert(1, 0, T0),
	T2 = gb_trees:insert(2, 0, T1),
	T3 = gb_trees:insert(3, 0, T2),
	T4 = compute_gang_result(Logic, [0, 1, 2, 3], T3),
	%%计算胡的分数
	HuResult = Logic#hh_main_logic.hupai_result,
	if
		Logic#hh_main_logic.state_flag =:= 1 -> gb_trees:to_list(T4);
		Logic#hh_main_logic.state_flag =:= 2 ->
			HuSeatNumber = HuResult#hh_hupai_result.seat_number,
			HuValue = get_hu_value(HuResult#hh_hupai_result.level),
			if
				HuResult#hh_hupai_result.type =:= 0 ->
					%%自摸,三家给分
					%%自己先加分
					T5 = otherseat_value_change([HuSeatNumber], 3 * HuValue, T4),
					%%另外三家减分
					T6 = otherseat_value_change(other_seatnumber_list(HuSeatNumber), 0 - HuValue, T5),
					gb_trees:to_list(T6);
				HuResult#hh_hupai_result.type =:= 1 ->
					%%放炮,胡牌的加分
					T5 = otherseat_value_change([HuSeatNumber], HuValue, T4),
					%%放炮的减分
					T6 = otherseat_value_change([HuResult#hh_hupai_result.fangpao_set_number], 0 - HuValue, T5),
					gb_trees:to_list(T6)
			end
	end.

compute_gang_result(_Logic, [], Tree) -> Tree;
compute_gang_result(Logic, [SeatNumber|T], Tree) ->
	SeatData = seatnumber_to_seatdata(SeatNumber, Logic),
	NewTree =
		lists:foldr(
			fun(GangItem, TmpTree) ->
				if
					GangItem#hh_seat_gang.type =:= 0 ->
						%%明杠
						TmpTree1 = otherseat_value_change([SeatNumber], 4, TmpTree),		%%杠牌的+4分
						otherseat_value_change([GangItem#hh_seat_gang.seat_number], - 4, TmpTree1); %%放杠的-4分
					GangItem#hh_seat_gang.type =:= 1 ->
						%%补杠
						%%另外三家每家-2分
						%%自己加6分
						TmpTree1 = otherseat_value_change([SeatNumber], 6, TmpTree),
						otherseat_value_change(other_seatnumber_list(SeatNumber), -2, TmpTree1);
					GangItem#hh_seat_gang.type =:= 2 ->
						%%暗杠
						%%另外三家每家-4分
						%%自己加12分
						TmpTree1 = otherseat_value_change([SeatNumber], 12, TmpTree),
						otherseat_value_change(other_seatnumber_list(SeatNumber), -4, TmpTree1)
				end
			end, Tree, SeatData#hh_seat.gang),
	compute_gang_result(Logic, T, NewTree).

seatnumber_to_seatdata(0, Logic) ->Logic#hh_main_logic.seat0;
seatnumber_to_seatdata(1, Logic) ->Logic#hh_main_logic.seat1;
seatnumber_to_seatdata(2, Logic) ->Logic#hh_main_logic.seat2;
seatnumber_to_seatdata(3, Logic) ->Logic#hh_main_logic.seat3.

other_seatnumber_list(0) -> [1,2,3];
other_seatnumber_list(1) -> [0,2,3];
other_seatnumber_list(2) -> [0,1,3];
other_seatnumber_list(3) -> [0,1,2].

otherseat_value_change(SeatNumberList, ChangeValue, Tree) ->
	lists:foldr(
		fun(SeatNumber, TmpTree) ->
			OldValue = gb_trees:get(SeatNumber, TmpTree),
			gb_trees:update(SeatNumber, OldValue + ChangeValue, TmpTree)
		end, Tree, SeatNumberList).

get_hu_value(1) -> 2;			%%屁胡
get_hu_value(2) -> 4;			%%刀胡
get_hu_value(3) -> 8;			%%七对
get_hu_value(4) -> 16;			%%豪华七对
get_hu_value(5) -> 32.			%%超豪华七对






test_game_start() ->
	{success, GameBin} = mj_nif:game_start(?GAME_TYPE, 0, qp_util:timestamp()),
	Logic = hh_mj_util:generate_main_logic(GameBin),
	?FILE_LOG_DEBUG("logic=~p", [Logic]),
	hh_mj_util:print(Logic),
	put(game_bin, GameBin).


test_game_oper(SeatNumber, Type, V1, V2) ->
	GameBin = get(game_bin),
	true = GameBin =/= undefined,
	mj_nif:game_oper(GameBin, ?GAME_TYPE, SeatNumber, Type, V1, V2),
	Logic = hh_mj_util:generate_main_logic(GameBin),
	hh_mj_util:print(Logic).
