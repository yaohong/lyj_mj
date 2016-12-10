%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 十一月 2016 14:21
%%%-------------------------------------------------------------------
-author("yaohong").
-ifndef(_hh_mj_h__).
-define(_hh_mj_h__, 0).

-record(hh_seat_chi, {pai :: integer(), type :: integer()}).
-record(hh_seat_peng, {pai :: integer(), seat_number :: integer()}).
-record(hh_seat_gang, {pai :: integer(), type :: integer(), seat_number :: integer()}).

-record(hh_seat, {
	chi :: [#hh_seat_chi{}],				%%吃的牌
	peng :: [#hh_seat_peng{}],			%%碰的牌
	gang :: [#hh_seat_gang{}],			%%杠的牌
	pai :: [integer()]				%%自己手里的牌
}).


%%特殊操作相关的数据
-record(hh_special, {
	oper_queue :: [{SeatNum :: integer(), OperFlag :: integer()}],
	oper_index :: integer()
}).


-record(hh_hu, {
	queue :: [integer()],
	index :: integer()
}).

-record(hh_old_oper, {
	seat_number :: integer(),
	flag :: integer(),
	type :: integer(),
	value1 :: integer(),
	value2 :: integer()
}).

-record(hh_next_oper, {
	seat_number :: integer(),
	flag :: integer(),
	value1 :: integer(),
	value2 :: integer()
}).

-record(hh_hupai_result, {
	seat_number :: integer(),
	value :: integer(),
	type :: integer(),
	level :: integer(),
	fangpao_set_number :: integer()
}).

-record(hh_error_log, {
	log :: list()
}).

-record(hh_main_logic, {
	pool :: [integer()],				%%牌池里面的牌
	seat0 :: #hh_seat{},				%%0号座位的数据
	seat1 :: #hh_seat{},				%%1号座位的数据
	seat2 :: #hh_seat{},				%%2号座位的数据
	seat3 :: #hh_seat{},				%%3号座位的数据
	banker_seat_number :: integer(),	%%庄家的座位号
	special :: #hh_special{},   		%%
	hu :: #hh_hu{},
	chupai_seatnumber :: integer(),
	chupai_value :: integer(),
	old :: #hh_old_oper{},
	next :: #hh_next_oper{},
	state_flag :: integer(),
	error_flag :: integer(),
	hupai_result :: #hh_hupai_result{},
	error_log :: #hh_error_log{}
}).

-define(HH_POOL_COUNT, 120).




%%游戏内协议
-define(CMD_MJ_GAME_START_NOTIFY, 10001).
-define(CMD_MJ_GAME_END_NOTIFY, 10002).
-define(CMD_MJ_OPER_REQ, 10003).
-define(CMD_MJ_OPER_NOTIFY, 10004).
-define(CMD_MJ_OPER_ERROR, 10005).



-endif.