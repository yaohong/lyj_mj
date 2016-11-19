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

-record(hh_seat, {
	chi :: [integer()],				%%吃的牌
	peng :: [integer()],			%%碰的牌
	gang :: [integer()],			%%杠的牌
	pai :: [integer()]				%%自己手里的牌
}).


%%特殊操作相关的数据
-record(hh_special, {
	oper_queue :: [{SeatNum :: integer(), OperFlag :: integer()}],
	oper_index :: integer(),
	chupai_seat_number :: integer(),
	chupai_value :: integer()
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
	old :: #hh_old_oper{},
	next :: #hh_next_oper{},
	state_flag :: integer(),
	error_flag :: integer(),
	last_time :: integer(),
	error_log :: #hh_error_log{}
}).

-define(HH_POOL_COUNT, 120).




%%游戏内协议
-define(CMD_MJ_GAME_START_NOTIFY, 10001).
-define(CMD_MJ_GAME_END_NOTIFY, 10002).
-define(CMD_MJ_OPER_REQ, 10003).
-define(CMD_MJ_OPER_NOTIFY, 10004).




-endif.