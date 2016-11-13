%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 十一月 2016 16:23
%%%-------------------------------------------------------------------
-module(qp_logic_cfg).
-author("yaohong").

%% API
-export([get_logic_mod/1]).

%%根据房间类型范围对应的逻辑处理模块
get_logic_mod(0) ->
	hh_mj;
get_logic_mod(1) ->
	ok;
get_logic_mod(2) ->
	ok.