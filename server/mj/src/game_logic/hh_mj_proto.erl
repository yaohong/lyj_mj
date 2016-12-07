%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 十一月 2016 21:43
%%%-------------------------------------------------------------------
-module(hh_mj_proto).
-author("yaohong").
-include("../../include/mj_pb.hrl").
-include("../../include/common_pb.hrl").
-include("hh_mj.hrl").
-include("../qp_type.hrl").
%% API

-export([decode_packet/1]).
-export([encode_packet/1]).

decode_packet(Bin) ->
	#qp_logic{cmd = Cmd, serialized = Body} = mj_pb:decode_qp_logic(Bin),
	decode_packet(Cmd, Body).


decode_packet(?CMD_MJ_OPER_REQ, Body) ->
	mj_pb:decode_qp_mj_oper_req(Body).



encode_packet(Packet) when is_record(Packet, qp_mj_game_start_notify) ->
	encode_packet(?CMD_MJ_GAME_START_NOTIFY, mj_pb:encode_qp_mj_game_start_notify(Packet));
encode_packet(Packet) when is_record(Packet, qp_mj_game_end_notify) ->
	encode_packet(?CMD_MJ_GAME_END_NOTIFY, mj_pb:encode_qp_mj_game_end_notify(Packet));
encode_packet(Packet) when is_record(Packet, qp_mj_oper_notify) ->
	encode_packet(?CMD_MJ_OPER_NOTIFY, mj_pb:encode_qp_mj_oper_notify(Packet));
encode_packet(Packet) when is_record(Packet, qp_mj_oper_error) ->
	encode_packet(?CMD_MJ_OPER_ERROR, mj_pb:encode_qp_mj_oper_error(Packet)).

encode_packet(Cmd, Body) ->
	Packet = #qp_logic{cmd = Cmd, serialized = Body},
	Bin = mj_pb:encode_qp_logic(Packet),
	qp_proto:encode_qp_packet(#qp_game_data{game_data = Bin}).



