%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 十月 2016 18:57
%%%-------------------------------------------------------------------
-module(qp_packet_util).
-author("yaohong").
-include("../include/common_pb.hrl").
%% API
-export([create_room_failed_bin/1, create_ping_rsp_bin/0]).



create_room_failed_bin(Code) ->
    CreateFailedRsp = #qp_create_room_rsp{state = Code},
    qp_proto:encode_qp_packet(CreateFailedRsp).


create_ping_rsp_bin() ->
    Rsp = #qp_ping_rsp{seat_number = 0},
    qp_proto:encode_qp_packet(Rsp).