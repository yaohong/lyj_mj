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
-include("../include/mj_pb.hrl").
%% API
-export([create_room_failed_bin/1]).



create_room_failed_bin(Code) ->
    CreateFailedRsp = #qp_create_room_rsp{state = Code},
    qp_proto:encode_qp_packet(CreateFailedRsp).