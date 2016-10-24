%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 十月 2016 21:16
%%%-------------------------------------------------------------------
-module(qp_proto).
-author("yaohong").
-include("../include/mj_pb.hrl").
-include("qp_proto.hrl").
-include("qp_type.hrl").
%% API

-export([decode_qp_packet/1]).
-export([encode_qp_packet/1]).

decode_qp_packet(Bin) ->
    #qp_packet{cmd = Cmd, serialized = Body} = mj_pb:decode_qp_packet(Bin),
    decode_qp_packet(Cmd, Body).


decode_qp_packet(?CMD_QP_LOGIN_REQ, Body) ->
    mj_pb:decode_qp_login_req(Body);
decode_qp_packet(?CMD_QP_CREATE_ROOM_REQ, Body) ->
    mj_pb:decode_qp_create_room_req(Body);
decode_qp_packet(?CMD_QP_JOIN_ROOM_REQ, Body) ->
    mj_pb:decode_qp_join_room_req(Body);
decode_qp_packet(?CMD_QP_READY_REQ, Body) ->
    mj_pb:decode_qp_ready_req(Body);
decode_qp_packet(?CMD_QP_EXIT_ROOM_REQ, Body) ->
    mj_pb:decode_qp_exit_room_req(Body);
decode_qp_packet(?CMD_QP_GAME_DATA, Body) ->
    mj_pb:decode_qp_game_data(Body);
decode_qp_packet(?CMD_QP_PING_REQ, Body) ->
    mj_pb:decode_qp_ping_req(Body).


encode_qp_packet(Packet) when is_record(Packet, qp_login_rsp) ->
    encode_qp_packet(?CMD_QP_LOGIN_RSP, mj_pb:encode_qp_login_rsp(Packet));
encode_qp_packet(Packet) when is_record(Packet, qp_create_room_rsp) ->
    encode_qp_packet(?CMD_QP_CREATE_ROOM_RSP, mj_pb:encode_qp_create_room_rsp(Packet));
encode_qp_packet(Packet) when is_record(Packet, qp_join_room_rsp) ->
    encode_qp_packet(?CMD_QP_JOIN_ROOM_RSP, mj_pb:encode_qp_join_room_rsp(Packet));
encode_qp_packet(Packet) when is_record(Packet, qp_join_room_push) ->
    encode_qp_packet(?CMD_QP_JOIN_ROOM_PUSH, mj_pb:encode_qp_join_room_push(Packet));
encode_qp_packet(Packet) when is_record(Packet, qp_ready_rsp) ->
    encode_qp_packet(?CMD_QP_READY_RSP, mj_pb:encode_qp_ready_rsp(Packet));
encode_qp_packet(Packet) when is_record(Packet, qp_ready_push) ->
    encode_qp_packet(?CMD_QP_READY_PUSH, mj_pb:encode_qp_ready_push(Packet));
encode_qp_packet(Packet) when is_record(Packet, qp_exit_room_rsp) ->
    encode_qp_packet(?CMD_QP_EXIT_ROOM_RSP, mj_pb:encode_qp_exit_room_rsp(Packet));
encode_qp_packet(Packet) when is_record(Packet, qp_exit_room_push) ->
    encode_qp_packet(?CMD_QP_EXIT_ROOM_PUSH, mj_pb:encode_qp_exit_room_push(Packet));
encode_qp_packet(Packet) when is_record(Packet, qp_room_dismiss_push) ->
    encode_qp_packet(?CMD_QP_ROOM_DISSMISS_PUSH, mj_pb:encode_qp_room_dismiss_push(Packet));
encode_qp_packet(Packet) when is_record(Packet, qp_game_data) ->
    encode_qp_packet(?CMD_QP_GAME_DATA, mj_pb:encode_qp_game_data(Packet));
encode_qp_packet(Packet) when is_record(Packet, qp_ping_rsp) ->
    encode_qp_packet(?CMD_QP_PING_RSP, mj_pb:encode_qp_ping_rsp(Packet)).

encode_qp_packet(Cmd, Body) ->
    Packet = #qp_packet{cmd = Cmd, serialized = Body, seq_id = 0},
    Bin = mj_pb:encode_qp_packet(Packet),
    Len = size(Bin),
    <<Len:?BIG_UINT32, Bin/binary>>.
