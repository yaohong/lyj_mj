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
%% API

-export([decode_qp_packet/1]).
-export([encode_qp_packet/1]).

decode_qp_packet(Bin) ->
    #qp_packet{cmd = Cmd, body = Body} = world_server_pb:decode_qp_packet(Bin),
    decode_qp_packet(Cmd, Body).


decode_qp_packet(?WS_CMD_CMD_DEV_LOGIN_REQ, Body) ->
    world_server_pb:decode_ws_dev_login_req(Body);
decode_qp_packet(?WS_CMD_CMD_CW_CREATE_ACTOR_REQ, Body) ->
    world_server_pb:decode_ws_create_actor_req(Body);
decode_qp_packet(?WS_CMD_CMD_CW_ENTER_SCENE_REQ, Body) ->
    world_server_pb:decode_cw_enter_scene_req(Body);
decode_qp_packet(?WS_CMD_CMD_CW_DELETE_ACTOR_REQ, Body) ->
    world_server_pb:decode_cw_delete_actor_req(Body);
decode_qp_packet(?WS_CMD_CMD_OTHER_PLAYER_INFO_REQ, Body) ->
    world_server_pb:decode_zbpkg_other_player_info_req(Body);
decode_qp_packet(?WS_CMD_CMD_HJ_LOGIN_REQ, Body) ->
    world_server_pb:decode_ws_hj_login_req(Body);
decode_qp_packet(?WS_CMD_CMD_USE_CDK_REQ, Body) ->
    world_server_pb:decode_zbpkg_use_cdk_req(Body).


encode_qp_packet(Packet) when is_record(Packet, login_rsp) ->
    encode_qp_packet(?CMD_LOGIN_RSP, world_server_pb:encode_login_rsp(Packet));
encode_qp_packet(Packet) when is_record(Packet, create_room_rsp) ->
    encode_qp_packet(?CMD_CREATE_ROOM_RSP, world_server_pb:encode_create_room_rsp(Packet));
encode_ws_packet(Packet) when is_record(Packet, join_room_rsp) ->
    encode_qp_packet(?CMD_JOIN_ROOM_RSP, world_server_pb:encode_join_room_rsp(Packet));
encode_ws_packet(Packet) when is_record(Packet, ready_rsp) ->
    encode_qp_packet(?CMD_READY_RSP, world_server_pb:encode_ready_rsp(Packet));
encode_ws_packet(Packet) when is_record(Packet, game_data) ->
    encode_qp_packet(?CMD_GAME_DATA, world_server_pb:encode_game_data(Packet)).

encode_qp_packet(Cmd, Body) ->
    Packet = #qp_packet{cmd = Cmd, body = Body, seq_id = 0},
    world_server_pb:encode_qp_packet(Packet).