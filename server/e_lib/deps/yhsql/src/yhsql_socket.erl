%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. 八月 2015 14:55
%%%-------------------------------------------------------------------
-module(yhsql_socket).
-author("yaohong").
-include("yhsql.hrl").
%% API
-export([
	do_send/4,
	do_recv/3
        ]
).

-define(MYSQL_HEAD_LEN, 3).


do_send(Sock, Packet, Num, LogFun) ->
	Data = <<(size(Packet)):24/little, Num:8, Packet/binary>>,
	case gen_tcp:send(Sock, Data) of
		ok -> ok;
		{error, Reason} ->
			?Log2(LogFun, debug, "do_send fail,reason=~p", [Reason]),
			throw({custom, {socket_error, Reason}})
	end.


do_recv(LogFun, Sock, SeqNum) when SeqNum =:= undefined ->
	do_recv1(LogFun, Sock);
do_recv(LogFun, Sock, SeqNum) when is_integer(SeqNum) ->
	ResponseNum = SeqNum + 1,
	{success, {Packet, Num}} = do_recv1(LogFun, Sock),
	if
		ResponseNum =:= Num ->
			{success, {Packet, Num}};
		true ->
			?Log2(LogFun, error, "do_recv fail,num=~p, response_num=~p", [ResponseNum, Num]),
			throw({custom, {yhsql_error, num_error}})
	end.

-spec do_recv1(
	              LogFun :: function(),
	              Sock :: term()
              )  -> {error, Reason :: atom()} | {success, {Packet :: binary(), Num :: integer(), Rest :: binary()}}.
do_recv1(LogFun, Sock) ->
	case gen_tcp:recv(Sock, ?MYSQL_HEAD_LEN, get(socket_timeout)) of
		{ok, <<Length:24/little>>} ->
			case gen_tcp:recv(Sock, Length + 1, get(socket_timeout)) of
				{ok, <<Num:8, D/binary>>} ->
					{success, {D, Num}};
				{error, Reason1} ->
					?Log2(LogFun, debug, "gen_tcp error,reason=~p", [Reason1]),
					throw({custom, {socket_error, Reason1}})
			end;
		{error, Reason} ->
			?Log2(LogFun, debug, "gen_tcp error,reason=~p", [Reason]),
			throw({custom, {socket_error, Reason}})
	end.