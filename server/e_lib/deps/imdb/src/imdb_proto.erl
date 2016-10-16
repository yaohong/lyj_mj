-module(imdb_proto).
-author('erlangonrails@gmail.com').
-include("imdb.hrl").
-include("netlib.hrl").
-include("file_log.hrl").
-export([encode_sys_a/5, decode_sys_a/1,
         encode_amh_aa/4, decode_amh_aa/1,
         encode_amh_ab/1, decode_amh_ab/1,
         encode_amh_ac/2, decode_amh_ac/1,
         encode_amh_ad/1, decode_amh_ad/1,
         encode_amh_af/1, decode_amh_af/1,
         encode_amh_ag/3, decode_amh_ag/1,
         encode_amh_ai/1, decode_amh_ai/1,
		 encode_amh_al/2, decode_amh_al/1,
         encode_amh_am/1, decode_amh_am/1,



         encode_filter_f/5, decode_filter_f/1,
         encode_score_r/1, decode_score_r/1,
         encode_score_a/2, decode_score_a/1,
         encode_score_d/2, decode_score_d/1,
         encode_score_a_ext/3, decode_score_a_ext/1,
         encode_score_d_ext/3, decode_score_d_ext/1,
         encode_proto_r/2, decode_proto_r/1,
         encode_proto_w/2, decode_proto_w/1,
         encode_proto_update/2, decode_proto_update/1,
         encode_proto_b/1, decode_proto_b/1,
         encode_proto_c/1, decode_proto_c/1,
         encode_proto_e/1, decode_proto_e/1,
         encode_proto_po/1, decode_proto_po/1,

         encode_proto_pr/3, decode_proto_pr/1,
         encode_proto_t/3, decode_proto_t/1,

         encode_proto_ln/3, decode_proto_ln/1,
         encode_proto_lo/3, decode_proto_lo/1,
         encode_proto_lp/3, decode_proto_lp/1,

         encode_proto_5/10, decode_proto_5/1,
         encode_proto_6/2, decode_proto_6/1,
         encode_proto_1/1, decode_proto_1/1,
         encode_proto_q/1, decode_proto_q/1,
         encode_proto_yy/1, decode_proto_yy/1,
         encode_proto_y/4, decode_proto_y/1,
         encode_proto_o/4, decode_proto_o/1,
         encode_proto_p/1, decode_proto_p/1,         
         encode_proto_a/5, decode_proto_a/1,
         encode_proto_fb/3, decode_proto_fb/1,
         encode_proto_g/1, decode_proto_g/1,
         encode_proto_j/1, decode_proto_j/1,
         encode_proto_fe/2, decode_proto_fe/1,
         encode_proto_f/2, decode_proto_f/1,
         encode_proto_fc/4, decode_proto_fc/1,
         encode_proto_d/4, decode_proto_d/1,
         encode_proto_h/2, decode_proto_h/1,
         encode_proto_hp/2, decode_proto_hp/1,
         encode_proto_la/4, decode_proto_la/1,
         encode_proto_lb/3, decode_proto_lb/1,
         encode_proto_lc/2, decode_proto_lc/1,

         encode_items/1,
         encode_kvs/1,
         decode_kvs/1,
         decode_multi_items/1,
	 protohex_to_int/1,
	 int_to_protohex/1]).

-spec encode_sys_a(
        Id :: netlib_string() | netlib_binary(),
        Type :: netlib_string() | netlib_binary(),
        Coin :: netlib_string() | netlib_binary(),
        Source :: netlib_string() | netlib_binary(),
        Date :: netlib_string() | netlib_binary()) ->
    iodata().
encode_sys_a(Id, Type, Coin, Source, Date) ->
    Data =
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_SYS,
           ?IMDB_SYS_A_BIN,
            encode_item(Id),
            encode_item(Type),
            encode_item(Coin),
            encode_item(Source),
            encode_item(Date)]),
    encode_item(Data).

-spec decode_sys_a(Data :: netlib_binary()) ->
    boolean().
decode_sys_a(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_SYS_LEN/binary, Data1/binary>> = Data,
    <<_Cmd:7/binary, Rest/binary>> = Data1,
    {_Username, Rest1} = decode_item(Rest),
    <<RetCode:8/binary, _Rest2/binary>> = Rest1,
    RetCode == ?IMDB_PROTO_RET_OK.



-spec encode_filter_f(
        Username :: netlib_string() | netlib_binary(),
        Encode :: netlib_string() | netlib_binary(),
        Channel :: netlib_string() | netlib_binary(),
        Type :: netlib_string() | netlib_binary(),
        Message :: netlib_string() | netlib_binary()) ->
    iodata().
encode_filter_f(Username, Encode, Channel, Type, Message) ->
    Data =
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_FILTER,
           ?IMDB_FILTER_F_BIN,
            encode_item(Username),
            encode_item(Encode),
            encode_item(Channel),
            encode_item(Type),
            encode_item(Message)]),
    encode_item(Data).

-spec decode_filter_f(Data :: netlib_binary()) ->
    Score :: binary() | undefined.
decode_filter_f(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_FILTER_LEN/binary, Data1/binary>> = Data,
    <<_Cmd:7/binary, Rest/binary>> = Data1,
    {_Username, Rest1} = decode_item(Rest),
    <<RetCode:8/binary, _Rest2/binary>> = Rest1,
    RetCode =/= ?IMDB_PROTO_RET_ERROR_09.


-spec encode_score_r(Username :: netlib_string() | netlib_binary()) ->
    iodata().
encode_score_r(Username) ->
    Data =
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_SCORE,
           ?IMDB_SCORE_R_BIN,
            encode_item(Username)]),
    encode_item(Data).

-spec decode_score_r(Data :: netlib_binary()) ->
    Score :: binary() | undefined.
decode_score_r(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_SCORE_LEN/binary, Data1/binary>> = Data,
    <<_Cmd:7/binary, Rest/binary>> = Data1,
    <<RetCode:8/binary, Rest1/binary>> = Rest,
    case RetCode =:= ?IMDB_PROTO_RET_OK of
        true ->
            {_Username, Rest2} = decode_item(Rest1),
            {Score, _Rest3} = decode_item(Rest2),
            Score;
        false ->
            undefined
    end.


-spec encode_score_a(
        Username :: netlib_string() | netlib_binary(),
        Delta :: netlib_string() | netlib_binary()) ->
    iodata().
encode_score_a(Username, Delta) ->
    Data =
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_SCORE,
           ?IMDB_SCORE_A_BIN,
            encode_item(Username),
            encode_item(Delta)]),
    encode_item(Data).

-spec decode_score_a(Data :: netlib_binary()) ->
    Score :: binary() | undefined.
decode_score_a(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_SCORE_LEN/binary, Data1/binary>> = Data,
    <<_Cmd:7/binary, Rest/binary>> = Data1,
    <<RetCode:8/binary, Rest1/binary>> = Rest,
    case RetCode =:= ?IMDB_PROTO_RET_OK of
        true ->
            {_Username, Rest2} = decode_item(Rest1),
            {Score, _Rest3} = decode_item(Rest2),
            Score;
        false ->
            undefined
    end.


-spec encode_score_d(
        Username :: netlib_string() | netlib_binary(),
        Delta :: netlib_string() | netlib_binary()) ->
    iodata().
encode_score_d(Username, Delta) ->
    Data =
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_SCORE,
           ?IMDB_SCORE_D_BIN,
            encode_item(Username),
            encode_item(Delta)]),
    encode_item(Data).

-spec decode_score_d(Data :: netlib_binary()) ->
    Score :: binary() | undefined.
decode_score_d(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_SCORE_LEN/binary, Data1/binary>> = Data,
    <<_Cmd:7/binary, Rest/binary>> = Data1,
    <<RetCode:8/binary, Rest1/binary>> = Rest,
    case RetCode =:= ?IMDB_PROTO_RET_OK of
        true ->
            {_Username, Rest2} = decode_item(Rest1),
            {Score, _Rest3} = decode_item(Rest2),
            Score;
        false ->
            undefined
    end.


-spec encode_score_a_ext(
        Username :: netlib_string() | netlib_binary(),
        Delta :: netlib_string() | netlib_binary(),
        Ext :: netlib_string() | netlib_binary()) ->
    iodata().
encode_score_a_ext(Username, Delta, Ext) ->
    Data =
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_SCORE,
           ?IMDB_SCORE_A_BIN,
            encode_item(Username),
            encode_item(Delta),
            encode_item(Ext)]),
    encode_item(Data).

-spec decode_score_a_ext(Data :: netlib_binary()) ->
    Score :: binary() | undefined.
decode_score_a_ext(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_SCORE_LEN/binary, Data1/binary>> = Data,
    <<_Cmd:7/binary, Rest/binary>> = Data1,
    <<RetCode:8/binary, Rest1/binary>> = Rest,
    case RetCode =:= ?IMDB_PROTO_RET_OK of
        true ->
            {_Username, Rest2} = decode_item(Rest1),
            {Score, _Rest3} = decode_item(Rest2),
            Score;
        false ->
            undefined
    end.


-spec encode_score_d_ext(
        Username :: netlib_string() | netlib_binary(),
        Delta :: netlib_string() | netlib_binary(),
        Ext :: netlib_string() | netlib_binary()) ->
    iodata().
encode_score_d_ext(Username, Delta, Ext) ->
    Data =
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_SCORE,
           ?IMDB_SCORE_D_BIN,
            encode_item(Username),
            encode_item(Delta),
            encode_item(Ext)]),
    encode_item(Data).

-spec decode_score_d_ext(Data :: netlib_binary()) ->
    Score :: binary() | undefined.
decode_score_d_ext(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_SCORE_LEN/binary, Data1/binary>> = Data,
    <<_Cmd:7/binary, Rest/binary>> = Data1,
    <<RetCode:8/binary, Rest1/binary>> = Rest,
    case RetCode =:= ?IMDB_PROTO_RET_OK of
        true ->
            {_Username, Rest2} = decode_item(Rest1),
            {Score, _Rest3} = decode_item(Rest2),
            Score;
        false ->
            undefined
    end.




-spec encode_proto_r(Username :: netlib_string() | netlib_binary(),
                     Fields :: [netlib_string() | netlib_binary()]) ->
    netlib_iolist().
encode_proto_r(Username, Fields) ->
    Data =
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_PUSER,
           ?IMDB_PROTO_R_BIN,
            encode_item(Username),
            encode_items(Fields)]),
    encode_item(Data).

-spec decode_proto_r(Data :: netlib_binary()) ->
    {boolean(), [{K :: netlib_binary(),
                  V :: netlib_binary()}]}.
decode_proto_r(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_PUSER_LEN/binary, Data1/binary>> = Data,
    <<_ReadCmd:7/binary, Rest/binary>> = Data1,
    <<RetCode:8/binary, Rest1/binary>> = Rest,
    {_Username, Rest2} = decode_item(Rest1),
    case RetCode =:= ?IMDB_PROTO_RET_OK of
        true ->
            decode_kvs(Rest2);
        false ->
            []
    end.


-spec encode_proto_5(
        Channel :: netlib_string() | netlib_binary(),
        OrderId :: netlib_string() | netlib_binary(),
        Game :: netlib_string() | netlib_binary(),
        Device :: netlib_string() | netlib_binary(),
        Account :: netlib_string() | netlib_binary(),
        Id :: netlib_string() | netlib_binary(),
        ProductId :: netlib_string() | netlib_binary(),
        ProductType :: netlib_string() | netlib_binary(),
        Quantity :: netlib_string() | netlib_binary(),
        Money :: netlib_string() | netlib_binary()) ->
    netlib_iolist().
encode_proto_5(Channel, OrderId, Game, Device, Account, Id, ProductId, ProductType, Quantity, Money) ->
    Data =
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_POKER,
           ?IMDB_PROTO_5_BIN,
            encode_item(Channel),
            encode_item(OrderId),
            encode_item(Game),
            encode_item(Device),
            encode_item(Account),
            encode_item(Id),
            encode_item(ProductId),
            encode_item(ProductType),
            encode_item(Quantity),
            encode_item(Money)]),
    encode_item(Data).


%% 返回true则在黑名单中
-spec decode_proto_5(Data :: netlib_binary()) ->
    boolean().
decode_proto_5(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_POKER_LEN/binary, Data1/binary>> = Data,
    <<_ProtoWCmd:7/binary, Rest/binary>> = Data1,
    {_ChannelId, Rest1} = decode_item(Rest),
    <<RetCode:8/binary, _Rest2/binary>> = Rest1,
    RetCode =:= ?IMDB_PROTO_RET_OK.


-spec encode_proto_6(
        Channel :: netlib_string() | netlib_binary(),
        OrderId :: netlib_string() | netlib_binary()) ->
    netlib_iolist().
encode_proto_6(Channel, OrderId) ->
    Data =
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_POKER,
           ?IMDB_PROTO_6_BIN,
            encode_item(Channel),
            encode_item(OrderId)]),
    encode_item(Data).


%% 返回true则在黑名单中
-spec decode_proto_6(Data :: netlib_binary()) ->
    boolean().
decode_proto_6(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_POKER_LEN/binary, Data1/binary>> = Data,
    <<_ProtoWCmd:7/binary, Rest/binary>> = Data1,
    {_ChannelId, Rest1} = decode_item(Rest),
    Rest1.


-spec encode_proto_1(Ip :: netlib_string() | netlib_binary()) ->
    netlib_iolist().
encode_proto_1(Ip) ->
    Data =
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_POKER,
           ?IMDB_PROTO_1_BIN,
            encode_item(Ip)]),
    encode_item(Data).


%% 返回true则在黑名单中
-spec decode_proto_1(Data :: netlib_binary()) ->
    boolean().
decode_proto_1(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_POKER_LEN/binary, Data1/binary>> = Data,
    <<_ProtoWCmd:7/binary, Rest/binary>> = Data1,
    {_IP, Rest1} = decode_item(Rest),
    <<RetCode:8/binary, _Rest2/binary>> = Rest1,
    RetCode =:= ?IMDB_PROTO_RET_OK.

-spec encode_proto_q(Type :: netlib_string() | netlib_binary()) ->
    netlib_iolist().
encode_proto_q(Type) ->
    Data =
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_POKER,
           ?IMDB_PROTO_Q_BIN,
            encode_item(Type)]),
    encode_item(Data).

decode_proto_q(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_POKER_LEN/binary, Data1/binary>> = Data,
    <<_Cmd:7/binary, Rest/binary>> = Data1,
    {_Id, Rest1} = decode_item(Rest),
    <<RetCode:8/binary, Rest2/binary>> = Rest1,
    case RetCode =:= ?IMDB_PROTO_RET_OK of
        true ->
            decode_multi_items(Rest2);
        false ->
            []
    end.

%% 获取筹码赠送记录
-spec encode_proto_yy(Userid :: netlib_string() | netlib_binary()) ->
    netlib_iolist().
encode_proto_yy(Userid) ->

    Data =
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_POKER,
           ?IMDB_PROTO_YY_BIN,
            encode_item(Userid)]),
    encode_item(Data).

decode_proto_yy(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_POKER_LEN/binary, Data1/binary>> = Data,
    <<_Cmd:7/binary, Rest/binary>> = Data1,
    {_Id, Rest1} = decode_item(Rest),
    <<RetCode:8/binary, Rest2/binary>> = Rest1,
    case RetCode =:= ?IMDB_PROTO_RET_OK of
        true ->
            decode_multi_items(Rest2);
        false ->
            []
    end.

%% 插入筹码赠送记录
-spec encode_proto_y(Donor :: integer() | binary(), Receiver :: integer() | binary(), Number :: integer() | binary(), Time :: string() | binary()) ->
    netlib_iolist().
encode_proto_y(Donor, Receiver, Number, Time) ->
    Data =
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_POKER,
           ?IMDB_PROTO_Y_BIN,
            encode_item(Donor),
            encode_item(Receiver),
            encode_item(Number),
            encode_item(Time)
            ]),
    encode_item(Data).

decode_proto_y(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_POKER_LEN/binary, Data1/binary>> = Data,
    <<_Cmd:7/binary, Rest/binary>> = Data1,
    {_Id, Rest1} = decode_item(Rest),
    <<RetCode:8/binary, _Rest2/binary>> = Rest1,
    case RetCode =:= ?IMDB_PROTO_RET_OK of
        true ->
            true;
        false ->
            false
    end.



%% 获取奥马哈礼物赠送记录
-spec encode_amh_ab(Userid :: netlib_string() | netlib_binary()) ->
    netlib_iolist().
encode_amh_ab(Userid) ->
    Data =
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_POKER,
           ?IMDB_AMH_GIFT_AB_BIN,
            encode_item(Userid)]),
    encode_item(Data).

decode_amh_ab(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_POKER_LEN/binary, Data1/binary>> = Data,
    <<_Cmd:8/binary, Rest/binary>> = Data1,
    {_Id, Rest1} = decode_item(Rest),
    <<RetCode:8/binary, Rest2/binary>> = Rest1,
    case RetCode =:= ?IMDB_PROTO_RET_OK of
        true ->
            decode_multi_items(Rest2);
        false ->
            []
    end.

%% 插入奥马哈礼物赠送记录
-spec encode_amh_aa(Donor :: integer() | binary(), Receiver :: integer() | binary(), Goods :: integer() | binary(), Time :: string() | binary()) ->
    netlib_iolist().
encode_amh_aa(Donor, Receiver, Goods, Time) ->
    Data =
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_POKER,
           ?IMDB_AMH_GIFT_AA_BIN,
            encode_item(Donor),
            encode_item(Receiver),
            encode_item(Goods),
            encode_item(Time)
            ]),
    encode_item(Data).

decode_amh_aa(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_POKER_LEN/binary, Data1/binary>> = Data,
    <<_Cmd:8/binary, Rest/binary>> = Data1,
    {_Id, Rest1} = decode_item(Rest),
    <<RetCode:8/binary, _Rest2/binary>> = Rest1,
    case RetCode =:= ?IMDB_PROTO_RET_OK of
        true ->
            true;
        false ->
            false
    end.


%% 删除奥马哈礼物赠送记录
-spec encode_amh_ac(Id :: integer() | binary(), Receiver :: integer() | binary()) ->
    netlib_iolist().
encode_amh_ac(ID, Receiver) ->
    Data =
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_POKER,
           ?IMDB_AMH_GIFT_AC_BIN,
            encode_item(ID),
            encode_item(Receiver)
            ]),
    encode_item(Data).

decode_amh_ac(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_POKER_LEN/binary, Data1/binary>> = Data,
    <<_Cmd:8/binary, Rest/binary>> = Data1,
    {_Id, Rest1} = decode_item(Rest),
    <<RetCode:8/binary, _Rest2/binary>> = Rest1,
    case RetCode =:= ?IMDB_PROTO_RET_OK of
        true ->
            true;
        false ->
            false
    end.

%% 获取奥马哈排行榜
-spec encode_amh_ad(Type ::integer() | netlib_binary()) ->
    netlib_iolist().
encode_amh_ad(Type) ->
    Data =
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_POKER,
           ?IMDB_AMH_GIFT_AD_BIN,
            encode_item(Type)]),
    encode_item(Data).

decode_amh_ad(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_POKER_LEN/binary, Data1/binary>> = Data,
    <<_Cmd:8/binary, Rest/binary>> = Data1,
    {_Id, Rest1} = decode_item(Rest),
    <<RetCode:8/binary, Rest2/binary>> = Rest1,
    case RetCode =:= ?IMDB_PROTO_RET_OK of
        true ->
            decode_multi_items(Rest2);
        false ->
            []
    end.

%% 获取问题列表
-spec encode_amh_af(UserId ::integer() | netlib_binary()) ->
    netlib_iolist().
encode_amh_af(UserId) ->
    Data =
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_POKER,
           ?IMDB_AMH_GIFT_AF_BIN,
            encode_item(UserId)]),
    encode_item(Data).

decode_amh_af(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_POKER_LEN/binary, Data1/binary>> = Data,
    <<_Cmd:8/binary, Rest/binary>> = Data1,
    {_Id, Rest1} = decode_item(Rest),
    <<RetCode:8/binary, Rest2/binary>> = Rest1,
    case RetCode =:= ?IMDB_PROTO_RET_OK of
        true ->
            decode_multi_items(Rest2);
        false ->
            []
    end.

%% 插入奥马哈问题
-spec encode_amh_ag(UserId :: integer() | binary(), Nick :: string() | binary(), Msg :: string() | binary()) ->
    netlib_iolist().
encode_amh_ag(UserId, Nick, Msg) ->
    Data =
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_POKER,
           ?IMDB_AMH_GIFT_AG_BIN,
            encode_item(UserId),
            encode_item(Nick),
            encode_item(Msg)
            ]),
    encode_item(Data).

decode_amh_ag(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_POKER_LEN/binary, Data1/binary>> = Data,
    <<_Cmd:8/binary, Rest/binary>> = Data1,
    {_Id, Rest1} = decode_item(Rest),
    <<RetCode:8/binary, _Rest2/binary>> = Rest1,
    case RetCode =:= ?IMDB_PROTO_RET_OK of
        true ->
            true;
        false ->
            false
    end.

%% 获取回答列表
-spec encode_amh_ai(UserId ::integer() | netlib_binary()) ->
    netlib_iolist().
encode_amh_ai(UserId) ->
    Data =
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_POKER,
           ?IMDB_AMH_GIFT_AI_BIN,
            encode_item(UserId)]),
    encode_item(Data).

decode_amh_ai(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_POKER_LEN/binary, Data1/binary>> = Data,
    <<_Cmd:8/binary, Rest/binary>> = Data1,
    {_Id, Rest1} = decode_item(Rest),
    <<RetCode:8/binary, Rest2/binary>> = Rest1,
    case RetCode =:= ?IMDB_PROTO_RET_OK of
        true ->
            decode_multi_items(Rest2);
        false ->
            []
    end.

encode_amh_al(GameName, DisplayPos) when is_binary(GameName) andalso is_binary(DisplayPos) ->
	Data =
		iolist_to_binary(
			[?IMDB_PROTO_PROXY_M_POKER,
				?IMDB_AMH_DISPLAY_POS_AL_BIN,
				encode_item(GameName),
				encode_item(DisplayPos)]),
	encode_item(Data).

decode_amh_al(Data) ->
	<<_ProxyPrefix:?IMDB_PROTO_PROXY_M_POKER_LEN/binary, Data1/binary>> = Data,
	<<_Cmd:8/binary, Rest/binary>> = Data1,
	{_Id, Rest1} = decode_item(Rest),
	<<RetCode:8/binary, _Rest2/binary>> = Rest1,
	case RetCode =:= ?IMDB_PROTO_RET_OK of
		true -> true;
		false -> false
	end.

encode_amh_am(Limit) when is_binary(Limit) ->
	Data =
		iolist_to_binary(
			[?IMDB_PROTO_PROXY_M_POKER,
				?IMDB_AMH_DISPLAY_POS_AM_BIN,
				encode_item(Limit)]),
	encode_item(Data).

decode_amh_am(Data) ->
	<<_ProxyPrefix:?IMDB_PROTO_PROXY_M_POKER_LEN/binary, Data1/binary>> = Data,
	<<_Cmd:8/binary, Rest/binary>> = Data1,
	{_Id, Rest1} = decode_item(Rest),
	<<RetCode:8/binary, Rest2/binary>> = Rest1,
	case RetCode =:= ?IMDB_PROTO_RET_OK of
		true -> decode_multi_items(Rest2);
		false -> []
	end.

   %% 更新用户密码
-spec encode_proto_hp(
        UserID :: netlib_string() | netlib_binary(),
        NewPasswd :: netlib_string() | netlib_binary()) ->
    netlib_iolist().
encode_proto_hp(UserID, NewPasswd) ->
    Data =
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_POKER,
           ?IMDB_PROTO_HP_BIN,
           encode_item(UserID),
           encode_item(NewPasswd)]),
    encode_item(Data).

%% 
decode_proto_hp(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_POKER_LEN/binary, Data1/binary>> = Data,
    <<_Cmd:7/binary, Rest/binary>> = Data1,
    {_Id, Rest1} = decode_item(Rest),
    <<RetCode:8/binary, _Rest2/binary>> = Rest1,
    case RetCode =:= ?IMDB_PROTO_RET_OK of
        true ->
            true;
        false ->
            false
    end. 


-spec encode_proto_w(Username :: netlib_string() | netlib_binary(),
                     FieldKVs :: [netlib_tuple()]) ->
    netlib_iolist().
encode_proto_w(Username, FieldKVs) ->
    Data =
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_PUSER,
           ?IMDB_PROTO_W_BIN,
            encode_item(Username),
            encode_kvs(FieldKVs)]),
    encode_item(Data).


-spec decode_proto_w(Data :: netlib_binary()) ->
    boolean().
decode_proto_w(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_PUSER_LEN/binary, Data1/binary>> = Data,
    <<_ProtoWCmd:7/binary, Rest/binary>> = Data1,
    <<RetCode:8/binary, Rest1/binary>> = Rest,
    {_Username, _Rest2} = decode_item(Rest1),
    RetCode =:= ?IMDB_PROTO_RET_OK.


-spec encode_proto_update(Username :: netlib_string() | netlib_binary(),
                          FieldKVs :: [netlib_tuple()]) ->
    netlib_iolist().
encode_proto_update(Username, FieldKVs) ->
    Data =
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_PUSER,
           ?IMDB_PROTO_A_BIN,
            encode_item(Username),
            encode_kvs(FieldKVs)]),
    encode_item(Data).


-spec decode_proto_update(Data :: netlib_binary()) ->
    boolean().
decode_proto_update(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_PUSER_LEN/binary, Data1/binary>> = Data,
    <<_ProtoWCmd:7/binary, Rest/binary>> = Data1,
    <<RetCode:8/binary, Rest1/binary>> = Rest,
    {_Username, _Rest2} = decode_item(Rest1),
    RetCode =:= ?IMDB_PROTO_RET_OK.


encode_proto_b(Username) ->
    Data =
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_POKER,
           ?IMDB_PROTO_B_BIN,
            encode_item(Username)]),
    encode_item(Data).

decode_proto_b(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_POKER_LEN/binary, Data1/binary>> = Data,
    <<_Cmd:7/binary, Rest/binary>> = Data1,
    {_Username, Rest1} = decode_item(Rest),
    <<RetCode:8/binary, Rest2/binary>> = Rest1,
    case RetCode =:= ?IMDB_PROTO_RET_OK of
        true ->
            decode_multi_items(Rest2);
        false ->
            []
    end.

encode_proto_c(Username) ->
    Data =
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_POKER,
           ?IMDB_PROTO_C_BIN,
            encode_item(Username)]),
    encode_item(Data).

decode_proto_c(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_POKER_LEN/binary, Data1/binary>> = Data,
    <<_ReadCmd:7/binary, Rest/binary>> = Data1,
    {_Username, Rest1} = decode_item(Rest),
    <<RetCode:8/binary, Rest2/binary>> = Rest1,
    case RetCode =:= ?IMDB_PROTO_RET_OK of
        true ->
            decode_multi_items(Rest2);
        false ->
            []
    end.


encode_proto_e(Id) ->
    Data =
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_POKER,
           ?IMDB_PROTO_E_BIN,
            encode_item(Id)]),
    encode_item(Data).

decode_proto_e(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_POKER_LEN/binary, Data1/binary>> = Data,
    <<_Cmd:7/binary, Rest/binary>> = Data1,
    {_Id, Rest1} = decode_item(Rest),
    <<RetCode:8/binary, Rest2/binary>> = Rest1,
    case RetCode =:= ?IMDB_PROTO_RET_OK of
        true ->
            decode_multi_items(Rest2);
        false ->
            []
    end.


encode_proto_po(Id) ->
    Data =
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_POKER,
           ?IMDB_PROTO_PO_BIN,
            encode_item(Id)]),
    encode_item(Data).

decode_proto_po(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_POKER_LEN/binary, Data1/binary>> = Data,
    <<_Cmd:7/binary, Rest/binary>> = Data1,
    {_Id, Rest1} = decode_item(Rest),
    <<RetCode:8/binary, Rest2/binary>> = Rest1,
    case RetCode =:= ?IMDB_PROTO_RET_OK of
        true ->
            decode_multi_items(Rest2);
        false ->
            []
    end.


%% 插入一条登陆记录
-spec encode_proto_pr(Username :: netlib_string() | netlib_binary(),
                      Ip :: netlib_string() | netlib_binary(),
                      Plat :: netlib_string() | netlib_binary()) ->
    netlib_iolist().
encode_proto_pr(Username, Ip, Plat) ->
    Data = 
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_POKER,
           ?IMDB_PROTO_PR_BIN,
           encode_item(Username),
           encode_item(Ip),
           encode_item(Plat)]),
    encode_item(Data).

-spec decode_proto_pr(Data :: netlib_binary()) ->
    boolean().
decode_proto_pr(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_POKER_LEN/binary, Data1/binary>> = Data,
    <<_Cmd:7/binary, Rest/binary>> = Data1,
    {_Username, RetCode} = decode_item(Rest),
    RetCode =:= ?IMDB_PROTO_RET_OK.


%% 插入一局输赢记录
-spec encode_proto_t(Username :: netlib_string() | netlib_binary(),
                     IsWin :: netlib_string() | netlib_binary(),     %% 1表示赢; 0表示输
                     Score :: netlib_string() | netlib_binary()) ->
    netlib_iolist().
encode_proto_t(Username, IsWin, Score) ->
    Data = 
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_POKER,
           ?IMDB_PROTO_T_BIN,
           encode_item(Username),
           encode_item(IsWin),
           encode_item(Score)]),
    encode_item(Data).

-spec decode_proto_t(Data :: netlib_binary()) ->
    boolean().
decode_proto_t(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_POKER_LEN/binary, Data1/binary>> = Data,
    <<_Cmd:7/binary, Rest/binary>> = Data1,
    {_Username, RetCode} = decode_item(Rest),
    RetCode =:= ?IMDB_PROTO_RET_OK.


%% 插入一条limei信息
%% App
%% Uid
%% Source
-spec encode_proto_ln(App :: netlib_string() | netlib_binary(),
                      Uid :: netlib_string() | netlib_binary(),
                      Source :: netlib_string() | netlib_binary()) ->
    netlib_iolist().
encode_proto_ln(App, Uid, Source) ->
    Data = 
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_POKER,
           ?IMDB_PROTO_LN_BIN,
           encode_item(App),
           encode_item(Uid),
           encode_item(Source)]),
    encode_item(Data).


-spec decode_proto_ln(Data :: netlib_binary()) ->
    boolean().
decode_proto_ln(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_POKER_LEN/binary, Data1/binary>> = Data,
    <<_Cmd:7/binary, Rest/binary>> = Data1,
    {_Username, RetCode} = decode_item(Rest),
    RetCode =:= ?IMDB_PROTO_RET_OK.

%% 读取limei数据
-spec encode_proto_lo(App :: netlib_string() | netlib_binary(),
                      Uid :: netlib_string() | netlib_binary(),
                      Source :: netlib_string() | netlib_binary()) ->
    netlib_iolist().
encode_proto_lo(App, Uid, Source) ->
    Data = 
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_POKER,
           ?IMDB_PROTO_LO_BIN,
           encode_item(App),
           encode_item(Uid),
           encode_item(Source)]),
    encode_item(Data).


-spec decode_proto_lo(Data :: netlib_binary()) ->
    tuple().
decode_proto_lo(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_POKER_LEN/binary, Data1/binary>> = Data,
    <<_Cmd:7/binary, Rest/binary>> = Data1,
    {_Username, Rest1} = decode_item(Rest),
    {RetCode, Rest2} = decode_item(Rest1),
    {RetCode, Rest2}.


%% 激活一条limei数据
-spec encode_proto_lp(App :: netlib_string() | netlib_binary(),
                      Uid :: netlib_string() | netlib_binary(),
                      Source :: netlib_string() | netlib_binary()) ->
    netlib_iolist().
encode_proto_lp(App, Uid, Source) ->
    Data = 
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_POKER,
           ?IMDB_PROTO_LP_BIN,
           encode_item(App),
           encode_item(Uid),
           encode_item(Source)]),
    encode_item(Data).


-spec decode_proto_lp(Data :: netlib_binary()) ->
    boolean().
decode_proto_lp(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_POKER_LEN/binary, Data1/binary>> = Data,
    <<_Cmd:7/binary, Rest/binary>> = Data1,
    {_Username, RetCode} = decode_item(Rest),
    RetCode =:= ?IMDB_PROTO_RET_OK.



%% 插入离线消息(Friend发送给Username的消息)
%% Username
%% Friend
%% Type
%% Content
-spec encode_proto_o(Username :: netlib_string() | netlib_binary(),
                     Friend :: netlib_string() | netlib_binary(),
                     Type :: netlib_string() | netlib_binary(),
                     Content :: netlib_string() | netlib_binary()) ->
    netlib_iolist().
encode_proto_o(Username, Friend, Type, Content) ->
    Data = 
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_NEWFRIENDS,
           ?IMDB_PROTO_O_BIN,
           encode_item(Username),
           encode_item(Friend),
           encode_item(Type),
           encode_item(Content)]),
    encode_item(Data).

-spec decode_proto_o(Data :: netlib_binary()) ->
    boolean().
decode_proto_o(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_NEWFRIENDS_LEN/binary, Data1/binary>> = Data,
    <<_Cmd:7/binary, Rest/binary>> = Data1,
    {_Username, RetCode} = decode_item(Rest),
    RetCode =:= ?IMDB_PROTO_RET_OK.

%% 读取Username的离线消息
-spec encode_proto_p(Username :: netlib_string() | netlib_binary()) ->
    netlib_iolist().
encode_proto_p(Username) ->
    Data = 
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_NEWFRIENDS,
           ?IMDB_PROTO_P_BIN,
           encode_item(Username)]),
    encode_item(Data).


%% [[Friend :: binary(), Type :: binary(), Content :: binary(), Timestamp :: binary()]]
-spec decode_proto_p(Data :: netlib_binary()) ->
    [[binary()]].
decode_proto_p(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_NEWFRIENDS_LEN/binary, Data1/binary>> = Data,
    <<_Cmd:7/binary, Rest/binary>> = Data1,
    {_Username, Rest1} = decode_item(Rest),
    <<RetCode:8/binary, Rest2/binary>> = Rest1,
    case RetCode =:= ?IMDB_PROTO_RET_OK of
        true ->
            decode_multi_items(Rest2);
        false ->
            []
    end.


%% 插入一条正向好友关系(Username的好哟列表里有Friend)
%% Username
%% Friend
%% Flag
%% Note
%% Group
-spec encode_proto_a(Username :: netlib_string() | netlib_binary(),
                     Friend :: netlib_string() | netlib_binary(),
                     Flag :: netlib_string() | netlib_binary(),
                     Note :: netlib_string() | netlib_binary(),
                     Group :: netlib_string() | netlib_binary()) ->
    netlib_iolist().
encode_proto_a(Username, Friend, Flag, Note, Group) ->
    Data = 
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_NEWFRIENDS,
           ?IMDB_PROTO_A_BIN,
           encode_item(Username),
           encode_item(Friend),
           encode_item(Flag),
           encode_item(Note),
           encode_item(Group)]),
    encode_item(Data).

-spec decode_proto_a(Data :: netlib_binary()) ->
    boolean().
decode_proto_a(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_NEWFRIENDS_LEN/binary, Data1/binary>> = Data,
    <<_Cmd:7/binary, Rest/binary>> = Data1,
    {_Username, RetCode} = decode_item(Rest),
    RetCode =:= ?IMDB_PROTO_RET_OK.


%% 插入一条反向好友关系(Friend的好友列表里有Username)
%% 也就是哪些人的好友列表里有Username.
%%
%% Username
%% Friend
%% Flag
-spec encode_proto_fb(Username :: netlib_string() | netlib_binary(),
                      Friend :: netlib_string() | netlib_binary(),
                      Flag :: netlib_string() | netlib_binary()) ->
    netlib_iolist().
encode_proto_fb(Username, Friend, Flag) ->
    Data = 
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_NEWFRIENDS,
           ?IMDB_PROTO_FB_BIN,
           encode_item(Username),
           encode_item(Friend),
           encode_item(Flag)]),
    encode_item(Data).

-spec decode_proto_fb(Data :: netlib_binary()) ->
    boolean().
decode_proto_fb(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_NEWFRIENDS_LEN/binary, Data1/binary>> = Data,
    <<_Cmd:7/binary, Rest/binary>> = Data1,
    {_Username, RetCode} = decode_item(Rest),
    RetCode =:= ?IMDB_PROTO_RET_OK.


%% 取正向好友列表
-spec encode_proto_g(Username :: netlib_string() | netlib_binary()) ->
    netlib_iolist().
encode_proto_g(Username) ->
    Data = 
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_NEWFRIENDS,
           ?IMDB_PROTO_G_BIN,
           encode_item(Username)]),
    encode_item(Data).


-spec decode_proto_g(Data :: netlib_binary()) ->
    [[binary()]].
decode_proto_g(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_NEWFRIENDS_LEN/binary, Data1/binary>> = Data,
    <<_Cmd:7/binary, Rest/binary>> = Data1,
    {_Username, Rest1} = decode_item(Rest),
    <<RetCode:8/binary, Rest2/binary>> = Rest1,
    case RetCode =:= ?IMDB_PROTO_RET_OK of
        true ->
            decode_multi_items(Rest2);
        false ->
            []
    end.

%% 取反向好友列表
-spec encode_proto_j(Username :: netlib_string() | netlib_binary()) ->
    netlib_iolist().
encode_proto_j(Username) ->
    Data = 
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_NEWFRIENDS,
           ?IMDB_PROTO_J_BIN,
           encode_item(Username)]),
    encode_item(Data).


-spec decode_proto_j(Data :: netlib_binary()) ->
    [[binary()]].
decode_proto_j(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_NEWFRIENDS_LEN/binary, Data1/binary>> = Data,
    <<_Cmd:7/binary, Rest/binary>> = Data1,
    {_Username, Rest1} = decode_item(Rest),
    <<RetCode:8/binary, Rest2/binary>> = Rest1,
    case RetCode =:= ?IMDB_PROTO_RET_OK of
        true ->
            decode_multi_items(Rest2);
        false ->
            []
    end.


%% 删除一条正向好友关系
-spec encode_proto_fe(Username :: netlib_string() | netlib_binary(),
                     Friend :: netlib_string() | netlib_binary()) ->
    netlib_iolist().
encode_proto_fe(Username, Friend) ->
    Data = 
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_NEWFRIENDS,
           ?IMDB_PROTO_FE_BIN,
           encode_item(Username),
           encode_item(Friend)]),
    encode_item(Data).

-spec decode_proto_fe(Data :: netlib_binary()) ->
    boolean().
decode_proto_fe(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_NEWFRIENDS_LEN/binary, Data1/binary>> = Data,
    <<_Cmd:7/binary, Rest/binary>> = Data1,
    {_Username, RetCode} = decode_item(Rest),
    RetCode =:= ?IMDB_PROTO_RET_OK.

%% 删除一条反向好友关系
-spec encode_proto_f(Username :: netlib_string() | netlib_binary(),
                     Friend :: netlib_string() | netlib_binary()) ->
    netlib_iolist().
encode_proto_f(Username, Friend) ->
    Data = 
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_NEWFRIENDS,
           ?IMDB_PROTO_F_BIN,
           encode_item(Username),
           encode_item(Friend)]),
    encode_item(Data).

-spec decode_proto_f(Data :: netlib_binary()) ->
    boolean().
decode_proto_f(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_NEWFRIENDS_LEN/binary, Data1/binary>> = Data,
    <<_Cmd:7/binary, Rest/binary>> = Data1,
    {_Username, RetCode} = decode_item(Rest),
    RetCode =:= ?IMDB_PROTO_RET_OK.


%% 修改正向好友关系属性
%% Field: flag | note | f_group
-spec encode_proto_fc(Username :: netlib_string() | netlib_binary(),
                     Friend :: netlib_string() | netlib_binary(),
                     Field :: netlib_string() | netlib_binary(),
                     Content :: netlib_string() | netlib_binary()) ->
    netlib_iolist().
encode_proto_fc(Username, Friend, Field, Content) ->
    Data = 
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_NEWFRIENDS,
           ?IMDB_PROTO_FC_BIN,
           encode_item(Username),
           encode_item(Friend),
           encode_item(Field),
           encode_item(Content)]),
    encode_item(Data).

-spec decode_proto_fc(Data :: netlib_binary()) ->
    boolean().
decode_proto_fc(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_NEWFRIENDS_LEN/binary, Data1/binary>> = Data,
    <<_Cmd:7/binary, Rest/binary>> = Data1,
    {_Username, RetCode} = decode_item(Rest),
    RetCode =:= ?IMDB_PROTO_RET_OK.


%% 修改一条反向好友关系flag属性
%% Field: flag 
-spec encode_proto_d(Username :: netlib_string() | netlib_binary(),
                     Friend :: netlib_string() | netlib_binary(),
                     Field :: netlib_string() | netlib_binary(),
                     Content :: netlib_string() | netlib_binary()) ->
    netlib_iolist().
encode_proto_d(Username, Friend, Field, Content) ->
    Data = 
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_NEWFRIENDS,
           ?IMDB_PROTO_D_BIN,
           encode_item(Username),
           encode_item(Friend),
           encode_item(Field),
           encode_item(Content)]),
    encode_item(Data).

-spec decode_proto_d(Data :: netlib_binary()) ->
    boolean().
decode_proto_d(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_NEWFRIENDS_LEN/binary, Data1/binary>> = Data,
    <<_Cmd:7/binary, Rest/binary>> = Data1,
    {_Username, RetCode} = decode_item(Rest),
    RetCode =:= ?IMDB_PROTO_RET_OK.


%% 读取Username和Friend之间的好友关系.
-spec encode_proto_h(
        Username :: netlib_string() | netlib_binary(),
        Friend :: netlib_string() | netlib_binary()) ->
    netlib_iolist().
encode_proto_h(Username, Friend) ->
    Data =
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_NEWFRIENDS,
           ?IMDB_PROTO_H_BIN,
           encode_item(Username),
           encode_item(Friend)]),
    encode_item(Data).


%% [[Flag :: binary(), Note :: binary(), Group :: binary()]]
-spec decode_proto_h(Data :: netlib_binary()) ->
    [[binary()]].
decode_proto_h(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_NEWFRIENDS_LEN/binary, Data1/binary>> = Data,
    <<_Cmd:7/binary, Rest/binary>> = Data1,
    {_Username, Rest1} = decode_item(Rest),
    <<RetCode:8/binary, Rest2/binary>> = Rest1,
    case RetCode =:= ?IMDB_PROTO_RET_OK of
        true ->
            decode_multi_items(Rest2);
        false ->
            []
    end.


%% 插入通知消息(Friend发给Username的消息)
-spec encode_proto_la(Username :: netlib_string() | netlib_binary(),
                      Friend :: netlib_string() | netlib_binary(),
                      Type :: netlib_string() | netlib_binary(),
                      Content :: netlib_string() | netlib_binary()) ->
    netlib_iolist().
encode_proto_la(Username, Friend, Type, Content) ->
    Data = 
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_NEWFRIENDS,
           ?IMDB_PROTO_LA_BIN,
           encode_item(Username),
           encode_item(Friend),
           encode_item(Type),
           encode_item(Content)]),
    encode_item(Data).

-spec decode_proto_la(Data :: netlib_binary()) ->
    boolean().
decode_proto_la(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_NEWFRIENDS_LEN/binary, Data1/binary>> = Data,
    <<_Cmd:7/binary, Rest/binary>> = Data1,
    {_Username, RetCode} = decode_item(Rest),
    RetCode =:= ?IMDB_PROTO_RET_OK.


%% 读取通知消息列表
-spec encode_proto_lb(
        Username :: netlib_string() | netlib_binary(),
        Begin :: netlib_string() | netlib_binary(),
        Limit :: netlib_string() | netlib_binary()) ->
    netlib_iolist().
encode_proto_lb(Username, Begin, Limit) ->
    Data =
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_NEWFRIENDS,
           ?IMDB_PROTO_LB_BIN,
           encode_item(Username),
           encode_item(Begin),
           encode_item(Limit)]),
    encode_item(Data).


-spec decode_proto_lb(Data :: netlib_binary()) ->
    [[binary()]].
decode_proto_lb(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_NEWFRIENDS_LEN/binary, Data1/binary>> = Data,
    <<_Cmd:7/binary, Rest/binary>> = Data1,
    {_Username, Rest1} = decode_item(Rest),
    <<RetCode:8/binary, Rest2/binary>> = Rest1,
    case RetCode =:= ?IMDB_PROTO_RET_OK of
        true ->
            decode_multi_items(Rest2);
        false ->
            []
    end.

%% 删除通知消息
-spec encode_proto_lc(Username :: netlib_string() | netlib_binary(),
                      Id :: netlib_string() | netlib_binary()) ->
    netlib_iolist().
encode_proto_lc(Username, Id) ->
    Data = 
        iolist_to_binary(
          [?IMDB_PROTO_PROXY_M_NEWFRIENDS,
           ?IMDB_PROTO_LC_BIN,
           encode_item(Username),
           encode_item(Id)]),
    encode_item(Data).

-spec decode_proto_lc(Data :: netlib_binary()) ->
    boolean().
decode_proto_lc(Data) when is_binary(Data) ->
    <<_ProxyPrefix:?IMDB_PROTO_PROXY_M_NEWFRIENDS_LEN/binary, Data1/binary>> = Data,
    <<_Cmd:7/binary, Rest/binary>> = Data1,
    {_Username, RetCode} = decode_item(Rest),
    RetCode =:= ?IMDB_PROTO_RET_OK.



%% encoder
-spec encode_item(Item :: netlib_binary() | netlib_string()) -> 
    netlib_iolist().
encode_item(Item) ->
    [int_to_protohex(imdb_util:length(Item)),
     Item].


-spec encode_items(Items :: [netlib_binary() | netlib_string()]) -> 
    netlib_iolist().
encode_items(Items) ->
    lists:map(fun(Item) ->
                  [int_to_protohex(imdb_util:length(Item)),
                   Item]
              end, Items).


-spec encode_kvs(KVs :: [{K :: netlib_binary() | netlib_string(), 
                          V :: netlib_binary() | netlib_string()}]) -> 
    netlib_iolist().
encode_kvs(KVs) ->
    lists:map(fun({K, V}) ->
                  [int_to_protohex(imdb_util:length(K)),
                   K,
                   int_to_protohex(imdb_util:length(V)),
                   V]
              end, KVs).

%% decoder

-spec decode_item(Data :: netlib_binary()) ->
    {Item :: netlib_binary() | undefined,
     Rest :: netlib_binary()}.
decode_item(<<>>) ->
    {undefined, <<>>};
decode_item(<<LenBin:?IMDB_PROTO_PERFIX/binary, Rest/binary>>) ->
    case LenBin =:= ?IMDB_PROTO_EMPTY_ITEM of
	true ->
	    {<<>>, Rest}; 
	false ->
	    Len = protohex_to_int(LenBin),
            <<Item:Len/binary, Rest1/binary>> = Rest,
            {Item, Rest1}
    end.

-spec decode_kv(Data :: netlib_binary()) ->
    {{Key :: netlib_binary() | undefined,
      Val :: netlib_binary() | undefined},
      Rest :: netlib_binary()}.
decode_kv(<<>>) ->
    {{undefined, undefined}, <<>>};
decode_kv(Data) when is_binary(Data) ->
    {Key, Rest} = decode_item(Data),
    {Val, Rest1} = decode_item(Rest),
    {{Key, Val}, Rest1}.


-spec decode_kvs(Data :: netlib_binary()) ->
    [{K :: netlib_binary(),
      V :: netlib_binary()}].
decode_kvs(Data) when is_binary(Data) ->
    decode_kvs(Data, []).

decode_kvs(<<>>, Acc) ->    
    lists:reverse(Acc);
decode_kvs(Data, Acc) when is_binary(Data) ->
    {KV, Rest} = decode_kv(Data),
    decode_kvs(Rest, [KV | Acc]).

-spec decode_multi_items(Data :: netlib_binary()) ->
    [netlib_list()].
decode_multi_items(<<_TotalLen:?IMDB_PROTO_PERFIX/binary, Rest/binary>>) ->
    decode_multi_items(Rest, []).

decode_multi_items(<<>>, Acc) ->
    lists:reverse(Acc);
decode_multi_items(<<RowLenLenBin:?IMDB_PROTO_PERFIX/binary, Rest/binary>>, Acc) ->
    RowLenLen = protohex_to_int(RowLenLenBin),
    <<RowLenBin:RowLenLen/binary, Rest1/binary>> = Rest, 
    RowLen = imdb_util:to_integer(RowLenBin),
    <<RowBin:RowLen/binary, Rest2/binary>> = Rest1,
    decode_multi_items(Rest2, [decode_items(RowBin) | Acc]).

-spec decode_items(Data :: netlib_binary()) ->
    [V :: netlib_binary()].
decode_items(Data) when is_binary(Data) ->
    decode_items(Data, []).

decode_items(<<>>, Acc) ->
    lists:reverse(Acc);
decode_items(Data, Acc) when is_binary(Data) ->
    {Item, Rest} = decode_item(Data),
    decode_items(Rest, [Item | Acc]).
    
-spec protohex_to_int(Data :: netlib_binary() |  netlib_string()) -> netlib_integer().
protohex_to_int(Data) when is_binary(Data) ->
    protohex_to_int(binary_to_list(Data));
protohex_to_int(Data) when is_list(Data) andalso length(Data) =:= 6 ->
    protohex_to_int(lists:reverse(Data), 1, 0).

-spec int_to_protohex(N :: netlib_integer()) -> netlib_string().
int_to_protohex(N) ->
    int_to_protohex(N, []).


protohex_to_int([], _, Sum) ->
    Sum;
protohex_to_int([H|T], Tag, Sum) ->
    protohex_to_int(T, Tag * 16, (to_integer(H) * Tag + Sum)).

int_to_protohex(_, Acc) when length(Acc) =:= 6 ->
    Acc;
int_to_protohex(0, Acc) ->
    int_to_protohex(0, [$0 | Acc]);
int_to_protohex(N, Acc) ->
    int_to_protohex((N div 16), [to_digit(N rem 16) | Acc]).


to_digit(N) when N < 10 -> $0 + N;
to_digit(N)             -> $a + N - 10.

to_integer(N) when N =< $9 -> N - $0;
to_integer(N)              ->10 + (N - $a).
