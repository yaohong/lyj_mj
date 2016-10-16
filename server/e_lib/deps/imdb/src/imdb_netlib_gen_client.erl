-module(imdb_netlib_gen_client).
-author('erlangonrails@gmail.com').
-include("imdb.hrl").
-include("netlib.hrl").
-include("file_log.hrl").
-export([start/0]).
-export([tcp_start/4,
         do_send_recv/2,
         do_ping/1,
         tcp_close/1]).

-record(state, {socket :: netlib_socket(),
                address :: netlib_address()}).

%% APIs:
-spec start() ->
    netlib_ret_ok(netlib_pid()).
start() ->
    ImdbProxy = imdb_util:get_imdb_proxy(),
    PoolSize = imdb_util:get_imdb_pool_size(),
    StartInterval = imdb_util:get_imdb_start_interval(),
    io:format("~p,~p,~p", [ImdbProxy, PoolSize, StartInterval]),
    netlib_gen_client:start(ImdbProxy, PoolSize, StartInterval, 30, ?MODULE, []).

%% netlib_gen_client callback
tcp_start(ok, Socket, Address, _Opts) ->
    ?FILE_LOG_INFO("imdb_tcp_start:~p", [Address]),
    {ok, #state{socket = Socket, address = Address}};
tcp_start({error, Reason}, StartInterval, Address, _Opts) ->
    ?FILE_LOG_WARNING("imdb_tcp_start failed due to:~p~n"
                      "** retry after ~p seconds", [Reason, StartInterval]),
    {ok, #state{address = Address}}.



do_send_recv({sys_a, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_sys_a(RecvData),
    {ok, State, Ret};
do_send_recv({filter_f, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_filter_f(RecvData),
    {ok, State, Ret};
do_send_recv({proto_1, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_proto_1(RecvData),
    {ok, State, Ret};
do_send_recv({proto_5, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_proto_5(RecvData),
    {ok, State, Ret};
do_send_recv({proto_6, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_proto_6(RecvData),
    {ok, State, Ret};
do_send_recv({score_r, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_score_r(RecvData),
    {ok, State, Ret};
do_send_recv({score_a, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_score_a(RecvData),
    {ok, State, Ret};
do_send_recv({score_d, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_score_d(RecvData),
    {ok, State, Ret};
do_send_recv({score_a_ext, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_score_a_ext(RecvData),
    {ok, State, Ret};
do_send_recv({score_d_ext, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_score_d_ext(RecvData),
    {ok, State, Ret};
do_send_recv({proto_r, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_proto_r(RecvData),
    {ok, State, Ret};
do_send_recv({proto_w, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_proto_w(RecvData),
    {ok, State, Ret};
do_send_recv({proto_update, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_proto_update(RecvData),
    {ok, State, Ret};
do_send_recv({proto_b, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_proto_b(RecvData),
    {ok, State, Ret};
do_send_recv({proto_c, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_proto_c(RecvData),
    {ok, State, Ret};
do_send_recv({proto_e, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_proto_e(RecvData),
    {ok, State, Ret};
do_send_recv({proto_po, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_proto_po(RecvData),
    {ok, State, Ret};
do_send_recv({proto_pr, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_proto_pr(RecvData),
    {ok, State, Ret};
do_send_recv({proto_t, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_proto_t(RecvData),
    {ok, State, Ret};
do_send_recv({proto_o, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_proto_o(RecvData),
    {ok, State, Ret};
do_send_recv({proto_p, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_proto_p(RecvData),
    {ok, State, Ret};
do_send_recv({proto_a, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_proto_a(RecvData),
    {ok, State, Ret};
do_send_recv({proto_fb, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_proto_fb(RecvData),
    {ok, State, Ret};
do_send_recv({proto_g, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_proto_g(RecvData),
    {ok, State, Ret};
do_send_recv({proto_q, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_proto_q(RecvData),
    {ok, State, Ret};

do_send_recv({proto_yy, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_proto_yy(RecvData),
    {ok, State, Ret};


do_send_recv({proto_y, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_proto_y(RecvData),
    {ok, State, Ret};

do_send_recv({proto_aa, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_amh_aa(RecvData),
    {ok, State, Ret};

do_send_recv({proto_ab, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_amh_ab(RecvData),
    {ok, State, Ret};

do_send_recv({proto_ac, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_amh_ac(RecvData),
    {ok, State, Ret};
do_send_recv({proto_ad, Iolist}, #state{socket = Socket} = State) ->
	RecvData = do_send_and_recv(Socket, Iolist),
	Ret = imdb_proto:decode_amh_ad(RecvData),
	{ok, State, Ret};
do_send_recv({proto_af, Iolist}, #state{socket = Socket} = State) ->
	RecvData = do_send_and_recv(Socket, Iolist),
	Ret = imdb_proto:decode_amh_af(RecvData),
	{ok, State, Ret};
do_send_recv({proto_ag, Iolist}, #state{socket = Socket} = State) ->
	RecvData = do_send_and_recv(Socket, Iolist),
	Ret = imdb_proto:decode_amh_ag(RecvData),
	{ok, State, Ret};
do_send_recv({proto_ai, Iolist}, #state{socket = Socket} = State) ->
	RecvData = do_send_and_recv(Socket, Iolist),
	Ret = imdb_proto:decode_amh_ai(RecvData),
	{ok, State, Ret};
do_send_recv({proto_al, Iolist}, #state{socket = Socket} = State) ->
	RecvData = do_send_and_recv(Socket, Iolist),
	Ret = imdb_proto:decode_amh_al(RecvData),
	{ok, State, Ret};
do_send_recv({proto_am, Iolist}, #state{socket = Socket} = State) ->
	RecvData = do_send_and_recv(Socket, Iolist),
	Ret = imdb_proto:decode_amh_am(RecvData),
	{ok, State, Ret};


do_send_recv({proto_j, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_proto_j(RecvData),
    {ok, State, Ret};
do_send_recv({proto_fe, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_proto_fe(RecvData),
    {ok, State, Ret};
do_send_recv({proto_f, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_proto_f(RecvData),
    {ok, State, Ret};
do_send_recv({proto_fc, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_proto_fc(RecvData),
    {ok, State, Ret};
do_send_recv({proto_d, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_proto_d(RecvData),
    {ok, State, Ret};
do_send_recv({proto_h, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_proto_h(RecvData),
    {ok, State, Ret};

do_send_recv({proto_hp, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_proto_hp(RecvData),
    {ok, State, Ret};

do_send_recv({proto_la, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_proto_la(RecvData),
    {ok, State, Ret};
do_send_recv({proto_lb, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_proto_lb(RecvData),
    {ok, State, Ret};
do_send_recv({proto_lc, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_proto_lc(RecvData),
    {ok, State, Ret};
do_send_recv({proto_ln, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_proto_ln(RecvData),
    {ok, State, Ret};
do_send_recv({proto_lo, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_proto_lo(RecvData),
    {ok, State, Ret};
do_send_recv({proto_lp, Iolist}, #state{socket = Socket} = State) ->
    RecvData = do_send_and_recv(Socket, Iolist),
    Ret = imdb_proto:decode_proto_lp(RecvData),
    {ok, State, Ret}.

do_ping(#state{address = _Address,
               socket = Socket} = State) ->
    %% ping消息必须用原始的Socket发送.
    Bin = imdb_proto:encode_proto_r("100053", ["user_money"]),
    do_send_and_recv(Socket, Bin),
    {ok, State}.

tcp_close(#state{address = Address} = _State) ->
    ?FILE_LOG_WARNING("imdb_tcp_close:~p", [Address]),
    ok.

%% Internal APIs:
do_send_and_recv(Socket, Iolist) ->
    ok = netlib_gen_client:do_send(Socket, Iolist),
    LenBin = netlib_gen_client:do_recv(Socket, ?IMDB_PROTO_PERFIX),
    netlib_gen_client:do_recv(Socket, imdb_proto:protohex_to_int(LenBin)).
    
