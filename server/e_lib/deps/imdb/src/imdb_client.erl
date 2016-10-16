-module(imdb_client).
-author('erlangonrails@gmail.com').
-include("imdb.hrl").
-include("netlib.hrl").

-export([sys_a/1,
         filter_f/1,
         proto_1/1,
         proto_5/1,
         proto_6/1,
         score_r/1,
         score_a/1,
         score_d/1,
         score_a_ext/1,
         score_d_ext/1,
         proto_r/1,
         proto_w/1,
         proto_update/1,
         proto_b/1,
         proto_c/1,
         proto_e/1,
         proto_q/1,
         proto_y/1,
         proto_yy/1,
         proto_aa/1,
         proto_ab/1,
         proto_ac/1,
         proto_ad/1,
         proto_af/1,
         proto_ag/1,
         proto_ai/1,
		 proto_al/1,
		 proto_am/1,

         proto_po/1,
         proto_pr/1,
         proto_t/1,
         proto_o/1,
         proto_p/1,
         proto_a/1,
         proto_fb/1,
         proto_g/1,
         proto_j/1,
         proto_fe/1,
         proto_f/1,
         proto_fc/1,
         proto_d/1,
         proto_h/1,
         proto_hp/1,
         proto_la/1,
         proto_lb/1,
         proto_lc/1,
         proto_ln/1, 
         proto_lo/1, 
         proto_lp/1]). 


-spec sys_a(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
sys_a(Iolist) ->
    sys_a(imdb_util:get_imdb_proxy(), Iolist).

-spec sys_a(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
sys_a(Address, Iolist) ->
    netlib_gen_client:call(Address, {sys_a, Iolist}).


-spec filter_f(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
filter_f(Iolist) ->
    filter_f(imdb_util:get_imdb_proxy(), Iolist).

-spec filter_f(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
filter_f(Address, Iolist) ->
    netlib_gen_client:call(Address, {filter_f, Iolist}).


-spec proto_1(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_1(Iolist) ->
    proto_1(imdb_util:get_imdb_proxy(), Iolist).

-spec proto_1(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_1(Address, Iolist) ->
    netlib_gen_client:call(Address, {proto_1, Iolist}).


-spec proto_5(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_5(Iolist) ->
    proto_5(imdb_util:get_imdb_proxy(), Iolist).

-spec proto_5(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_5(Address, Iolist) ->
    netlib_gen_client:call(Address, {proto_5, Iolist}).



-spec proto_6(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_6(Iolist) ->
    proto_6(imdb_util:get_imdb_proxy(), Iolist).

-spec proto_6(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_6(Address, Iolist) ->
    netlib_gen_client:call(Address, {proto_6, Iolist}).


-spec score_r(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
score_r(Iolist) ->
    score_r(imdb_util:get_imdb_proxy(), Iolist).

-spec score_r(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
score_r(Address, Iolist) ->
    netlib_gen_client:call(Address, {score_r, Iolist}).


-spec score_a(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
score_a(Iolist) ->
    score_a(imdb_util:get_imdb_proxy(), Iolist).

-spec score_a(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
score_a(Address, Iolist) ->
    netlib_gen_client:call(Address, {score_a, Iolist}).

-spec score_d(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
score_d(Iolist) ->
    score_d(imdb_util:get_imdb_proxy(), Iolist).

-spec score_d(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
score_d(Address, Iolist) ->
    netlib_gen_client:call(Address, {score_d, Iolist}).


-spec score_a_ext(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
score_a_ext(Iolist) ->
    score_a_ext(imdb_util:get_imdb_proxy(), Iolist).

-spec score_a_ext(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
score_a_ext(Address, Iolist) ->
    netlib_gen_client:call(Address, {score_a_ext, Iolist}).


-spec score_d_ext(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
score_d_ext(Iolist) ->
    score_d_ext(imdb_util:get_imdb_proxy(), Iolist).

-spec score_d_ext(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
score_d_ext(Address, Iolist) ->
    netlib_gen_client:call(Address, {score_d_ext, Iolist}).

-spec proto_r(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_r(Iolist) ->
    proto_r(imdb_util:get_imdb_proxy(), Iolist).

-spec proto_r(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_r(Address, Iolist) ->
    netlib_gen_client:call(Address, {proto_r, Iolist}).


-spec proto_w(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_w(Iolist) ->
    proto_w(imdb_util:get_imdb_proxy(), Iolist).

-spec proto_w(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_w(Address, Iolist) ->
    netlib_gen_client:call(Address, {proto_w, Iolist}).


-spec proto_update(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_update(Iolist) ->
    proto_update(imdb_util:get_imdb_proxy(), Iolist).

-spec proto_update(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_update(Address, Iolist) ->
    netlib_gen_client:call(Address, {proto_update, Iolist}).

-spec proto_b(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_b(Iolist) ->
    proto_b(imdb_util:get_imdb_proxy(), Iolist).

-spec proto_b(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_b(Address, Iolist) ->
    netlib_gen_client:call(Address, {proto_b, Iolist}).


-spec proto_c(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_c(Iolist) ->
    proto_c(imdb_util:get_imdb_proxy(), Iolist).

-spec proto_c(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_c(Address, Iolist) ->
    netlib_gen_client:call(Address, {proto_c, Iolist}).


-spec proto_e(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_e(Iolist) ->
    proto_e(imdb_util:get_imdb_proxy(), Iolist).

-spec proto_e(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_e(Address, Iolist) ->
    netlib_gen_client:call(Address, {proto_e, Iolist}).

-spec proto_q(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_q(Iolist) ->
    proto_q(imdb_util:get_imdb_proxy(), Iolist).

-spec proto_q(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_q(Address, Iolist) ->
    netlib_gen_client:call(Address, {proto_q, Iolist}).


-spec proto_yy(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_yy(Iolist) ->
    proto_yy(imdb_util:get_imdb_proxy(), Iolist).

-spec proto_yy(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_yy(Address, Iolist) ->
    netlib_gen_client:call(Address, {proto_yy, Iolist}).


-spec proto_y(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_y(Iolist) ->
    proto_y(imdb_util:get_imdb_proxy(), Iolist).

-spec proto_y(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_y(Address, Iolist) ->
    netlib_gen_client:call(Address, {proto_y, Iolist}).

-spec proto_aa(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_aa(Iolist) ->
    proto_aa(imdb_util:get_imdb_proxy(), Iolist).

-spec proto_aa(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_aa(Address, Iolist) ->
    netlib_gen_client:call(Address, {proto_aa, Iolist}).


-spec proto_ab(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_ab(Iolist) ->
    proto_ab(imdb_util:get_imdb_proxy(), Iolist).

-spec proto_ab(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_ab(Address, Iolist) ->
    netlib_gen_client:call(Address, {proto_ab, Iolist}).

-spec proto_ac(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_ac(Iolist) ->
    proto_ac(imdb_util:get_imdb_proxy(), Iolist).

-spec proto_ac(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_ac(Address, Iolist) ->
    netlib_gen_client:call(Address, {proto_ac, Iolist}).

-spec proto_ad(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_ad(Iolist) ->
    proto_ad(imdb_util:get_imdb_proxy(), Iolist).

-spec proto_ad(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_ad(Address, Iolist) ->
    netlib_gen_client:call(Address, {proto_ad, Iolist}).

-spec proto_af(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_af(Iolist) ->
    proto_af(imdb_util:get_imdb_proxy(), Iolist).

-spec proto_af(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_af(Address, Iolist) ->
    netlib_gen_client:call(Address, {proto_af, Iolist}).

-spec proto_ag(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_ag(Iolist) ->
    proto_ag(imdb_util:get_imdb_proxy(), Iolist).

-spec proto_ag(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_ag(Address, Iolist) ->
    netlib_gen_client:call(Address, {proto_ag, Iolist}).

-spec proto_ai(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_ai(Iolist) ->
    proto_ai(imdb_util:get_imdb_proxy(), Iolist).

-spec proto_ai(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_ai(Address, Iolist) ->
    netlib_gen_client:call(Address, {proto_ai, Iolist}).

proto_al(IoList) ->
	proto_al(imdb_util:get_imdb_proxy(), IoList).

proto_al(Address, Iolist) ->
	netlib_gen_client:call(Address, {proto_al, Iolist}).

proto_am(IoList) ->
	proto_am(imdb_util:get_imdb_proxy(), IoList).

proto_am(Address, Iolist) ->
	netlib_gen_client:call(Address, {proto_am, Iolist}).

-spec proto_po(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_po(Iolist) ->
    proto_po(imdb_util:get_imdb_proxy(), Iolist).

-spec proto_po(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_po(Address, Iolist) ->
    netlib_gen_client:call(Address, {proto_po, Iolist}).


-spec proto_pr(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_pr(Iolist) ->
    proto_pr(imdb_util:get_imdb_proxy(), Iolist).

-spec proto_pr(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_pr(Address, Iolist) ->
    netlib_gen_client:call(Address, {proto_pr, Iolist}).


-spec proto_t(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_t(Iolist) ->
    proto_t(imdb_util:get_imdb_proxy(), Iolist).

-spec proto_t(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_t(Address, Iolist) ->
    netlib_gen_client:call(Address, {proto_t, Iolist}).


-spec proto_o(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_o(Iolist) ->
    proto_o(imdb_util:get_imdb_proxy(), Iolist).

-spec proto_o(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_o(Address, Iolist) ->
    netlib_gen_client:call(Address, {proto_o, Iolist}).

%% [[Friend :: binary(), Type :: binary(), Content :: binary(), Timestamp :: binary()]]
-spec proto_p(Iolist :: netlib_iolist()) ->
    [[binary()]] | undefined.
proto_p(Iolist) ->
    proto_p(imdb_util:get_imdb_proxy(), Iolist).


%% [[Friend :: binary(), Type :: binary(), Content :: binary(), Timestamp :: binary()]]
-spec proto_p(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    [[binary()]] | undefined.
proto_p(Address, Iolist) ->
    netlib_gen_client:call(Address, {proto_p, Iolist}).


-spec proto_a(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_a(Iolist) ->
    proto_a(imdb_util:get_imdb_proxy(), Iolist).

-spec proto_a(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_a(Address, Iolist) ->
    netlib_gen_client:call(Address, {proto_a, Iolist}).


-spec proto_fb(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_fb(Iolist) ->
    proto_fb(imdb_util:get_imdb_proxy(), Iolist).

-spec proto_fb(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_fb(Address, Iolist) ->
    netlib_gen_client:call(Address, {proto_fb, Iolist}).

-spec proto_g(Iolist :: netlib_iolist()) ->
    [[binary()]] | undefined.
proto_g(Iolist) ->
    proto_g(imdb_util:get_imdb_proxy(), Iolist).

-spec proto_g(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    [[binary()]] | undefined.
proto_g(Address, Iolist) ->
    netlib_gen_client:call(Address, {proto_g, Iolist}).

-spec proto_j(Iolist :: netlib_iolist()) ->
    [[binary()]] | undefined.
proto_j(Iolist) ->
    proto_j(imdb_util:get_imdb_proxy(), Iolist).

-spec proto_j(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    [[binary()]] | undefined.
proto_j(Address, Iolist) ->
    netlib_gen_client:call(Address, {proto_j, Iolist}).


-spec proto_fe(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_fe(Iolist) ->
    proto_fe(imdb_util:get_imdb_proxy(), Iolist).

-spec proto_fe(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_fe(Address, Iolist) ->
    netlib_gen_client:call(Address, {proto_fe, Iolist}).


-spec proto_f(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_f(Iolist) ->
    proto_f(imdb_util:get_imdb_proxy(), Iolist).

-spec proto_f(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_f(Address, Iolist) ->
    netlib_gen_client:call(Address, {proto_f, Iolist}).


-spec proto_fc(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_fc(Iolist) ->
    proto_fc(imdb_util:get_imdb_proxy(), Iolist).

-spec proto_fc(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_fc(Address, Iolist) ->
    netlib_gen_client:call(Address, {proto_fc, Iolist}).

-spec proto_d(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_d(Iolist) ->
    proto_d(imdb_util:get_imdb_proxy(), Iolist).

-spec proto_d(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_d(Address, Iolist) ->
    netlib_gen_client:call(Address, {proto_d, Iolist}).

-spec proto_h(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_h(Iolist) ->
    proto_h(imdb_util:get_imdb_proxy(), Iolist).

-spec proto_h(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_h(Address, Iolist) ->
    netlib_gen_client:call(Address, {proto_h, Iolist}).


-spec proto_hp(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_hp(Iolist) ->
    proto_hp(imdb_util:get_imdb_proxy(), Iolist).

-spec proto_hp(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_hp(Address, Iolist) ->
    netlib_gen_client:call(Address, {proto_hp, Iolist}).


-spec proto_la(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_la(Iolist) ->
    proto_la(imdb_util:get_imdb_proxy(), Iolist).

-spec proto_la(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_la(Address, Iolist) ->
    netlib_gen_client:call(Address, {proto_la, Iolist}).


-spec proto_lb(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_lb(Iolist) ->
    proto_lb(imdb_util:get_imdb_proxy(), Iolist).

-spec proto_lb(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_lb(Address, Iolist) ->
    netlib_gen_client:call(Address, {proto_lb, Iolist}).


-spec proto_lc(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_lc(Iolist) ->
    proto_lc(imdb_util:get_imdb_proxy(), Iolist).

-spec proto_lc(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_lc(Address, Iolist) ->
    netlib_gen_client:call(Address, {proto_lc, Iolist}).


-spec proto_ln(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_ln(Iolist) ->
    proto_ln(imdb_util:get_imdb_proxy(), Iolist).

-spec proto_ln(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_ln(Address, Iolist) ->
    netlib_gen_client:call(Address, {proto_ln, Iolist}).


-spec proto_lo(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_lo(Iolist) ->
    proto_lo(imdb_util:get_imdb_proxy(), Iolist).

-spec proto_lo(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_lo(Address, Iolist) ->
    netlib_gen_client:call(Address, {proto_lo, Iolist}).


-spec proto_lp(Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_lp(Iolist) ->
    proto_lp(imdb_util:get_imdb_proxy(), Iolist).

-spec proto_lp(Address :: netlib_address() | netlib_binary(), Iolist :: netlib_iolist()) ->
    boolean() | undefined.
proto_lp(Address, Iolist) ->
    netlib_gen_client:call(Address, {proto_lp, Iolist}).

