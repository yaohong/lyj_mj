-module(imdb_api).
-author("erlangonrails@gmail.com").
-export([log_score/4,
         filter/2,
         
         iap_add/10,
         iap_check/2,

         check_ip/1,
         limei_add/3,
         limei_get/3,
         limei_reg/3,

         score_read/1,
         score_add/2,
         score_add_ext/3,
         score_sub/2,
         score_sub_ext/3,

         log_login/3,
         log_score/2,

         get_id_by_email/1,
         get_id_by_device/1,
         get_data_by_id/1,
         get_best_list/1,
         read/2,
         write/2,
         update/2,

         update_password/2,
         get_gift_info/1,

         get_give_score/1,
         insert_give_score/4,


		 insert_amh_giving/4,
         get_amh_giving/1,
		 delete_amh_giving/2,

         get_amh_toplist/1,
         get_feedback_request/1,
         insert_feedback_request/3,
         get_feedback_answer/1,

		 update_display_pos/2,
		 get_display_pos_list/1,

         %% 离线消息
         add_offline/4,
         get_offline/1,

         %% 好友状态相关的APIs
         add_friend/4,
         add_friend/6,
         del_friend/2,
         get_friend/1,
         get_friend/2,
         get_rfriend/1,

         %% 好友状态更新相关的APIs
         update_friend_flag/3,
         update_rfriend_flag/3,
         update_friend_note/3,
         update_friend_group/3,


         add_friend_note/3,
         get_add_friend_note/2,
         agree_friend_note/3,
         reject_friend_note/3,
         get_pending_notes/1,
         del_note/2]).

log_score(Id, Type, Coin, Source) ->
    {Date, _Time} = calendar:now_to_local_time(now()),
    DateFmt = io_lib:fwrite("~w~.2.0w~.2.0w ", tuple_to_list(Date)),
    Bin = imdb_proto:encode_sys_a(Id, Type, Coin, Source, iolist_to_binary(DateFmt)),
    imdb_client:sys_a(Bin).

-spec filter(
        Username :: string() | binary(),
        Message :: string() | binary()) ->
    todo.
filter(Username, Message) ->
    Encode = "utf8", 
    Channel = "2",
    Type = "10",
    Bin = imdb_proto:encode_filter_f(Username, Encode, Channel, Type, Message),
    imdb_client:filter_f(Bin).


%% 所有的参数都是string() | binary()
iap_add(Channel, OrderId, Game, Device, Account, Id, ProductId, ProductType, Quantity, Money) ->
    Bin = imdb_proto:encode_proto_5(Channel, OrderId, Game, Device, Account, Id, ProductId, ProductType, Quantity, Money),
    case imdb_client:proto_5(Bin) of
        true -> true;
        _ -> false
    end.


%% 返回true表示记录存在
iap_check(Channel, OrderId) ->
    Bin = imdb_proto:encode_proto_6(Channel, OrderId),
    Ret = imdb_client:proto_6(Bin),
    case size(Ret) > 25 of
        true -> true;
        _ -> false
    end.



%% 返回true则封禁IP
-spec check_ip(Ip :: string() | binary()) ->
    boolean().
check_ip(Ip) ->
    Bin = imdb_proto:encode_proto_1(Ip),
    case imdb_client:proto_1(Bin) of
        true -> true;
        _ -> false
    end.

-spec limei_add(
        App :: string() | binary(), 
        Uid :: string() | binary(), 
        Source :: string() | binary()) ->
    boolean().
limei_add(App, Uid, Source) ->
    Bin = imdb_proto:encode_proto_ln(App, Uid, Source),
    case imdb_client:proto_ln(Bin) of
        true -> true;
        _ -> false
    end.

%% {<<"00">>, <<"00000e00000170000010">>}   - 有记录, 还没有激活(可以激活) - 返回true
%% {<<"00">>, <<"00000e00000170000011">>}   - 有记录, 已经激活             - 返回false
%% {<<"01">>, <<>>}                         - 没有激活                     - 返回false
-spec limei_get(
        App :: string() | binary(), 
        Uid :: string() | binary(), 
        Source :: string() | binary()) ->
    boolean().
limei_get(App, Uid, Source) ->
    Bin = imdb_proto:encode_proto_lo(App, Uid, Source),
    case imdb_client:proto_lo(Bin) of
        {<<"00">>, <<"00000e00000170000010">>} -> true;
        _ -> false
    end.

-spec limei_reg(
        App :: string() | binary(), 
        Uid :: string() | binary(), 
        Source :: string() | binary()) ->
    boolean().
limei_reg(App, Uid, Source) ->
    Bin = imdb_proto:encode_proto_lp(App, Uid, Source),
    case imdb_client:proto_lp(Bin) of
        true -> true;
        _ -> false
    end.

-spec score_read(Username :: binary() | string()) ->
    binary() | undefined.
score_read(Username) ->
    Bin = imdb_proto:encode_score_r(Username),
    imdb_client:score_r(Bin).

-spec score_add(
        Username :: binary() | string(),
        Score :: binary() | string()) ->
    binary() | undefined.
score_add(Username, Score) ->
    Bin = imdb_proto:encode_score_a(Username, Score),
    imdb_client:score_a(Bin).

-spec score_add_ext(
        Username :: binary() | string(),
        Score :: binary() | string(),
        Ext :: binary() | string()) ->
    binary() | undefined.
score_add_ext(Username, Score, Ext) ->
    Bin = imdb_proto:encode_score_a_ext(Username, Score, Ext),
    imdb_client:score_a_ext(Bin).


-spec score_sub(
        Username :: binary() | string(),
        Score :: binary() | string()) ->
    binary() | undefined.
score_sub(Username, Score) ->
    Bin = imdb_proto:encode_score_d(Username, Score),
    imdb_client:score_d(Bin).

-spec score_sub_ext(
        Username :: binary() | string(),
        Score :: binary() | string(),
        Ext :: binary() | string()) ->
    binary() | undefined.
score_sub_ext(Username, Score, Ext) ->
    Bin = imdb_proto:encode_score_d_ext(Username, Score, Ext),
    imdb_client:score_d_ext(Bin).

-spec log_login(
        Username :: string() | binary(), 
        Ip :: string() | binary(), 
        Plat :: string() | binary()) ->
    boolean().
log_login(Username, Ip, Plat) ->
    Bin = imdb_proto:encode_proto_pr(Username, Ip, Plat),
    case imdb_client:proto_pr(Bin) of
        true -> true;
        _ -> false
    end.


-spec log_score(
        Username :: string() | binary(), 
        Score :: integer()) ->
    boolean().
log_score(Username, Score) ->
    IsWin = case Score > 0 of
                true -> "1";
                _ -> "0"
            end,
    Bin = imdb_proto:encode_proto_t(Username, IsWin, integer_to_list(Score)),
    case imdb_client:proto_t(Bin) of
        true -> true;
        _ -> false
    end.

-spec get_id_by_email(Email :: string() | binary()) ->
    binary() | undefined.
get_id_by_email(Email) ->
    Bin = imdb_proto:encode_proto_c(Email),
    case imdb_client:proto_c(Bin) of
        [[Id]] -> Id;
        _ -> undefined
    end.


-spec get_id_by_device(Device :: string() | binary()) ->
    binary() | undefined.
get_id_by_device(Device) ->
    Bin = imdb_proto:encode_proto_b(Device),
    case imdb_client:proto_b(Bin) of
        [[Id]] -> Id;
        _ -> undefined
    end.

-spec get_data_by_id(Id :: string() | binary()) ->
    {Device :: binary(), Email :: binary(), Password :: binary(), RegFrom :: binary(),
     RegIp :: binary(), RegTime :: binary()} | undefined.
get_data_by_id(Id) ->
    Bin = imdb_proto:encode_proto_e(Id),
    case imdb_client:proto_e(Bin) of
        [[Device, Email, Password, RegFrom, RegIp, RegTime]] -> {Device, Email, Password, RegFrom, RegIp, RegTime};
        _ -> undefined
    end.

%% 获取排行榜列表
%%
%% 返回的字段:
%% Id, Value
-spec get_best_list(Type :: string() | binary()) ->
    [[binary()]].
get_best_list(Type) ->
    Bin = imdb_proto:encode_proto_q(Type),
    case imdb_client:proto_q(Bin) of
        undefined -> [];
        Result -> Result
    end.


%% 获取赠送筹码信息
%%
%% 返回字段：
%% 

-spec get_give_score(Userid :: string() | binary()) ->
    [[binary()]].
get_give_score(Userid) ->
    Bin = imdb_proto:encode_proto_yy(Userid),
    case imdb_client:proto_yy(Bin) of
        undefined -> [];
        Result -> Result
    end.


%% 插入筹码赠送记录
-spec insert_give_score(Donor :: integer() | binary(), Receiver :: integer() | binary(), Number :: integer() | binary(), Time :: string() | binary()) ->
    [[binary()]].
insert_give_score(Donor, Receiver, Number, Time) ->
    Bin = imdb_proto:encode_proto_y(Donor, Receiver, Number, Time),
    case imdb_client:proto_y(Bin) of
        true -> true;
        _ -> false
    end.


%%获取奥马哈礼物赠送记录
-spec get_amh_giving(Userid :: string() | binary()) ->
    [[binary()]].
get_amh_giving(Userid) ->
    Bin = imdb_proto:encode_amh_ab(Userid),
    case imdb_client:proto_ab(Bin) of
        undefined -> [];
        Result -> Result
    end.


%% 插入奥马哈礼物赠送记录
-spec insert_amh_giving(Donor :: integer() | binary(), Receiver :: integer() | binary(), Goods :: integer() | binary(), Time :: string() | binary()) ->
    [[binary()]].
insert_amh_giving(Donor, Receiver, Goods, Time) ->
    Bin = imdb_proto:encode_amh_aa(Donor, Receiver, Goods, Time),
    case imdb_client:proto_aa(Bin) of
        true -> true;
        _ -> false
    end.

-spec get_amh_toplist(Type :: integer() | binary()) ->
    [[binary()]].
get_amh_toplist(Type) ->
    Bin = imdb_proto:encode_amh_ad(Type),
    case imdb_client:proto_ad(Bin) of
        undefined -> [];
        Result -> Result
    end.
   
-spec get_feedback_request(UserId :: string() | binary()) ->
    [[binary()]].
get_feedback_request(Userid) ->
    Bin = imdb_proto:encode_amh_af(Userid),
    case imdb_client:proto_af(Bin) of
        undefined -> [];
        Result -> Result
    end.

-spec insert_feedback_request(UserId :: integer() | binary(), Nick ::string() | binary(), Msg :: string() |       binary()) ->
    [[binary()]].
insert_feedback_request(UserId, Nick, Msg) ->
    Bin = imdb_proto:encode_amh_ag(UserId, Nick, Msg),
    case imdb_client:proto_ag(Bin) of
        true -> true;
        _ -> false
    end.

-spec get_feedback_answer(UserId :: string() | binary()) ->
        [[binary()]].
get_feedback_answer(Userid) ->
        Bin = imdb_proto:encode_amh_ai(Userid),
        case imdb_client:proto_ai(Bin) of
            undefined -> [];
            Result -> Result
        end.

%% 删除奥马哈礼物赠送记录
-spec delete_amh_giving(ID :: integer() | binary(), Receiver :: integer() | binary()) ->
    [[binary()]].
delete_amh_giving(ID, Receiver) ->
    Bin = imdb_proto:encode_amh_ac(ID, Receiver),
    case imdb_client:proto_ac(Bin) of
        true -> true;
        _ -> false
    end.

%%把用户ID放入到显示位
update_display_pos(GameName, DisplayPos) when is_binary(GameName) andalso is_binary(DisplayPos) ->
	Bin = imdb_proto:encode_amh_al(GameName, DisplayPos),
	case imdb_client:proto_al(Bin) of
		true -> true;
		_ -> false
	end.

get_display_pos_list(GameName) when is_binary(GameName) ->
	Bin = imdb_proto:encode_amh_am(GameName),
	case imdb_client:proto_am(Bin) of
		undefined -> [];
		Result -> Result
	end.


%% 当前支持的字段: 
%% score, success, fail, offline, user_faceurl, nickname, language, status, user_charge, user_chips, user_email, user_money, user_device_id, 
%% user_property, user_gift, user_send_chips, user_login_type, user_location, user_best, user_online, fish_total_gain
%% fish_ticket： 奖券
%% fish_sign_days：连续签到天数 
%% fish_last_sign：最后签到时间戳
%% fish_last_login: 最后一次登录时间戳
-spec read(
        Id :: string() | binary(),
        Fields :: [string() | binary()]) ->
    [{Key :: binary(), Val :: binary()}].
read(Id, Fields) ->
    Bin = imdb_proto:encode_proto_r(Id, Fields),
    case imdb_client:proto_r(Bin) of
        undefined -> [];
        Result -> Result
    end.

-spec write(
        Id :: string() | binary(),
        FieldKVs :: [{Key :: string() | binary(), Val :: string() | binary()}]) ->
    boolean().
write(Id, FieldKVs) ->
    Bin = imdb_proto:encode_proto_w(Id, FieldKVs),
    case imdb_client:proto_w(Bin) of
        true -> true;
        _ -> false
    end.


-spec update(
        Id :: string() | binary(),
        FieldKVs :: [{Key :: string() | binary(), Val :: string() | binary()}]) ->
    boolean().
update(Id, FieldKVs) ->
    Bin = imdb_proto:encode_proto_update(Id, FieldKVs),
    case imdb_client:proto_update(Bin) of
        true -> true;
        _ -> false
    end.

-spec update_password(
        UserID :: string() | binary(),
        NewPasswd :: string() | binary()) ->
    boolean().
update_password(UserID, NewPasswd) ->
    Bin = imdb_proto:encode_proto_hp(UserID, NewPasswd),
    case imdb_client:proto_hp(Bin) of
        true -> true;
        _ -> false
    end.

-spec get_gift_info(Id :: string() | binary()) ->
    undefined | {RetId :: binary(), RetType :: binary(), RetDesc :: binary(), RetPrice :: binary(), RetPicUrl :: binary()}.
get_gift_info(Id) ->
    Bin = imdb_proto:encode_proto_po(Id),
    case imdb_client:proto_po(Bin) of
        [[RetId, TypeId, Desc, Price, PicUrl]] ->
            {RetId, TypeId, Desc, Price, PicUrl};
        _ ->
            undefined
    end.

%% 插入一条离线消息(Friend发送给Username的离线消息)
-spec add_offline(
        Username :: string() | binary(),
        Friend :: string() | binary(),
        Type :: string() | binary(),
        Content :: string() | binary()) ->
    boolean().
add_offline(Username, Friend, Type, Content) ->
    Bin = imdb_proto:encode_proto_o(Username, Friend, Type, Content),
    case imdb_client:proto_o(Bin) of
        undefined -> false;
        Result -> Result
    end.

%% 读取离线消息(读取后会删除)
%% [[Friend :: binary(), Type :: binary(), Content :: binary(), Timestamp :: binary()]]
-spec get_offline(Username :: string() | binary()) -> 
    [[binary()]].
get_offline(Username) ->
    Bin = imdb_proto:encode_proto_p(Username),
    case imdb_client:proto_p(Bin) of
        undefined -> [];
        Result -> Result
    end.



-spec add_friend(
        Username :: string() | binary(),
        Friend :: string() | binary(),
        Note :: string() | binary(),
        Group :: string() | binary()) ->
    boolean().
add_friend(Username, Friend, Note, Group) ->
    add_friend(Username, Friend, "", Note, Group, "").


%% 添加好友
%% 1. 正向关系插入一条记录
%% 2. 反向关系插入一条记录
-spec add_friend(
        Username :: string() | binary(),
        Friend :: string() | binary(),
        Flag :: string() | binary(),
        Note :: string() | binary(),
        Group :: string() | binary(),
        RFlag :: string() | binary()) ->
    boolean().
add_friend(Username, Friend, Flag, Note, Group, RFlag) ->
    BinA = imdb_proto:encode_proto_a(Username, Friend, Flag, Note, Group),
    BinB = imdb_proto:encode_proto_fb(Friend, Username, RFlag),
    case {imdb_client:proto_a(BinA), imdb_client:proto_fb(BinB)} of
        {true, true} -> true;
        _ -> false
    end.
    

%% 删除好友
%% 1. 删除正向关系
%% 2. 删除反向关系
-spec del_friend(
        Username :: string() | binary(),
        Friend :: string() | binary()) ->
    boolean().
del_friend(Username, Friend) ->
    BinE = imdb_proto:encode_proto_fe(Username, Friend),
    BinF = imdb_proto:encode_proto_f(Friend, Username),
    case {imdb_client:proto_fe(BinE), imdb_client:proto_f(BinF)} of
        {true, true} -> true;
        _ -> false
    end.

%% 获取用户的正向好友
%%
%% 返回的字段:
%% Friend, Flag, Note, Group
-spec get_friend(Username :: string() | binary()) ->
    [[binary()]].
get_friend(Username) ->
    Bin = imdb_proto:encode_proto_g(Username),
    case imdb_client:proto_g(Bin) of
        undefined -> [];
        Result -> Result
    end.

%% 查看两个人的正向好友关系
-spec get_friend(
        Username :: string() | binary(), 
        Friend :: string() | binary()) ->
    {Flag :: binary(), Note :: binary(), Group :: binary()} | undefined.
get_friend(Username, Friend) ->
    Bin = imdb_proto:encode_proto_h(Username, Friend),
    case imdb_client:proto_h(Bin) of
        [[Flag, Note, Group]] -> {Flag, Note, Group};
        _ -> undefined
    end.

%% 获取用户的反向好友
%%
%% 返回的字段:
%% Friend, Flag
-spec get_rfriend(Username :: string() | binary()) ->
    [[binary()]].
get_rfriend(Username) ->
    Bin = imdb_proto:encode_proto_j(Username),
    case imdb_client:proto_j(Bin) of
        undefined -> [];
        Result -> Result
    end.

%% 更新正向好友的flag标志
-spec update_friend_flag(
        Username :: string() | binary(),
        Friend :: string() | binary(),
        Flag :: string() | binary()) ->
    boolean().
update_friend_flag(Username, Friend, Flag) ->
    Bin = imdb_proto:encode_proto_fc(Username, Friend, "flag", Flag),
    case imdb_client:proto_fc(Bin) of
        undefined -> false;
        Result -> Result
    end.


%% 更新反向好友的flag标志
%% 
%% 注意:
%% 这里的Username & Friend的语义和update_friend_flag/1一致, 也就是Friend是Username
%% 的反向好友.
-spec update_rfriend_flag(
        Username :: string() | binary(),
        Friend :: string() | binary(),
        Flag :: string() | binary()) ->
    boolean().
update_rfriend_flag(Username, Friend, Flag) ->
    Bin = imdb_proto:encode_proto_d(Username, Friend, "flag", Flag),
    case imdb_client:proto_d(Bin) of
        undefined -> false;
        Result -> Result
    end.


%% 更新正向好友的note
-spec update_friend_note(
        Username :: string() | binary(),
        Friend :: string() | binary(),
        Note :: string() | binary()) ->
    boolean().
update_friend_note(Username, Friend, Note) ->
    Bin = imdb_proto:encode_proto_fc(Username, Friend, "note", Note),
    case imdb_client:proto_fc(Bin) of
        undefined -> false;
        Result -> Result
    end.

%% 更新正向好友的group
-spec update_friend_group(
        Username :: string() | binary(),
        Friend :: string() | binary(),
        Group :: string() | binary()) ->
    boolean().
update_friend_group(Username, Friend, Group) ->
    Bin = imdb_proto:encode_proto_fc(Username, Friend, "f_group", Group),
    case imdb_client:proto_fc(Bin) of
        undefined -> false;
        Result -> Result
    end.


%% note系统的设计
%% 1. A添加B为好友
%%    B + A + 'add_friend' + xml stanza
%%    这条消息不管B是否在线, 都需要写离线消息, 并且在B答复A之后才会删除.
%%
%% 2. B同意A的添加请求
%%    A + B + 'agree_friend' + xml stanza
%%    a. 检测是否有B + A + 'add_friend'的msg信息, 如果有, 则删除.
%%    b. 如果A在线, 则直接将这条答复发送给A
%%       如果A离线, 写入一条离线消息, 等待A上线之后推送给A并删除.
%%
%% 3. B拒绝A的添加请求
%%    A + B + 'reject_friend' + xml stanza
%%    a. 检测是否有B + A + 'add_friend'的msg信息, 如果有, 则删除.
%%    b. 如果A在线, 则直接将这条答复发送给A
%%       如果A离线, 写入一条离线消息, 等待A上线之后推送给A并删除.
%%


%% Friend想添加Username为好友的pending requst
%%
%% 注意:
%% 内部会先调用get_add_friend_note/1检测是否有老的pending request,
%% 如果存在老的请求, 则删除老的请求, 然后添加新的.
-spec add_friend_note(Username :: string() | binary(),
                      Friend :: string() | binary(),
                      Content :: string() | binary()) ->
    boolean().
add_friend_note(Username, Friend, Content) ->
    %% 删除老的pending请求
    case get_add_friend_note(Username, Friend) of
        [{RetId, _RetFriend, _RetContent}] ->
            BinDel = imdb_proto:encode_proto_lc(Username, RetId),
            imdb_client:proto_lc(BinDel);
        [] ->
            ok
    end,

    %% 插入新的pending请求
    Bin = imdb_proto:encode_proto_la(Username, Friend, "add_friend", Content),
    case imdb_client:proto_la(Bin) of
        undefined -> false;
        Result -> Result
    end.


%% 获取Friend想添加Username为好友的pending request
-spec get_add_friend_note(Username :: string() | binary(),
                          Friend :: string() | binary()) ->
    [{Id :: binary(), Friend :: binary(), Content :: binary()}] | [].
get_add_friend_note(Username, Friend) when is_list(Friend) ->
    get_add_friend_note(Username, list_to_binary(Friend));
get_add_friend_note(Username, Friend) ->
    Bin = imdb_proto:encode_proto_lb(Username, "0", "1000"),
    ResList = 
      case imdb_client:proto_lb(Bin) of
          undefined -> [];
          Result -> Result
      end,
    lists:foldl(
      fun([Id, RetFriend, <<"add_friend">>, Content, _Timestamp], []) -> 
             case RetFriend == Friend of
                 true ->
                     [{Id, RetFriend, Content}];
                 false ->
                     []
             end;
         ([Id, RetFriend, <<"add_friend">>, _Content, _Timestamp], Acc) -> 
             case RetFriend == Friend of
                 true ->
                     %% 删除重复的add_friend请求, 只保留一个
                     %% 在正常的逻辑下不会有这个分支
                     BinDel = imdb_proto:encode_proto_lc(Username, Id),
                     imdb_client:proto_lc(BinDel);
                 false ->
                     ok
             end,
             Acc;
         (_, Acc) -> 
             Acc
      end, [], ResList).


%% Friend同意Username的添加请求
-spec agree_friend_note(Username :: string() | binary(),
                        Friend :: string() | binary(),
                        Content :: string() | binary()) ->
    boolean().
agree_friend_note(Username, Friend, Content) ->
    Bin = imdb_proto:encode_proto_la(Username, Friend, "agree_friend", Content),
    case imdb_client:proto_la(Bin) of
        undefined -> false;
        Result -> Result
    end.


%% Friend拒绝Username的添加请求
-spec reject_friend_note(Username :: string() | binary(),
                         Friend :: string() | binary(),
                         Content :: string() | binary()) ->
    boolean().
reject_friend_note(Username, Friend, Content) ->
    Bin = imdb_proto:encode_proto_la(Username, Friend, "reject_friend", Content),
    case imdb_client:proto_la(Bin) of
        undefined -> false;
        Result -> Result
    end.


%% 用户登录的时候调用这个APIs获取所有pending的notes
-spec get_pending_notes(Username :: string()) -> 
    [{Id :: binary(), Friend :: binary(), Content :: binary()}].
get_pending_notes(Username) ->
    Bin = imdb_proto:encode_proto_lb(Username, "0", "1000"),
    ResList = 
      case imdb_client:proto_lb(Bin) of
          undefined -> [];
          Result -> Result
      end,
    lists:foldl(
      fun([Id, Friend, <<"add_friend">>, Content, _Timestamp], Acc) -> 
             %% add_friend只有在答复的时候才能删除
             [{Id, Friend, <<"add_friend">>, Content} | Acc];
         ([Id, Friend, <<"agree_friend">>, Content, _Timestamp], Acc) -> 
             %% 删除已经返回的agree_friend的note
             BinDel = imdb_proto:encode_proto_lc(Username, Id),
             imdb_client:proto_lc(BinDel),
             [{Id, Friend, <<"agree_friend">>, Content} | Acc];
         ([Id, Friend, <<"reject_friend">>, Content, _Timestamp], Acc) -> 
             %% 删除已经返回的reject_friend的note
             BinDel = imdb_proto:encode_proto_lc(Username, Id),
             imdb_client:proto_lc(BinDel),
             [{Id, Friend, <<"reject_friend">>, Content} | Acc];
         (_, Acc) -> 
             Acc
      end, [], ResList).


%% 删除一个note
-spec del_note(Username :: string() | binary(),
               Id :: string() | binary()) ->
    boolean().
del_note(Username, Id) ->
    Bin = imdb_proto:encode_proto_lc(Username, Id),
    case imdb_client:proto_lc(Bin) of
        undefined -> false;
        Result -> Result
    end.

