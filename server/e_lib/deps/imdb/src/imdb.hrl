%% Proto
-define(IMDB_PROTO_PERFIX, 6).

-define(IMDB_PROTO_PROXY_M_NEWFRIENDS,     %% size = 25
        <<$0,$0,$0,$0,$0,$1,$1,                                    
          $0,$0,$0,$0,$0,$c,$M,$_,$N,$E,$W,$F,$R,$I,$E,$N,$D,$S>>).
-define(IMDB_PROTO_PROXY_M_NEWFRIENDS_LEN, 25).          

-define(IMDB_PROTO_PROXY_M_PUSER,     %% size = 20
        <<$0,$0,$0,$0,$0,$1,$1,                                    
          $0,$0,$0,$0,$0,$7,$M,$_,$P,$U,$S,$E,$R>>).
-define(IMDB_PROTO_PROXY_M_PUSER_LEN, 20).          

-define(IMDB_PROTO_PROXY_M_SCORE,     %% size = 20
        <<$0,$0,$0,$0,$0,$1,$1,                                    
          $0,$0,$0,$0,$0,$7,$M,$_,$S,$C,$O,$R,$E>>).
-define(IMDB_PROTO_PROXY_M_SCORE_LEN, 20).          


-define(IMDB_PROTO_PROXY_M_POKER,     %% size = 20
        <<$0,$0,$0,$0,$0,$1,$1,                                    
          $0,$0,$0,$0,$0,$7,$M,$_,$P,$O,$K,$E,$R>>).
-define(IMDB_PROTO_PROXY_M_POKER_LEN, 20).          


-define(IMDB_PROTO_PROXY_M_FILTER,     %% size = 21 
        <<$0,$0,$0,$0,$0,$1,$1,                                    
          $0,$0,$0,$0,$0,$8,$M,$_,$F,$I,$L,$T,$E,$R>>).
-define(IMDB_PROTO_PROXY_M_FILTER_LEN, 21).          


-define(IMDB_PROTO_PROXY_M_SYS,     %% size = 18
        <<$0,$0,$0,$0,$0,$1,$1,                                    
          $0,$0,$0,$0,$0,$5,$M,$_,$S,$Y,$S>>).
-define(IMDB_PROTO_PROXY_M_SYS_LEN, 18).          

%% 系统业务
%% ID + Type + Coin + Source + Date
%% 
%% E.g.
%% Date = 20130108, 时间必须是这个格式
-define(IMDB_SYS_A_BIN, <<$0,$0,$0,$0,$0,$1,$A>>).  %% 写入一条加分记录
-define(IMDB_AMH_GIFT_AA_BIN, <<$0,$0,$0,$0,$0,$2,$a,$a>>). %%奥马哈写入赠送筹码记录
-define(IMDB_AMH_GIFT_AB_BIN, <<$0,$0,$0,$0,$0,$2,$a,$b>>). %%奥马哈读取赠送筹码记录
-define(IMDB_AMH_GIFT_AC_BIN, <<$0,$0,$0,$0,$0,$2,$a,$c>>). %%奥马哈删除赠送筹码记录
-define(IMDB_AMH_GIFT_AD_BIN, <<$0,$0,$0,$0,$0,$2,$a,$d>>). %%奥马哈取排行榜
-define(IMDB_AMH_GIFT_AF_BIN, <<$0,$0,$0,$0,$0,$2,$a,$f>>). %%奥马哈取问题列表
-define(IMDB_AMH_GIFT_AG_BIN, <<$0,$0,$0,$0,$0,$2,$a,$g>>). %%奥马哈插入问题
-define(IMDB_AMH_GIFT_AI_BIN, <<$0,$0,$0,$0,$0,$2,$a,$i>>). %%奥马哈取回复列表

-define(IMDB_AMH_DISPLAY_POS_AL_BIN, <<$0,$0,$0,$0,$0,$2,$a,$l>>).      %%写入一个显示位
-define(IMDB_AMH_DISPLAY_POS_AM_BIN, <<$0,$0,$0,$0,$0,$2,$a,$m>>).      %%获取显示位的人


%% 过滤词业务
-define(IMDB_FILTER_F_BIN, <<$0,$0,$0,$0,$0,$1,$F>>). %% 过滤词业务

%% 积分业务
-define(IMDB_SCORE_R_BIN, <<$0,$0,$0,$0,$0,$1,$R>>).  %% 扎金花积分 - 读
-define(IMDB_SCORE_A_BIN, <<$0,$0,$0,$0,$0,$1,$A>>).  %% 扎金花积分 - 加
-define(IMDB_SCORE_D_BIN, <<$0,$0,$0,$0,$0,$1,$D>>).  %% 扎金花积分 - 减


%% Poker业务
-define(IMDB_PROTO_5_BIN, <<$0,$0,$0,$0,$0,$1,$5>>).    %% 将支付数据写入到DB
-define(IMDB_PROTO_6_BIN, <<$0,$0,$0,$0,$0,$1,$6>>).    %% 查询支付数据是否在DB
-define(IMDB_PROTO_1_BIN, <<$0,$0,$0,$0,$0,$1,$1>>).    %% 查询一个IP是否在黑名单
-define(IMDB_PROTO_Q_BIN, <<$0,$0,$0,$0,$0,$1,$q>>).    %% 查询排行榜
-define(IMDB_PROTO_R_BIN, <<$0,$0,$0,$0,$0,$1,$R>>). 
-define(IMDB_PROTO_W_BIN, <<$0,$0,$0,$0,$0,$1,$W>>). 
-define(IMDB_PROTO_B_BIN, <<$0,$0,$0,$0,$0,$1,$B>>).    %% 根据机器码取账户 
-define(IMDB_PROTO_C_BIN, <<$0,$0,$0,$0,$0,$1,$C>>).    %% 根据email取账户
-define(IMDB_PROTO_E_BIN, <<$0,$0,$0,$0,$0,$1,$E>>). 
-define(IMDB_PROTO_PO_BIN, <<$0,$0,$0,$0,$0,$1,$O>>).   %% 获取礼物的详细信息

-define(IMDB_PROTO_PR_BIN, <<$0,$0,$0,$0,$0,$1,$R>>).   
-define(IMDB_PROTO_T_BIN, <<$0,$0,$0,$0,$0,$1,$T>>).   

-define(IMDB_PROTO_LN_BIN, <<$0,$0,$0,$0,$0,$1,$n>>).   
-define(IMDB_PROTO_LO_BIN, <<$0,$0,$0,$0,$0,$1,$o>>).   
-define(IMDB_PROTO_LP_BIN, <<$0,$0,$0,$0,$0,$1,$p>>). 
-define(IMDB_PROTO_Y_BIN, <<$0,$0,$0,$0,$0,$1,$y>>).   %% 写入筹码赠送记录
-define(IMDB_PROTO_YY_BIN, <<$0,$0,$0,$0,$0,$1,$Y>>).  %% 获取筹码赠送记录
-define(IMDB_PROTO_HP_BIN, <<$0,$0,$0,$0,$0,$1,$H>>).  %% 修改密码


%% 好友相关业务
-define(IMDB_PROTO_O_BIN, <<$0,$0,$0,$0,$0,$1,$O>>).    %% 插入一条离线消息 
-define(IMDB_PROTO_P_BIN, <<$0,$0,$0,$0,$0,$1,$P>>).    %% 读取离线消息
-define(IMDB_PROTO_A_BIN, <<$0,$0,$0,$0,$0,$1,$A>>).    
-define(IMDB_PROTO_FB_BIN, <<$0,$0,$0,$0,$0,$1,$B>>).
-define(IMDB_PROTO_G_BIN, <<$0,$0,$0,$0,$0,$1,$G>>). 
-define(IMDB_PROTO_J_BIN, <<$0,$0,$0,$0,$0,$1,$J>>). 
-define(IMDB_PROTO_FE_BIN, <<$0,$0,$0,$0,$0,$1,$E>>). 
-define(IMDB_PROTO_F_BIN, <<$0,$0,$0,$0,$0,$1,$F>>). 
-define(IMDB_PROTO_FC_BIN, <<$0,$0,$0,$0,$0,$1,$C>>). 
-define(IMDB_PROTO_D_BIN, <<$0,$0,$0,$0,$0,$1,$D>>). 
-define(IMDB_PROTO_H_BIN, <<$0,$0,$0,$0,$0,$1,$H>>). 
-define(IMDB_PROTO_LA_BIN, <<$0,$0,$0,$0,$0,$1,$a>>). 
-define(IMDB_PROTO_LB_BIN, <<$0,$0,$0,$0,$0,$1,$b>>). 
-define(IMDB_PROTO_LC_BIN, <<$0,$0,$0,$0,$0,$1,$c>>).

-define(IMDB_PROTO_RET_OK,       <<$0,$0,$0,$0,$0,$2,$0,$0>>).
-define(IMDB_PROTO_RET_OK_NULL,  <<$0,$0,$0,$0,$0,$2,$0,$1>>).
-define(IMDB_PROTO_RET_ERROR_09, <<$0,$0,$0,$0,$0,$2,$0,$9>>).
-define(IMDB_PROTO_RET_ERROR_97, <<$0,$0,$0,$0,$0,$2,$9,$7>>).
-define(IMDB_PROTO_RET_ERROR_98, <<$0,$0,$0,$0,$0,$2,$9,$8>>).
-define(IMDB_PROTO_RET_ERROR_99, <<$0,$0,$0,$0,$0,$2,$9,$9>>).

-define(IMDB_PROTO_EMPTY_ITEM,   <<$0,$0,$0,$0,$0,$0>>). 

