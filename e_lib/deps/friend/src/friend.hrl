%%好友常规的返回值
-define(FD_SUCCESS, 0).								%%成功
-define(FD_ACCOUNT_NOT_LOGIN, 1).					%%账号没登录
-define(FD_GAME_NOT_LOGIN, 2).						%%游戏没登录
-define(FD_SESSION_NOT_LOGIN, 3).					%%session无效
-define(FD_ARE_FRIEND, 3).							%%已经是好友了
-define(FD_LOAD_NOT_COMPLETE, 4).					%%加载没完成
-define(FD_NOT_FRIEND, 5).							%%不是好友
-define(FD_NOT_GET_FRIEND_LIST, 6).					%%没有获取好友列表
-define(FD_OTHER_ERROR, 7).							%%其他错误

-type mode() :: complete | continute.