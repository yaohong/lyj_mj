%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 二月 2014 下午3:30
%%%-------------------------------------------------------------------
-module(qp_config).
-author("yaohong").

-behaviour(gen_server).
-export([read_cfg_url/0]).
%% API
-export([start_link/0, start_link/1, start_link/2]).
-include("../deps/file_log/include/file_log.hrl").
%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).
-export([get_cfg/1]).
-export([write_cfg/2]).
-export([reload_cfg/0]).


-define(SERVER, ?MODULE).


-record(state, {pre_mod, load_type}).
-record(cfg, {key, value}).

reload_cfg() ->
	gen_server:call(?MODULE, reload_cfg).
%%%===================================================================
%%% API
%%%===================================================================

%%{update_cache_node, CacheNode}
write_cfg(Key, Value) ->
	gen_server:call(?MODULE, {write_cfg, {Key, Value}}).

get_cfg(Key) ->
	case ets:lookup(cfg, Key) of
		[#cfg{value = Value}] -> {success, Value};
		_ -> fail
	end.



%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
	L1 =
		case os:getenv("PRIVATE_CFG_PATH") of
			PrivateCfgPath when is_list(PrivateCfgPath) andalso length(PrivateCfgPath) > 0 -> [PrivateCfgPath];
			_ -> []
		end,

	L2 =
		case os:getenv("PUBLIC_CFG_PATH") of
			PublicPath when is_list(PublicPath) andalso length(PublicPath) > 0 -> [PublicPath|L1];
			_ -> L1
		end,

	L3 =
		case os:getenv("DEBUG_CFG_PATH") of
			DebugPath when is_list(DebugPath) andalso length(DebugPath) > 0 -> [DebugPath|L2];
			_ -> L2
		end,
	start_link(undefined, {local_cfg, L3}).

start_link(PreMod) when is_atom(PreMod) ->
	L1 =
		case os:getenv("PRIVATE_CFG_PATH") of
			PrivateCfgPath when is_list(PrivateCfgPath) andalso length(PrivateCfgPath) > 0 -> [PrivateCfgPath];
			_ -> []
		end,

	L2 =
		case os:getenv("PUBLIC_CFG_PATH") of
			PublicPath when is_list(PublicPath) andalso length(PublicPath) > 0 -> [PublicPath|L1];
			_ -> L1
		end,

	L3 =
		case os:getenv("DEBUG_CFG_PATH") of
			DebugPath when is_list(DebugPath) andalso length(DebugPath) > 0 -> [DebugPath|L2];
			_ -> L2
		end,
	gen_server:start_link({local, ?SERVER}, ?MODULE, [PreMod, {local_cfg, L3}], []).

start_link(PreMod, LoadType) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [PreMod, LoadType], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
	{ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([PreMod, LoadCfg]) ->
	ets:new(cfg, [set, protected, named_table, {keypos, #cfg.key}]),

	?FILE_LOG_DEBUG("load_cfg=~p", [LoadCfg]),
	%%先加载公共配置
	RawParamList = load_all_cfg(LoadCfg),
	?FILE_LOG_DEBUG("raw_param_list=~p", [RawParamList]),
	insert_param_to_ets(RawParamList, PreMod),
	{ok, #state{pre_mod = PreMod, load_type = LoadCfg}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
	State :: #state{}) ->
	{reply, Reply :: term(), NewState :: #state{}} |
	{reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_call(reload_cfg, _From, #state{pre_mod = PreMod, load_type = LoadType}=State) ->
	try
		RawParamList = load_all_cfg(LoadType),
		insert_param_to_ets(RawParamList, PreMod)
	catch
		What:Type ->
			?FILE_LOG_WARNING("reload_cfg fail what=~p, type=~p stack=~p", [What, Type, erlang:get_stacktrace()])
	end,
	{reply, success, State};
handle_call({write_cfg, {Key, Value}}, _From, State) ->
	ets:insert(cfg, #cfg{key = Key,value = Value}),
	{reply, success, State};
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
	State :: #state{}) -> term()).
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
	Extra :: term()) ->
	{ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
load_all_cfg({local_cfg, CfgList}) ->
	CfgParamTree =
		lists:foldl(
			fun(Cfg, OutCfgParamTree) ->
				load_param_by_cfg(Cfg, OutCfgParamTree)
			end, gb_trees:empty(), CfgList),
	gb_trees:to_list(CfgParamTree);
load_all_cfg({url, {UrlAddr, Uuid, AppName}}) ->
	Value = ["uuid=",qp_util:to_list(Uuid), "&app_name=", qp_util:to_list(AppName)],
	case httpc:request(
		'post',
		{
			UrlAddr,
			[],
			"application/x-www-form-urlencoded",
			iolist_to_binary(lists:flatten(Value))
		},
		[{connect_timeout, 3000}], []) of
		{ok, {{_, 200, _}, _, TmpJsonData}} ->
			?FILE_LOG_DEBUG("json_data=~p", [TmpJsonData]),
			{struct, JsonValueList} = mochijson2:decode(TmpJsonData),
			%%先读取public
			{struct, PublicParamList} = proplists:get_value(<<"public">>, JsonValueList),
			Param1 = extract_public_param(PublicParamList),
			{struct, DebugParamList} = proplists:get_value(<<"debug">>, JsonValueList),
			Param2 = extract_debug_param(DebugParamList),
			Param3 =
				case proplists:get_value(AppName, JsonValueList) of
					undefined -> [];
					{struct,AppParamList} ->
						?FILE_LOG_DEBUG("app_name=~p, app_param_list=~p", [AppName, AppParamList]),
						extract_app_param(AppName, AppParamList)
				end,

			TotalParam = Param1 ++ Param2 ++ Param3,
			?FILE_LOG_DEBUG("all param=~p", [TotalParam]),
			TotalParam;
		Other ->
			?FILE_LOG_WARNING("load param reason=~p", [Other]),
			throw("load param fail")
	end.

load_param_by_cfg(CfgPath, CfgParamTree) ->
	case file:consult(CfgPath) of
		{ok, ConfigDataList} ->
			lists:foldr(
				fun({Key, Value}, OutCfgParamTree) ->
					case gb_trees:lookup(Key, OutCfgParamTree) of
						none -> gb_trees:insert(Key, Value, OutCfgParamTree);
						{value, OldValue} ->
							?FILE_LOG_DEBUG("param key[~p] value[~p] => [~p]", [Key, OldValue, Value]),
							gb_trees:update(Key, Value, OutCfgParamTree)
					end
				end, CfgParamTree, ConfigDataList);
		{error, Reason} ->
			?FILE_LOG_WARNING("cfg_path[~p] reason=~p", [CfgPath, Reason]),
			CfgParamTree
	end.


insert_param_to_ets(RawParamList, PreMod) ->
	PreAfterParamList = parm_pre_handle(RawParamList, PreMod),
	lists:foreach(
		fun({Key, Value}) ->
			ets:insert(cfg, #cfg{key = Key, value= Value})
		end, PreAfterParamList).



parm_pre_handle(ParamList, undefined) -> ParamList;
parm_pre_handle(ParamList, PreMod) ->
	lists:map(
		fun({Key, Value}) ->
			{Key, PreMod:pre_handle(Key, Value)}
		end,  ParamList).


extract_public_param(PublicJsonList) ->
	lists:foldr(
		fun({<<"server_info">>, JsonValue}, TmpParam1) ->
			{struct, [{<<"server_id">>,  ServerId}, {<<"server_srcid">>, SrcId}, {<<"server_name">>, ServerName}]} = JsonValue,
			[{server_info, {qp_util:to_integer(ServerId), qp_util:to_integer(SrcId), qp_util:to_list(ServerName)}}|TmpParam1];
			({<<"force_verify">>, JsonValue}, TmpParam1) ->
				[{force_verify, qp_util:to_integer(JsonValue)}|TmpParam1];
			({<<"filter_proto">>, JsonValue}, TmpParam1) ->
				FilterProtoList = string:tokens(qp_util:to_list(JsonValue), "#"),
				FilterProSet =
					lists:foldr(
						fun(FilterProtoCmd, TmpSet) ->
							gb_sets:add_element(qp_util:to_integer(FilterProtoCmd), TmpSet)
						end, gb_sets:new(), FilterProtoList),
				[{filter_proto, FilterProSet}|TmpParam1];
			({<<"server_version">>, JsonValue}, TmpParam1) ->
				[{server_version, qp_util:to_list(JsonValue)}|TmpParam1];
			({<<"game_id">>, JsonValue}, TmpParam1) ->
				[{game_id, qp_util:to_list(JsonValue)}|TmpParam1];
			({<<"cdk_srv">>, JsonValue}, TmpParam1) ->
				{struct, [{<<"cdk_host">>,  CdkHost}, {<<"cdk_port">>, CdkPort}]} = JsonValue,
				[{cdk_srv, {qp_util:to_list(CdkHost), qp_util:to_integer(CdkPort)}}|TmpParam1];

			({<<"invite_srv">>, JsonValue}, TmpParam1) ->
				{struct, [{<<"invite_host">>,  InviteHost}, {<<"invite_port">>, InvitePort}]} = JsonValue,
				[{invite_srv, {qp_util:to_list(InviteHost), qp_util:to_integer(InvitePort)}}|TmpParam1];

			({<<"token_addr">>, JsonValue}, TmpParam1) ->
				{struct, [{<<"token_host">>,  TokenHost}, {<<"token_port">>, TokenPort}]} = JsonValue,
				[{token_addr, {qp_util:to_list(TokenHost), qp_util:to_integer(TokenPort)}}|TmpParam1];
			({<<"gm_internal_url">>,JsonValue}, TmpParam1) ->
				[{gm_internal_url, qp_util:to_list(JsonValue)}|TmpParam1];
			({<<"dir_svr_addr">>, JsonValue}, TmpParam1) ->
				{struct, [{<<"dir_svr_host">>,  DirSvrHost}, {<<"dir_svr_port">>, DirSvrPort}]} = JsonValue,
				[{dir_svr_addr, {qp_util:to_list(DirSvrHost), qp_util:to_integer(DirSvrPort)}}|TmpParam1];
			({<<"dbmgr_node">>, JsonValue}, TmpParam1) ->
				[{dbmgr_node, qp_util:to_atom(JsonValue)}|TmpParam1];
			({<<"gateway_node">>, JsonValue}, TmpParam1) ->
				[{gateway_node, qp_util:to_atom(JsonValue)}|TmpParam1];
			({<<"verify_node">>, JsonValue}, TmpParam1) ->
				[{verify_node, qp_util:to_atom(JsonValue)}|TmpParam1];
			({<<"gmcontrol_node">>, JsonValue}, TmpParam1) ->
				[{gmcontrol_node, qp_util:to_atom(JsonValue)}|TmpParam1];
			({<<"pvp_node">>, JsonValue}, TmpParam1) ->
				[{pvp_node, qp_util:to_atom(JsonValue)}|TmpParam1];
			({<<"rcservice_node">>, JsonValue}, TmpParam1) ->
				[{rcservice_node, qp_util:to_atom(JsonValue)}|TmpParam1];
			({<<"oss_node">>, JsonValue}, TmpParam1) ->
				[{oss_node, qp_util:to_atom(JsonValue)}|TmpParam1];
			({<<"cache_node">>, JsonValue}, TmpParam1) ->
				[{cache_node, qp_util:to_atom(JsonValue)}|TmpParam1];
			({<<"im_node">>, JsonValue}, TmpParam1) ->
				[{im_node, qp_util:to_atom(JsonValue)}|TmpParam1];
			({<<"push_node">>, JsonValue}, TmpParam1) ->
				[{push_node, qp_util:to_atom(JsonValue)}|TmpParam1];
			({<<"filter_node">>, JsonValue}, TmpParam1) ->
				[{filter_node, qp_util:to_atom(JsonValue)}|TmpParam1];
			({<<"ap_node">>, JsonValue}, TmpParam1) ->
				[{ap_node, qp_util:to_atom(JsonValue)}|TmpParam1];
			({<<"mail_server">>, JsonValue}, TmpParam1) ->
				[{mail_server, qp_util:to_list(JsonValue)}|TmpParam1];
			({<<"db_addr">>, JsonValue}, TmpParam1) ->
				{struct, [{<<"db_ip">>,  DbIp}, {<<"db_port">>, DbPort}, {<<"db_account">>, DbAccount}, {<<"db_pwd">>, DbPwd}, {<<"db_suffix">>, DbSuffix}]} = JsonValue,
				[{db_addr, {qp_util:to_list(DbIp), qp_util:to_integer(DbPort), qp_util:to_list(DbAccount),qp_util:to_list(DbPwd), qp_util:to_list(DbSuffix)}}|TmpParam1];
			({<<"intranet_ip">>, JsonValue}, TmpParam1) ->
				[{intranet_ip, qp_util:to_list(JsonValue)}|TmpParam1];
			({<<"extranet_ip">>, JsonValue}, TmpParam1) ->
				[{extranet_ip, qp_util:to_list(JsonValue)}|TmpParam1];
			({<<"activity_state_list">>, JsonValue}, TmpParam1) ->
				ActivityItemList = string:tokens(qp_util:to_list(JsonValue), "#"),
				ErlActivityList =
					lists:map(
						fun(ActivityItem) ->
							[ActivityId, IsOpen] = string:tokens(ActivityItem, "-"),
							{qp_util:to_integer(ActivityId), qp_util:to_integer(IsOpen)}
						end, ActivityItemList),
				[{activity_state_list, ErlActivityList}|TmpParam1]
		end, [], PublicJsonList).

extract_debug_param(DebugJsonList) ->
	lists:foldr(
		fun({<<"debug_fast_game">>, JsonValue}, TmpParam1) ->
			[{debug_fast_game, qp_util:to_integer(JsonValue)}|TmpParam1];
			({<<"debug_pvp_rank_update">>, JsonValue}, TmpParam1) ->
				[{debug_pvp_rank_update, qp_util:to_integer(JsonValue)}|TmpParam1];
			({<<"debug_test_recharge">>, JsonValue}, TmpParam1) ->
				[{debug_test_recharge, qp_util:to_integer(JsonValue)}|TmpParam1];
			({<<"debug_test_vr">>, JsonValue}, TmpParam1) ->
				[{debug_test_vr, qp_util:to_integer(JsonValue)}|TmpParam1];
			({<<"debug_gm_control">>,JsonValue}, TmpParam1) ->
				[{debug_gm_control, qp_util:to_integer(JsonValue)}|TmpParam1]
		end, [], DebugJsonList).


extract_app_param(<<"cache">>, AppJsonList) ->
	lists:foldr(
		fun({<<"server_start_time">>, JsonValue}, TmpParam1) ->
			[{server_start_time, qp_util:to_list(JsonValue)}|TmpParam1];
			({<<"family_start_time">>, JsonValue}, TmpParam1) ->
				[{family_start_time, qp_util:to_list(JsonValue)}|TmpParam1];
			({<<"register_limit">>, JsonValue}, TmpParam1) ->
				[{register_limit, qp_util:to_integer(JsonValue)}|TmpParam1]
		end, [], AppJsonList);
extract_app_param(<<"gateway">>, AppJsonList) ->
	lists:foldr(
		fun({<<"listen_port">>, JsonValue}, TmpParam1) ->
			[{listen_port, qp_util:to_integer(JsonValue)}|TmpParam1]
		end, [], AppJsonList);
extract_app_param(<<"gmcontrol">>, AppJsonList) ->
	lists:foldr(
		fun({<<"listen_port">>, JsonValue}, TmpParam1) ->
			[{listen_port, qp_util:to_integer(JsonValue)}|TmpParam1];
			({<<"lt_listen_port">>, JsonValue}, TmpParam1) ->
				[{lt_listen_port, qp_util:to_integer(JsonValue)}|TmpParam1];
			({<<"dis_listen_port">>, JsonValue}, TmpParam1) ->
				[{dis_listen_port, qp_util:to_integer(JsonValue)}|TmpParam1];
			({<<"enter_ip_list">>, JsonValue}, TmpParam1) ->
				IpList = string:tokens(qp_util:to_list(JsonValue), "#"),
				EmterIpList =
					lists:map(
						fun(IpItem) ->
							qp_util:to_list(IpItem)
						end, IpList),
				[{enter_ip_list, EmterIpList}|TmpParam1]
		end, [], AppJsonList);
extract_app_param(<<"im">>, AppJsonList) ->
	lists:foldr(
		fun({<<"max_global_msg_count">>, JsonValue}, TmpParam1) ->
			[{max_global_msg_count, qp_util:to_integer(JsonValue)}|TmpParam1];
			({<<"max_union_msg_count">>, JsonValue}, TmpParam1) ->
				[{max_union_msg_count, qp_util:to_integer(JsonValue)}|TmpParam1]
		end, [], AppJsonList);
extract_app_param(<<"rc">>, AppJsonList) ->
	lists:foldr(
		fun({<<"listen_port">>, JsonValue}, TmpParam1) ->
			[{listen_port, qp_util:to_integer(JsonValue)}|TmpParam1]
		end, [], AppJsonList).


%%读取CFG的路径
read_cfg_url() ->
	case file:read_file("/sl/cfg_addr.cfg") of
		{ok, CfgUrl} ->
			{success, qp_util:to_list(CfgUrl)};
		{error, Reason} ->
			?FILE_LOG_ERROR("read local cfg fail, reason=~p", [Reason]),
			fail
	end.