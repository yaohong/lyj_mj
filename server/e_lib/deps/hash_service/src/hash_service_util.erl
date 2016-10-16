-module(hash_service_util).
-author("yaohong").
-include("hash_service.hrl").
%% API
-export([ensure_app_started/1]).

-export([test_hash/1]).
-export([update_hash_rule/2, update_hash_rule/3]).
-export([update_hash_list/2]).

-export([hex_to_big/1, hex_to_small/1]).
-export([to_virtual_node/2, to_node/1]).

-export([find_key_store_node/2]).

-export([hash_string/1]).
-spec ensure_app_started(App :: atom()) -> ok.
ensure_app_started(App) ->
	case application:start(App) of
		ok ->
			ok;
		{error, {already_started, App}} ->
			ok
	end.


test_hash(Count) ->
	NodeNameList =
	lists:map(
		fun(Index) ->
			"testnode@192.168.1" ++ integer_to_list(Index)
		end, lists:seq(1, Count)),
	{AffectedNodeList, NewHashRule} =
	lists:foldr(
		fun(NodeName, {TmpAffectedNodeList, TmpHashRule}) ->
			{AffectedNodeListItem, TmpNewHashRule} = update_hash_rule(NodeName, TmpHashRule),
			{[AffectedNodeListItem|TmpAffectedNodeList], TmpNewHashRule}
		end, {[], #hash_rule{rem_value_list =  [], node_tree =  gb_trees:empty()}}, NodeNameList),
	{AffectedNodeList, NewHashRule#hash_rule.rem_value_list, gb_trees:to_list(NewHashRule#hash_rule.node_tree)}.





hex_to_big(C) when C >= 97 andalso C =< 122 ->  C - 32;
hex_to_big(C) -> C.

hex_to_small(C) when C >= 65 andalso C =< 90 ->  C + 32;
hex_to_small(C) -> C.


hash_string(Value) ->
	hash_string(Value, 1).

hash_string(Value, HashType) ->
	string_iter(Value, 16#7FED7FED, 16#EEEEEEEE, HashType).
string_iter([], S1, _, _) -> S1;
string_iter([V|T], S1, S2, HashType) ->
	Ch = hash_service_util:hex_to_big(V),
	Index = (HashType bsl 8) + Ch,
	[#hash_rd{value = HashValue}] = ets:lookup(hash_rd, Index),
	S1_2 = HashValue bxor (S1 + S2),
	S2_2 = Ch + S1_2 + S2 + (S2 bsl 5) + 3,
	string_iter(T, S1_2, S2_2, HashType).


to_virtual_node(NodeName, Index) when is_list(NodeName) andalso is_integer(Index) ->
	NodeName ++ "#" ++ integer_to_list(Index).

to_node(VirtualNodeName) when is_list(VirtualNodeName) ->
    [NodeName, _VirtualIndex] = string:tokens(VirtualNodeName, "#"),
	list_to_atom(NodeName).


%%添加一个存储节点，更新哈希规则
-spec update_hash_rule(Node :: list(), HashRule :: #hash_rule{}) -> {NextNodeSet :: gb_set(), NewHashRule :: #hash_rule{}}.
update_hash_rule(Node, HashRule) when is_atom(Node) andalso is_record(HashRule, hash_rule) ->
	update_hash_rule(atom_to_list(Node), HashRule);
update_hash_rule(Node, HashRule) when is_list(Node) ->
	{NextHashValueSet, NewHashRule} =
		lists:foldr(
			fun(VirtualIndex, {TmpNextHashValueSet, TmpHashRule}) ->
				VirtualNodeName = hash_service_util:to_virtual_node(Node, VirtualIndex),
				HashValue = hash_service_util:hash_string(VirtualNodeName),
				RemValue = HashValue rem ?MAX_INDEX,
				case gb_trees:lookup(RemValue, TmpHashRule#hash_rule.node_tree) of
					none ->
						%%不存在
						NewTmpNodeTree = gb_trees:insert(RemValue, VirtualNodeName, TmpHashRule#hash_rule.node_tree),
						case hash_service_util:update_hash_list(RemValue, TmpHashRule#hash_rule.rem_value_list) of
							{NewTmpRemValueList, -1} ->
								{TmpNextHashValueSet, TmpHashRule#hash_rule{node_tree = NewTmpNodeTree, rem_value_list = NewTmpRemValueList}};
							{NewTmpRemValueList, NextRemValue} ->
								{gb_sets:add_element(NextRemValue, TmpNextHashValueSet), TmpHashRule#hash_rule{node_tree = NewTmpNodeTree, rem_value_list = NewTmpRemValueList}}
						end;
					{value, _} ->
						{TmpNextHashValueSet, TmpHashRule}
				end
			end, {gb_sets:new(), HashRule}, lists:seq(1, ?VIRTUAL_NODE_COUNT)),
	NextNodeSet =
		lists:foldr(
			fun(NextHashValueItem, TmpNextNodeSet) ->
				VirtualNodeName = gb_trees:get(NextHashValueItem, NewHashRule#hash_rule.node_tree),
				TmpNode = to_node(VirtualNodeName),
				case list_to_atom(Node) =:= TmpNode of
					true -> TmpNextNodeSet;
					false -> gb_sets:add_element(TmpNode, TmpNextNodeSet)
				end
			end, gb_sets:new(), gb_sets:to_list(NextHashValueSet)),
	{gb_sets:to_list(NextNodeSet), NewHashRule}.

%%移除节点更新哈希规则
-spec update_hash_rule(Node :: node(), HashRule :: #hash_rule{}, remove) -> NewHashRule :: #hash_rule{}.
update_hash_rule(Node, HashRule, remove) when is_atom(Node) andalso is_record(HashRule, hash_rule) ->
	update_hash_rule(atom_to_list(Node), HashRule, remove);
update_hash_rule(Node, HashRule, remove) ->
	lists:foldr(
		fun(VirtualIndex, #hash_rule{node_tree = NodeTree, rem_value_list = RemValueList} = TmpHashRule) ->
			VirtualNodeName = hash_service_util:to_virtual_node(Node, VirtualIndex),
			HashValue = hash_service_util:hash_string(VirtualNodeName),
			RemValue = HashValue rem ?MAX_INDEX,
			case gb_trees:is_defined(RemValue,NodeTree) of
				false -> TmpHashRule;
				true ->
					TmpHashRule#hash_rule{
						node_tree = gb_trees:delete(RemValue, NodeTree),
						rem_value_list = lists:delete(RemValue, RemValueList)}
			end
		end, HashRule, lists:seq(1, ?VIRTUAL_NODE_COUNT)).


%%更新之前的hash列表，返回我顺时针的下一个hash值(值从小到大)(该hash值对应的节点上的部分key会映射到当前的hash对应的节点上
update_hash_list(HashValue, []) -> {[HashValue], -1};
update_hash_list(HashValue, [Item]) ->
	if
		HashValue > Item -> {[Item, HashValue], Item};
		HashValue < Item -> {[HashValue, Item], Item}
	end;
update_hash_list(HashValue, HashList) ->
	{NewHashList, NextHashValue} = get_next_hash_value(HashValue, HashList, []),
	{lists:reverse(NewHashList), NextHashValue}.

get_next_hash_value(HashValue, [BackHV, NextHV|Tail], BackList) ->
	if
		HashValue < BackHV ->
			{lists:reverse(Tail) ++ [NextHV, BackHV, HashValue|BackList], BackHV};
		HashValue > NextHV ->
			case Tail of
				[] ->
					case BackList of
						[] ->
							{[HashValue, NextHV, BackHV], BackHV};
						BackList ->
							[Head|_] =  lists:reverse(BackList),
							{[HashValue, NextHV, BackHV|BackList], Head}
					end;
				Tail ->
					get_next_hash_value(HashValue, [NextHV|Tail], [BackHV|BackList])
			end;
		true ->
			{lists:reverse(Tail) ++ [NextHV, HashValue, BackHV|BackList], NextHV}
	end.

%%根据key找到对应的存储节点
-spec find_key_store_node(Key :: list(), HashRule :: #hash_rule{}) -> fail | {success, Node :: node()}.
find_key_store_node(Key, HashRule) when is_list(Key) andalso is_record(HashRule, hash_rule) ->
	KeyHashValue = hash_string(Key),
	KeyRemValue = KeyHashValue rem ?MAX_INDEX,
	case find_key_store_node_index(KeyRemValue, HashRule#hash_rule.rem_value_list) of
		{success, NodeRemValue}  -> {success, to_node(gb_trees:get(NodeRemValue, HashRule#hash_rule.node_tree))};
		fail -> fail
	end.


%%查找key的存储节点的哈希值
find_key_store_node_index(_KeyRemValue, []) -> fail;
find_key_store_node_index(KeyRemValue, HashValueList) ->
	find_key_store_node_index(KeyRemValue, HashValueList, []).

find_key_store_node_index(_KeyRemValue, [], L) ->
    [NodeIndex|_] = lists:reverse(L),
	{success, NodeIndex};
find_key_store_node_index(KeyRemValue, [NodeRemValue|T], L) when KeyRemValue > NodeRemValue ->
	find_key_store_node_index(KeyRemValue, T, [NodeRemValue|L]);
find_key_store_node_index(KeyRemValue, [NodeRemValue|_], _) when KeyRemValue =< NodeRemValue ->
	{success, NodeRemValue}.





