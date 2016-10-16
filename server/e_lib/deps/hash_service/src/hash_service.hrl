%%好友常规的返回值
-ifndef(_HASH_SERVICE_HRL).
-define(_HASH_SERVICE_HRL, true).
-record(hash_rd, {index, value}).

-record(hash_rule, {
	node_tree :: gb_tree(),								%%存放存储节点的树
	rem_value_list :: [RemValue :: integer()]           %%存放节点hash值的列表
}).
-define(VIRTUAL_NODE_COUNT, 1).
-define(MAX_INDEX, 16#FFFFFFFF).




-endif.
