%% Copyright
-module(apns_api).
-author("yaohong").

-include("file_log.hrl").
-include("apns.hrl").
%% API
-export([apns_push/4]).


-spec apns_push(GameName :: atom(), PushMod :: sandbox | product, Device :: string(), Msg :: string()) -> ok.
apns_push(GameName, PushMode, Device, Msg) when is_atom(GameName) andalso is_atom(PushMode) andalso is_list(Device) andalso is_list(Msg) ->
	catch gen_server:cast(apns_work, {apns_push, {GameName, PushMode, Device, Msg}}).


