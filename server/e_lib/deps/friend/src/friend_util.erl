%% Copyright
-module(friend_util).
-author("yaohong").

%% API
-export([
	ensure_app_started/1]).

-spec ensure_app_started(App :: atom()) -> ok.
ensure_app_started(App) ->
	case application:start(App) of
		ok ->
			ok;
		{error, {already_started, App}} ->
			ok
	end.
