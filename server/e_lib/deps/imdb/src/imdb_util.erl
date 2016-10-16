-module(imdb_util).
-author('erlangonrails@gmail.com').
-include("netlib.hrl").
-export([ensure_app_started/1,
         get_imdb_pool_size/0,
         get_imdb_start_interval/0,
         get_imdb_proxy/0,
         length/1,
         to_string/1,
         to_integer/1,
         items_to_string/1]).

-spec ensure_app_started(App :: atom()) -> ok.
ensure_app_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

-spec get_imdb_pool_size() ->
    netlib_non_neg_integer().
get_imdb_pool_size() ->
    {ok, PoolSize} = application:get_env('imdb', 'imdb_pool_size'),
    PoolSize.

-spec get_imdb_start_interval() ->
    netlib_non_neg_integer().
get_imdb_start_interval() ->
    {ok, StartInterval} = application:get_env('imdb', 'imdb_start_interval'),
    StartInterval.

-spec get_imdb_proxy() ->
    netlib_address().
get_imdb_proxy() ->
    {ok, Address} = application:get_env('imdb', 'imdb_proxy'),
    Address.

-spec length(Data :: netlib_string() | netlib_binary()) ->
    netlib_integer().
length(Data) when is_binary(Data) ->
    erlang:size(Data);
length(Data) when is_list(Data) ->
    erlang:iolist_size(Data).

-spec to_string(Data :: netlib_string() | netlib_binary()) ->
    netlib_string().
to_string(Data) when is_list(Data) ->
    Data;
to_string(Data) when is_binary(Data)->
    binary_to_list(Data).

-spec to_integer(Data :: netlib_string() | netlib_binary()) ->
    netlib_integer().
to_integer(Data) when is_list(Data) ->
    list_to_integer(Data);
to_integer(Data) when is_binary(Data) ->
    D = binary_to_list(Data),
    list_to_integer(D).

%% 参数是列表的列表, 每个子列表是一行记录, 将列表的每个元素转换成list()形式.
-spec items_to_string(Items :: [netlib_list()]) ->
    [list()].
items_to_string(Items) ->
    lists:map(fun(Row) ->
                  items_to_string_internal(Row)
              end, Items).

items_to_string_internal(Row) ->
    lists:map(fun(I) ->
                  imdb_util:to_string(I)
              end, Row).
 
