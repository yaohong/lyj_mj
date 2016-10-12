-module(uuid_server).
-author('erlangonrails@gmail.com').
-behaviour(gen_server).

-export([get/0,
         get_algorithm/0,
         start_link/0]).

-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
         terminate/2, 
         code_change/3]).

%% 产生32 bytes的uuid
-spec get() -> 
    binary().
get() ->
    gen_server:call(?MODULE, 'generate_uuid', infinity).

-spec get_algorithm() -> 
    random | utc_random | sequential.
get_algorithm() ->
    gen_server:call(?MODULE, 'get_algorithm', infinity).

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, state()}.

handle_call('generate_uuid', _From, random) ->
    {reply, random(), random};
handle_call('generate_uuid', _From, utc_random) ->
    {reply, utc_random(), utc_random};
handle_call('generate_uuid', _From, {sequential, Pref, Seq}) ->
    Result = iolist_to_binary(Pref ++ io_lib:format("~6.16.0b", [Seq])),
    case Seq >= 16#fff000 of
        true ->
            {reply, Result, {sequential, new_prefix(), inc()}};
        _ ->
            {reply, Result, {sequential, Pref, Seq + inc()}}
    end;
handle_call('get_algorithm', _From, State) ->
    Reply = 
      case State of
          random -> random;
          utc_random -> utc_random;
          {sequential, _, _} -> sequential
      end,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Internal APIs:
-spec get_uuid_algorithm() -> atom().
get_uuid_algorithm() ->
    {ok, Algorithm} = application:get_env('uuid', algorithm),
    Algorithm.

state() ->
    case get_uuid_algorithm() of
        random ->
            random;
        utc_random ->
            utc_random;
        sequential ->
            {sequential, new_prefix(), inc()}
    end.

new_prefix() ->
    relax_hex:to_hex((crypto:rand_bytes(13))).

inc() ->
    crypto:rand_uniform(1, 16#ffe).


random() ->
    list_to_binary(relax_hex:to_hex(crypto:rand_bytes(16))).

utc_random() ->
    Now = {_, _, Micro} = now(),
    Nowish = calendar:now_to_universal_time(Now),
    Nowsecs = calendar:datetime_to_gregorian_seconds(Nowish),
    Then = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Prefix = io_lib:format("~14.16.0b", [(Nowsecs - Then) * 1000000 + Micro]),
    list_to_binary(Prefix ++ relax_hex:to_hex(crypto:rand_bytes(9))).
