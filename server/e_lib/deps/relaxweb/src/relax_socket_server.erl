-module(relax_socket_server).
-author('erlangonrails@gmail.com').
-include("file_log.hrl").
-behaviour(gen_server).

-define(RECBUF_SIZE, 8192).

-export([start/1,      %% 启动socket_server, 挂在监控树下
         start_link/1, %% 启动独立的socket_server, 没有监控树
         stop/1]).

-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         terminate/2, 
         code_change/3,
         handle_info/2]).

-export([get/2, set/3]).


%% active_sockets的计算方式:
%% 1. 当acceptor进程接收到一个socket的时候, 会向socket_server进程
%%    发送{accepted, Pid, Timeing}这样的消息, 收到消息后
%%    active_socket + 1, 多了一个socket在处理数据.
%% 2. 当acceptor进程死掉之后, socket_server进程会收到{'EXIT', Pid, _}
%%    的退出消息, 这时候, active_socket - 1, 表示一个socket
%%    已经处理完成.

-record(relax_socket_server,
          {port :: relax_socket:socket_port(),      
           ip = any :: any | inet:ip_address(),    
           loop :: fun() | {M :: atom(), F :: atom()} | {M :: atom(), F :: atom(), A :: list()},  
           name = {local, ?MODULE},
           listen = null :: relax_socket:socket(),
           nodelay = false :: boolean(),
           backlog = 128 :: integer(),
           active_sockets = 0 :: integer(),
           acceptor_pool_size = 16 :: integer(),
           ssl = false :: boolean(),
           ssl_opts = [{ssl_imp, new}] :: list(),
           acceptor_pool = sets:new(),
           profile_fun = undefined :: fun()}).

-spec start_link(Options :: list() | #relax_socket_server{}) ->
    {ok, pid()} | {error, _}.
start_link(Options) ->
    start_server(parse_options(Options)).

-spec start(Options :: list() | #relax_socket_server{}) ->
    {ok, pid()} | {error, _}.
start(Options) ->
    {name, Name} = proplists:lookup(name, Options),
    ChildSpec = {Name,
                 {?MODULE, start_link, [Options]},
                 transient,
                 brutal_kill,
                 worker,
                 [?MODULE]},
    supervisor:start_child(relax_sup, ChildSpec).

-spec get(Name :: pid() | atom(), Property :: port | active_sockets) -> 
    term() | undefined.
get(Name, port) ->
    gen_server:call(Name, {get, port});
get(Name, active_sockets) ->
    gen_server:call(Name, {get, active_sockets});
get(_, _) ->
    undefined.

-spec set(
        Name :: pid() | atom(), 
        Property :: profile_fun, 
        Fun :: fun()) ->
    ok | error.
set(Name, profile_fun, Fun) ->
    gen_server:cast(Name, {set, profile_fun, Fun});
set(Name, Property, _Value) ->
    ?FILE_LOG_WARNING("set for ~p with ~p not implemented~n", [Name, Property]),
    error.

%% 注意:
%% Stop一个relax_socket_server, 并不会kill已经开始的工作进程, 这些工作进程会在完成
%% 业务逻辑后自己退出.
%%
%% 有可能会返回{error, running}, 这个时候可以再次调用stop/1来尝试停止server.
-spec stop(Name :: atom()) -> ok | {error, Term :: term()}.
stop(Name) when is_atom(Name) ->
    gen_server:cast(Name, stop),
    timer:sleep(1000),
    %% 注意:
    %% 停止之后应该从监控树删除, 否则再调用start/1启动的时候抛出下面错误:
    %% {error,already_present}
    case supervisor:delete_child(relax_sup, Name) of
        {error, not_found} -> ok;
        ErrorTerm -> ErrorTerm
    end.

%% Internal API:

-spec parse_options(State :: list() | #relax_socket_server{}) ->
    #relax_socket_server{}.
parse_options(State = #relax_socket_server{}) ->
    State;
parse_options(Options) ->
    parse_options(Options, #relax_socket_server{}).

parse_options([], State) ->
    State;


%% 支持的选项:
%% name :: atom()
%% port :: relax_socket:socket_port()
%% ip :: any | inet:ip_address()
%% loop :: fun() | {M :: atom(), F :: atom()} | {M :: atom(), F :: atom(), A :: list()}
%% nodelay :: boolean()
%% backlog :: integer()
%% acceptor_pool_size :: integer()
%% ssl :: boolean()
%% ssl_opts :: list()
%% profile_fun :: fun()

parse_options([{name, A} | Rest], State) when is_atom(A) ->
    Name = {local, A},
    parse_options(Rest, State#relax_socket_server{name = Name});
parse_options([{port, Port} | Rest], State) ->
    parse_options(Rest, State#relax_socket_server{port = Port});
parse_options([{ip, Ip} | Rest], State) ->
    ParsedIp = case Ip of
                   any ->
                       any;
                   Ip when is_tuple(Ip) ->
                       Ip
               end,
    parse_options(Rest, State#relax_socket_server{ip = ParsedIp});
parse_options([{loop, Loop} | Rest], State) ->
    parse_options(Rest, State#relax_socket_server{loop = Loop});
parse_options([{backlog, Backlog} | Rest], State) ->
    parse_options(Rest, State#relax_socket_server{backlog = Backlog});
parse_options([{nodelay, NoDelay} | Rest], State) ->
    parse_options(Rest, State#relax_socket_server{nodelay = NoDelay});
parse_options([{acceptor_pool_size, Max} | Rest], State) when is_integer(Max) ->
    parse_options(Rest, State#relax_socket_server{acceptor_pool_size = Max});
parse_options([{ssl, Ssl} | Rest], State) when is_boolean(Ssl) ->
    parse_options(Rest, State#relax_socket_server{ssl = Ssl});
parse_options([{ssl_opts, SslOpts} | Rest], State) when is_list(SslOpts) ->
    SslOpts1 = [{ssl_imp, new} | proplists:delete(ssl_imp, SslOpts)],
    parse_options(Rest, State#relax_socket_server{ssl_opts = SslOpts1});
parse_options([{profile_fun, ProfileFun} | Rest], State) when is_function(ProfileFun) ->
    parse_options(Rest, State#relax_socket_server{profile_fun = ProfileFun}).

-spec start_server(State :: #relax_socket_server{}) -> 
    {ok, pid()} | {error, _}.
start_server(State = #relax_socket_server{ssl = Ssl, name = Name}) ->
    ok = prep_ssl(Ssl),
    gen_server:start_link(Name, ?MODULE, State, []).

-spec prep_ssl(Ssl :: boolean()) -> ok.
prep_ssl(true) ->
    ok = relax_util:ensure_app_started(crypto),
    ok = relax_util:ensure_app_started(public_key),
    ok = relax_util:ensure_app_started(ssl),
    ok = relax_util:ensure_app_started(relaxweb),
    ok = relax_util:ensure_app_started(file_log);
prep_ssl(false) ->
    ok = relax_util:ensure_app_started(relaxweb),
    ok = relax_util:ensure_app_started(file_log),
    ok.

-spec ipv6_supported() -> boolean().
ipv6_supported() ->
    case (catch inet:getaddr("localhost", inet6)) of
        {ok, _Addr} -> 
            true;
        {error, _} -> 
            false
    end.

-spec init(State :: #relax_socket_server{}) ->
    {ok, #relax_socket_server{}} | {stop, _}.
init(State = #relax_socket_server{ip = Ip, port = Port, backlog = Backlog, nodelay = NoDelay}) ->
    process_flag(trap_exit, true),
    BaseOpts = [binary,
                {reuseaddr, true},
                {packet, 0},
                {backlog, Backlog},
                {recbuf, ?RECBUF_SIZE},
                {active, false},
                {nodelay, NoDelay}],
    Opts = case Ip of
               any ->
                   case ipv6_supported() of % IPv4, and IPv6 if supported
                       true -> [inet, inet6 | BaseOpts];
                       _ -> BaseOpts
                   end;
               {_, _, _, _} -> % IPv4
                   [inet, {ip, Ip} | BaseOpts];
               {_, _, _, _, _, _, _, _} -> % IPv6
                   [inet6, {ip, Ip} | BaseOpts]
           end,
    listen(Port, Opts, State).

-spec new_acceptor_pool(
          Listen :: relax_socket:socket(), 
          State :: #relax_socket_server{}) ->
    #relax_socket_server{}.
new_acceptor_pool(Listen, State = #relax_socket_server{acceptor_pool = Pool,
                                                       acceptor_pool_size = Size,
                                                       loop = Loop}) ->
    F = fun (_, S) ->
            Pid = relax_acceptor:start_link(self(), Listen, Loop),
            sets:add_element(Pid, S)
        end,
    Pool1 = lists:foldl(F, Pool, lists:seq(1, Size)),
    State#relax_socket_server{acceptor_pool = Pool1}.

-spec listen(
        Port :: relax_socket:socket_port(), 
        Opts :: list(), 
        State :: #relax_socket_server{}) ->
    {ok, #relax_socket_server{}} | {stop, _}.
listen(Port, Opts, State = #relax_socket_server{ssl = Ssl, ssl_opts = SslOpts}) ->
    case relax_socket:listen(Ssl, Port, Opts, SslOpts) of 
        {ok, Listen} ->
            {ok, {ListenAddr, ListenPort}} = relax_socket:sockname(Listen),
            ?FILE_LOG_INFO("relaxweb listen on ~s:~p", [inet_parse:ntoa(ListenAddr), ListenPort]),
            {ok, new_acceptor_pool(Listen, State#relax_socket_server{listen = Listen,
                                                                     port = ListenPort})};
        {error, Reason} ->
            {stop, Reason}
    end.

state_to_proplist(#relax_socket_server{name = {local, Name},
                                       port = Port,
                                       active_sockets = ActiveSockets}) ->
    [{name, Name}, {port, Port}, {active_sockets, ActiveSockets}].

handle_call({get, port}, _From, #relax_socket_server{port = Port} = State) ->
    {reply, Port, State};
handle_call({get, active_sockets}, _From, #relax_socket_server{active_sockets = ActiveSockets} = State) ->
    {reply, ActiveSockets, State};
handle_call(_Message, _From, State) ->
    {reply, undefined, State}.


%% 每接收一个客户端连接, 都会触发这个函数
handle_cast({accepted, Pid, Timing},
            State = #relax_socket_server{active_sockets = ActiveSockets}) ->
    State1 = State#relax_socket_server{active_sockets = 1 + ActiveSockets},
    case State#relax_socket_server.profile_fun of
        undefined ->
            undefined;
        F when is_function(F) ->
            %% [{timing, Timing :: integer()}, %% microseconds(微秒)
            %%  {name, Name :: atom()},
            %%  {port, Port :: relax_socket:socket_port()},
            %%  {active_sockets, ActiveSockets :: integer()}]
            catch F([{timing, Timing} | state_to_proplist(State1)])
    end,
    {noreply, recycle_acceptor(Pid, State1)};
handle_cast({set, profile_fun, ProfileFun}, State) ->
    State1 = case ProfileFun of
                 ProfileFun when is_function(ProfileFun); ProfileFun =:= undefined ->
                     State#relax_socket_server{profile_fun = ProfileFun};
                 _ ->
                     State
             end,
    {noreply, State1};
handle_cast(stop, State) ->
    {stop, normal, State}.


terminate(_Reason, #relax_socket_server{listen = Listen}) ->
    relax_socket:close(Listen).

code_change(_OldVsn, State, _Extra) ->
    State.

recycle_acceptor(Pid, State = #relax_socket_server{acceptor_pool = Pool,
                                                   listen = Listen,
                                                   loop = Loop,
                                                   active_sockets = ActiveSockets}) ->
    case sets:is_element(Pid, Pool) of
        true ->
            %% {accepted, Pid, Timing} - 开始处理一个客户端连接的时候调用
            Acceptor = relax_acceptor:start_link(self(), Listen, Loop),
            Pool1 = sets:add_element(Acceptor, sets:del_element(Pid, Pool)),
            State#relax_socket_server{acceptor_pool = Pool1};
        false ->
            %% {'EXIT', Pid, _} - '处理完'一个客户端连接的时候调用
            State#relax_socket_server{active_sockets = ActiveSockets - 1}
    end.

%% 处理进程正常退出
handle_info({'EXIT', Pid, normal}, State) ->
    {noreply, recycle_acceptor(Pid, State)};
%% 处理进程异常退出
handle_info({'EXIT', Pid, Reason},
            State = #relax_socket_server{acceptor_pool = Pool}) ->
    case sets:is_element(Pid, Pool) of
        true ->
            ?FILE_LOG_INFO("acceptor_error:~p", [Reason]),
            timer:sleep(100);
        false ->
            ok
    end,
    {noreply, recycle_acceptor(Pid, State)};
handle_info(_Info, State) ->
    {noreply, State}.
