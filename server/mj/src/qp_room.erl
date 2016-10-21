%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 十月 2016 16:33
%%%-------------------------------------------------------------------
-module(qp_room).
-author("yaohong").

-behaviour(gen_fsm).

%% API
-export([start_link/2]).

%% gen_fsm callbacks
-export([init/1,
         idle/2,
         game/2,
         idle/3,
         game/3,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).
-export([join/2]).
-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
join(RoomPid, UserKey) ->
    gen_fsm:sync_send_event(RoomPid, {join, UserKey}).
%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(OwnerUserId :: integer(), RoomType :: integer()) ->
    {ok, pid()} | ignore | {error, Reason :: term()}).
start_link(OwnerUserId, RoomType) ->
    gen_fsm:start_link(?MODULE, [OwnerUserId, RoomType], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, StateName :: atom(), StateData :: #state{}} |
    {ok, StateName :: atom(), StateData :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    {ok, state_name, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @end
%%--------------------------------------------------------------------
-spec(idle(Event :: term(), State :: #state{}) ->
    {next_state, NextStateName :: atom(), NextState :: #state{}} |
    {next_state, NextStateName :: atom(), NextState :: #state{},
     timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
idle(_Event, State) ->
    {next_state, state_name, State}.


game(_Event, State) ->
    {next_state, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(idle(Event :: term(), From :: {pid(), term()},
           State :: #state{}) ->
              {next_state, NextStateName :: atom(), NextState :: #state{}} |
              {next_state, NextStateName :: atom(), NextState :: #state{},
               timeout() | hibernate} |
              {reply, Reply, NextStateName :: atom(), NextState :: #state{}} |
              {reply, Reply, NextStateName :: atom(), NextState :: #state{},
               timeout() | hibernate} |
              {stop, Reason :: normal | term(), NewState :: #state{}} |
              {stop, Reason :: normal | term(), Reply :: term(),
               NewState :: #state{}}).
idle({join, UserKey}, _From, State) ->
    ok;
idle(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, state_name, State}.


game(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_event(Event :: term(), StateName :: atom(),
                   StateData :: #state{}) ->
                      {next_state, NextStateName :: atom(), NewStateData :: #state{}} |
                      {next_state, NextStateName :: atom(), NewStateData :: #state{},
                       timeout() | hibernate} |
                      {stop, Reason :: term(), NewStateData :: #state{}}).
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_sync_event(Event :: term(), From :: {pid(), Tag :: term()},
                        StateName :: atom(), StateData :: term()) ->
                           {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term()} |
                           {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term(),
                            timeout() | hibernate} |
                           {next_state, NextStateName :: atom(), NewStateData :: term()} |
                           {next_state, NextStateName :: atom(), NewStateData :: term(),
                            timeout() | hibernate} |
                           {stop, Reason :: term(), Reply :: term(), NewStateData :: term()} |
                           {stop, Reason :: term(), NewStateData :: term()}).
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: term(), StateName :: atom(),
                  StateData :: term()) ->
                     {next_state, NextStateName :: atom(), NewStateData :: term()} |
                     {next_state, NextStateName :: atom(), NewStateData :: term(),
                      timeout() | hibernate} |
                     {stop, Reason :: normal | term(), NewStateData :: term()}).
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: normal | shutdown | {shutdown, term()}
                | term(), StateName :: atom(), StateData :: term()) ->
                   term()).
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, StateName :: atom(),
                  StateData :: #state{}, Extra :: term()) ->
                     {ok, NextStateName :: atom(), NewStateData :: #state{}}).
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
