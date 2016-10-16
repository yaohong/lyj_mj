-module(tmp_sup).
-author('yh@gmail.com').
-export([start_link/2, 
		 start_link/3,
         init/1]).

%% ===================================================
%% 一个simple_one_for_one形式的监控树:
%%
%% <1> 参数
%% Name :: atom()   - supervisor进程的名字
%% Module :: atom() - 子进程的模块名字
%%
%% <2> 所有的子进程都是动态添加的
%% supervisor:start_child(Name, ArgList).
%% 本质上是使用下面方式启动子进程.
%% apply(Module, start_link, [] ++ ArgList)
%%
%% <3> 子进程都是temporary类性的, 不会被supervisor进程重启
%% ===================================================
start_link(Name, Module) ->
	start_link(Name, Module, temporary).

%%RestartMode : temporary
start_link(Name, Module, RestartMode) ->
    supervisor:start_link({local, Name}, ?MODULE, [Module, RestartMode]).

init([Module, RestartMode]) ->
    {ok, {{simple_one_for_one, 10, 1},
          [{undefined, {Module, start_link, []},
           RestartMode, brutal_kill, worker, [Module]}]}}.
