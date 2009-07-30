%% 
%% Stoplight server supervisor
%% 
-module(stoplight_srv_sup).
-behaviour(supervisor).
-include_lib("../include/defines.hrl").

-export([start/0, start/1]).
-export([start_in_shell_for_testing/0, start_link/1, init/1]).

start()     -> spawn(fun() -> supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []) end).
start(Args) -> spawn(fun() -> supervisor:start_link({local, ?MODULE}, ?MODULE, Args) end).

start_in_shell_for_testing() ->
	{ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []),
	unlink(Pid).

start_link(Args) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init(Args) ->
	RestartStrategy = one_for_one,
	MaxRestarts = 1000,
	MaxTimeBetRestarts = 3600,
	TimeoutTime = 5000,

	SupFlags = {RestartStrategy, MaxRestarts, MaxTimeBetRestarts},
	
	NodeServer  = {?SERVER_MODULE, {?SERVER_MODULE, start_link, Args}, permanent, TimeoutTime, worker, [?SERVER_MODULE]},
	LoadServers = [NodeServer],

	{ok, {SupFlags, LoadServers}}.
