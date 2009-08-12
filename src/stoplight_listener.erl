%%%-------------------------------------------------------------------
%%% File    : stoplight_listener.erl
%%% Author  : Nate Murray <nmurray@attinteractive>
%%% Description : 
%%% Created     : 2009-08-12
%%%-------------------------------------------------------------------

%% Stoplight listener
-module(stoplight_listener).
-include_lib("../include/defines.hrl").
-behaviour(gen_server).

-export([start_link/2, start_named/2]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-compile(export_all).
-record(state, {pid}).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(_Type, _Args) ->
    gen_cluster:start_link({local, stoplight_listener}, ?MODULE, _InitOpts=[], _GenServerOpts=[]).

%% for testing multiple servers
start_named(Name, Config) ->
    gen_cluster:start_link({local, Name}, ?MODULE, [Config], []).

%% gen_server callbacks
init(_Args) -> {ok, #state{pid=self()}}.

handle_call({try_mutex, Name}, From, State) ->
    Pid = spawn(stoplight_lobbyist, start_named, [[{name, Name}, {client, From}]]),
    {reply, {ok, Pid}, State};

handle_call(state, _From, State)    -> {reply, {ok, State}, State};
handle_call(_Request, _From, State) -> {reply, okay, State}.
handle_cast(_Msg, State)   -> {noreply, State}.
handle_info(_Info, State)  -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% where we're at: http://www.trapexit.org/Building_a_Non-blocking_TCP_server_using_OTP_principles
%% write a non-blocking listener. stoplight_interface_sup,
%% stoplight_interface_listener . listener immediately spawns off a lobbyist
%% and hands the request over to them. we probably need to supervise the
%% lobbyists, though im not sure why. the server actually is already monitoring
%% them, so no real reason to need to do so. better idea is have the process be
%% linked to whatever user is talking to it. 
