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

-export([start_link/2]).

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

init(_Args) -> {ok, #state{pid=self()}}.

handle_call({try_mutex, Name}, From, State) ->
    {ok, Pid} = stoplight_lobbyist:start([{name, Name}, {client, From}]),
    {reply, {ok, Pid}, State};

handle_call(state, _From, State)    -> {reply, {ok, State}, State};
handle_call(_Request, _From, State) -> {reply, okay, State}.
handle_cast(_Msg, State)   -> {noreply, State}.
handle_info(_Info, State)  -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
