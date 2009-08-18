%%%-------------------------------------------------------------------
%%% File        : stoplight_listener.erl
%%% Author      : Nate Murray <nmurray@attinteractive>
%%% Description : 
%%% Created     : 2009-08-12
%%%-------------------------------------------------------------------
-module(stoplight_listener).
-include_lib("../include/defines.hrl").
-behaviour(gen_server).

-export([start_link/2]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-compile(export_all).
-record(state, {pid}).

start_link(_Type, _Args) ->
    gen_server:start_link({local, ?STOPLIGHT_LISTENER}, ?MODULE, _InitOpts=[], _GenServerOpts=[]).

start_named_link(Name, _Type, _Args) ->
    gen_server:start_link({local, Name}, ?MODULE, _InitOpts=[], _GenServerOpts=[]).

init(_Args) -> {ok, #state{pid=self()}}.

handle_call({try_mutex, Name, Timeout}, From, State) ->
    {ClientPid, _Tag} = From,
    {ok, Pid} = stoplight_lobbyist:start([{name, Name}, {client, ClientPid}, {request_ttl, Timeout}]),
    spawn(fun() ->
       ?enable_tracing,
       ok = gen_server:call(Pid, petition)
    end),
    {reply, {ok, Pid}, State};

handle_call(state, _From, State)    -> {reply, {ok, State}, State};
handle_call(_Request, _From, State) -> {reply, okay, State}.
handle_cast(_Msg, State)            -> {noreply, State}.
handle_info(Info, State)           -> 
    ?TRACE("rec'd INFO", Info),
    {noreply, State}.
terminate(_Reason, _State)          -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

pid() ->
    whereis(?STOPLIGHT_LISTENER).


