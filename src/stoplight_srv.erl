%%%-------------------------------------------------------------------
%%% File    : stoplight_srv.erl
%%% Author  : nmurray
%%% Description : desc
%%% Created     : 2009-07-30
%%%-------------------------------------------------------------------

-module(stoplight_srv).
-behaviour(gen_server).
-include_lib("../include/defines.hrl").

-export([start_link/2]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

% debug
-compile(export_all).

%% Macros
-define(SERVER, ?MODULE).
% -define(SERVER, node()).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start() -> {ok,Pid} | ignore | {error,Error}
%% Description: Alias for start_link
%%--------------------------------------------------------------------
% start() ->
%     start_link(?DEFAULT_CONFIG). 

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(_Type, _Args) ->
  io:format(user, "Got ~p in start_link for ~p~n", [{}, ?MODULE]),
  gen_server:start_link({local, ?SERVER}, ?MODULE, _InitOpts=[], _GenServerOpts=[]).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------

init([]) -> 
    ?TRACE("Starting Stoplight Server", self()),
    InitialState = #srv_state{
                      pid=self(),
                      nodename=node(),
                      ring=[]
                   },
    {Resp, State01} = start_cluster_if_needed(InitialState),
    {ok, State02} = join_existing_cluster(State01),
    {ok, State02}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call(_Request, _From, State) -> 
    {reply, okay, State}.

% e.g.
% handle_call({create_ring}, _From, State) ->
%     {Reply, NewState} = handle_create_ring(State),
%     {reply, Reply, NewState};
%
% handle_call({join, OtherNode}, _From, State) ->
%     {Reply, NewState} = handle_join(OtherNode, State),
%     {reply, Reply, NewState};
% ...
% etc.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) -> 
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) -> 
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) -> 
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.

%%--------------------------------------------------------------------
%% Func: handle_join(OtherNode, State, Extra) -> {{ok, OtherNodes}, NewState}
%% Description: Called When another node joins the server cluster. 
%% Give that node the list of the other sigma servers
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Func: handle_leave(OtherNode, State, Extra) -> {ok, NewState}
%% Description: Called When another node leaves the server cluster. 
%% Give that node the list of the other sigma servers
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Func: join_existing_cluster(State) -> {ok, NewState}
%% Description: Look for any existing servers in the cluster, try to join them
%%--------------------------------------------------------------------
join_existing_cluster(State) ->
    Servers = stoplight_misc:get_existing_servers(stoplight),
    stoplight_misc:connect_to_servers(Servers),
    ?TRACE("servers", Servers),
    {ok, State}.


%%--------------------------------------------------------------------
%% Func: start_cluster_if_needed(State) -> {{ok, yes}, NewState} |
%%                                         {{ok, no}, NewState}
%% Description: Start cluster if we need to
%%--------------------------------------------------------------------
start_cluster_if_needed(State) ->
    {_Resp, NewState} = case whereis_name(?SERVER_GLOBAL) -> of
      undefined ->
          start_cluster(State);
      _ ->
          {no, State}
    end,
    {{ok, Resp}, NewState}.

%%--------------------------------------------------------------------
%% Func: start_cluster(State) -> {yes, NewState} | {no, NewState}
%% Description: Start a new cluster, basically just globally register a pid for
%% joining
%%--------------------------------------------------------------------
start_cluster(State) ->
    RegisterResp = global:register_name(?SERVER_GLOBAL, self()),
    {RegisterResp, State}.
