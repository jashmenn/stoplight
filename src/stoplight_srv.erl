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
  gen_server:start_link({local, stoplight_srv_local}, ?MODULE, _InitOpts=[], _GenServerOpts=[]).

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
                      ring=[self()],
                      reqQs=dict:new(),
                      owners=dict:new()
                   },

    {ok, State01} = join_existing_cluster(InitialState),
    {_Resp, State02} = start_cluster_if_needed(State01),
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

handle_call({join, FromSrv}, _From, State) ->
    {ok, NewState} = handle_node_joining(FromSrv, State),
    ?TRACE("join ok", NewState),
    Reply = {ok, NewState#srv_state.ring},
    {reply, Reply, NewState};

handle_call({joined_announcement, KnownRing}, _From, State) ->
    {ok, NewState} = handle_node_joined_announcement(KnownRing, State),
    ?TRACE("rec'd node join announcement", NewState),
    Reply = {ok, NewState#srv_state.ring},
    {reply, Reply, NewState};

handle_call({mutex, Tag, Req}, From, State) when is_record(Req, req) ->
% if (ci, t') appears in (cowner, towner) or ReqQ then 
%   if t < t' then skip the rest;  {the message received is an older message} 
%   if t > t' then Delete(ci, tÕ, ReqQ, cowner, towner); 
%
% then handle_mutex from here
    {reply, todo, State};

handle_call({state}, _From, State) ->
    ?TRACE("state:", State),
    {reply, ok, State};

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
handle_info({'DOWN', _MonitorRef, process, Pid, Info}, State) ->
    ?TRACE("received 'DOWN'. Removing node from list. info was:", Info),
    {ok, NewState} = remove_pid_from_ring(Pid, State),
    ?TRACE("NewState is:", NewState),
    {noreply, NewState};
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
%% Func: handle_join(OtherNode, State) -> {{ok, OtherNodes}, NewState}
%% Description: Called When another node joins the server cluster. 
%% Give that node the list of the other sigma servers
%%--------------------------------------------------------------------
handle_node_joining(OtherNode, State) ->
    {ok, NewState} = add_node_if_needed(OtherNode, State),
    {ok, NewState}.

%%--------------------------------------------------------------------
%% Func: handle_node_joined_announcement(KnownRing, State) -> {ok, NewState}
%% Description: When a node joins a known server, it then broadcasts to all
%% other servers that it joined.  % It tells all other servers about the entire
%% pidlist it received from the known node. This is a check to make sure % that
%% everytime a node joins all the other nodes know about it as well as every
%% other node in the cluster.
%%--------------------------------------------------------------------
handle_node_joined_announcement(KnownRing, State) ->
    {ok, NewState} = add_pids_to_ring(KnownRing, State),
    {ok, NewState}.

%%--------------------------------------------------------------------
%% Func: handle_leave(OtherNode, State, Extra) -> {ok, NewState}
%% Description: Called When another node leaves the server cluster. 
%% Give that node the list of the other sigma servers
%%--------------------------------------------------------------------

handle_mutex({request, Req}, From, State) ->
    {reply, todo, State};

handle_mutex({yield, Req}, From, State) ->
    {reply, todo, State};

handle_mutex({release, Req}, From, State) ->
    {reply, todo, State};

handle_mutex({inquiry, Req}, From, State) ->
    {reply, todo, State}.

%%--------------------------------------------------------------------
%% Func: join_existing_cluster(State) -> {ok, NewState}
%% Description: Look for any existing servers in the cluster, try to join them
%%--------------------------------------------------------------------
join_existing_cluster(State) ->
    Servers = stoplight_misc:get_existing_servers(stoplight),
    stoplight_misc:connect_to_servers(Servers),
    global:sync(), % otherwise we may not see the pid yet
    NewState = case global:whereis_name(?SERVER_GLOBAL) of % join unless we are the main server 
        undefined ->
            ?TRACE("existing cluster undefined", undefined),
            ok;
        X when X =:= self() ->
            ?TRACE("we are the cluster, skipping", X),
            ok;
        _ ->
            ?TRACE("joining server...", global:whereis_name(?SERVER_GLOBAL)),
            {ok, KnownRing} = gen_server:call({global, ?SERVER_GLOBAL}, {join, State}),
            {ok, NewInformedState} = add_pids_to_ring(KnownRing, State),
            broadcast_join_announcement(NewInformedState)
    end,
    {ok, NewState}.

broadcast_join_announcement(State) ->
    NotSelfPids   = lists:delete(self(), State#srv_state.ring),
    NotGlobalPids = lists:delete(global:whereis_name(?SERVER_GLOBAL), NotSelfPids),
    % multicall? 
    [gen_server:call(Pid, {joined_announcement, State#srv_state.ring}) || Pid <- NotGlobalPids],
    State.

%%--------------------------------------------------------------------
%% Func: start_cluster_if_needed(State) -> {{ok, yes}, NewState} |
%%                                         {{ok, no}, NewState}
%% Description: Start cluster if we need to
%%--------------------------------------------------------------------
start_cluster_if_needed(State) ->
    global:sync(), % otherwise we may not see the pid yet
    {Resp, NewState} = case global:whereis_name(?SERVER_GLOBAL) of
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
    ?TRACE("Starting server:", ?SERVER_GLOBAL),
    RegisterResp = global:register_name(?SERVER_GLOBAL, self()),
    {RegisterResp, State}.

add_node_if_needed(OtherNode, State) ->
    OtherPid = OtherNode#srv_state.pid,
    add_pid_to_ring(OtherPid, State).

add_pids_to_ring([Head|OtherPids], State) ->
    {ok, NewState} = add_pid_to_ring(Head, State),
    add_pids_to_ring(OtherPids, NewState);
add_pids_to_ring([], State) ->
    {ok, State}.

add_pid_to_ring(OtherPid, State) ->
    Exists = lists:any(fun(Elem) -> Elem =:= OtherPid end, State#srv_state.ring),
    NewRing = case Exists of
        true ->
          State#srv_state.ring;
        false ->
          % monitor that pid
          erlang:monitor(process, OtherPid),
          % add the other pid to our ring
          [OtherPid|State#srv_state.ring]
    end,
    NewState  = State#srv_state{ring=NewRing},
    {ok, NewState}.

remove_pid_from_ring(OtherPid, State) ->
    NewRing = lists:delete(OtherPid, State#srv_state.ring),
    NewState  = State#srv_state{ring=NewRing},
    {ok, NewState}.

% ---- mutex helpers
have_previous_request_from_this_client(Req) -> % {true, OtherReq} | false
    todo.

% look at Req.owner, see if it is from our current owner, namespaced by name
is_request_from_current_owner(Req, State) when is_record(Req, req) -> % {true, CurrentOwner} | false
    #req{owner=Owner, name=Name} = Req,
    case current_owner_for_name(Name, State) of
        {ok, CurrentOwner} ->
            #req{owner=CurrentOwnerId} = CurrentOwner,
            #req{owner=ThisOwnerId}    = Req,
            case CurrentOwnerId =:= ThisOwnerId of
                true ->
                    {true, CurrentOwner};
                false ->
                    false
            end;
        _ ->
            false
    end.

% look at Req.owner, see if we have another request from this same owner in our
% queue that is namespaced by Req.name. If so, return the OtherReq that is in
% the queue. 
is_there_a_request_from_owner_in_the_queue(Req, State) -> % {true, OtherReq} | false
    #req{owner=Owner, name=Name} = Req,
    Q = queue_for_name(Name, State),
    case Q of 
        {ok, Queue} ->
             ReqsForOwner = lists:filter(fun(Elem) ->
                         #req{owner=ElemOwner} = Elem,
                         ElemOwner =:= Owner
                end, 
             Q),
             ReqsForOwnerSorted = stoplight_request:sort_by_timestamp(ReqsForOwner),
             {true, lists:nth(1, ReqsForOwnerSorted)};
        _ ->
            false
    end.



% checks the current owners list, namespaced by name
% CurrentOwner = #req
current_owner_for_name(Name, State) -> % {ok, req#CurrentOwner} | error
    #srv_state{owners=Owners} = State, 
    dict:find(Name, Owners).

% Queue = list() of #req
queue_for_name(Name, State) -> % {ok, Queue} | error
    #srv_state{reqQs=ReqQs} = State,
    dict:find(Name, reqQs).
