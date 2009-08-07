%%%-------------------------------------------------------------------
%%% File    : stoplight_srv.erl
%%% Author  : nmurray
%%% Description : desc
%%% Created     : 2009-07-30
%%%-------------------------------------------------------------------
-module(stoplight_srv).
-behaviour(gen_cluster).
-include_lib("../include/defines.hrl").

-export([start_link/2, start_named/2]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

% gen_cluster callback
-export([handle_join/3, handle_node_joined/3, handle_leave/4]).

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
    gen_cluster:start_link({local, stoplight_srv_local}, ?MODULE, _InitOpts=[], _GenServerOpts=[]).

%% for testing multiple servers
start_named(Name, Config) ->
    gen_cluster:start_link({local, Name}, ?MODULE, [Config], []).

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

init(_Args) -> 
    % ?TRACE("Starting Stoplight Server", self()),
    InitialState = #srv_state{
                      pid=self(),
                      nodename=node(),
                      reqQs=dict:new(),
                      owners=dict:new()
                   },
    {ok, InitialState}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({state}, _From, State) ->
    % ?TRACE("queried state:", State),
    {reply, {ok, State}, State};

% reply with the current_owner for Name. Do *not* use this as part of the
% algorithm. This is for test/debugging inspection only. 
handle_call({current_owner, Name}, _From, State) -> % -> {ok, CurrentOwner}
    CurrentOwner = current_owner_for_name_short(Name, State),
    {reply, {ok, CurrentOwner}, State};

% again, for testing/debug only. not part of the algorithm
handle_call({queue, Name}, _From, State) -> % -> {ok, Queue}
    Queue = queue_for_name_short(Name, State),
    {reply, {ok, Queue}, State};

handle_call(_Request, _From, State) -> 
    {reply, okay, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({mutex, Tag, Req}, State) when is_record(Req, req) ->
    case is_request_stale(Req, State) of
        true -> 
            % CurrentOwner = current_owner_for_name_short(Req#req.name, State),
            % {reply, {stale, CurrentOwner}, State}; % it is stale, don't give the mutex
            {noreply, State}; % it is stale, don't give the mutex
           _ -> handle_non_stale_mutex_cast({mutex, Tag, Req}, State)
    end;
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

handle_non_stale_mutex_cast({mutex, Tag, Req}, State) when is_record(Req, req) -> % {noreply, State}
    {ok, NewState} = case have_previous_request_from_this_client(Req, State) of
        {true, OrigReq} ->
             case Req#req.timestamp > OrigReq#req.timestamp of
                 true -> 
                     {ok, _CurrentOwner, State1} = delete_request(OrigReq, State),
                     {ok, State1};
                 _ -> {ok, State}
             end;
         _ ->
             {ok, State}
    end,
    ?TRACE("contining on to handle", {Tag, Req}),
    handle_mutex({Tag, Req}, NewState).


handle_mutex({request, Req}, State) -> % {noreply, State}
    case is_request_from_current_owner(Req, State) of
        {true, _CurrentOwner} -> 
            {noreply, State}; % hmm, TODO - maybe just respond with the current owned request?
        false -> 
            handle_mutex_request_from_not_owner(Req, State)
    end;

handle_mutex({release, Req}, State) ->
    handle_mutex_release(Req, State);

handle_mutex({yield, Req}, State) ->
    handle_mutex_yield(Req, State);

handle_mutex({inquiry, Req}, State) ->
    handle_mutex_inquiry(Req, State).
    
handle_mutex_inquiry(Req, State) ->
    case is_request_from_current_owner(Req, State) of
        {true, CurrentOwner} ->
            % {reply, {undefined}, State};
            {noreply, State};
        false ->
            case current_owner_for_name(Req#req.name, State) of
              % {ok, CurrentOwner} -> {reply, {response, CurrentOwner}, State};
              % undefined          -> {reply, {undefined}, State}
              {ok, CurrentOwner} -> {noreply, State};
              undefined          -> {noreply, State}
            end
    end.

handle_mutex_yield(Req, State) ->
    case is_request_the_current_owner_exactly(Req, State) of
        true ->
            {ok, State1} = append_request_to_queue(Req, State),
            {ok, State2} = promote_request_in_queue(Req#req.name, State1),
            CurrentOwner = current_owner_for_name_short(Req#req.name, State2),
            % {reply, {response, CurrentOwner}, State2};
            {noreply, State2};
        false ->
            % {reply, {undefined}, State}
            {noreply, State}
    end.

% 37   if (cowner, towner) = (ci, t) then 
% 38    insert (ci, t) into ReqQ, by predetermined order; 
% 39    (cowner, towner) := dequeue(ReqQ);   
% 40    send (RESPONSE, cowner, towner) to cowner; 
% 41    if cowner ­ ci then send (RESPONSE, cowner, towner) to ci;  
    


handle_mutex_request_from_not_owner(Req, State) ->
    {ok, CurrentOwnerReq, NewState} = case current_owner_for_name(Req#req.name, State) of
        undefined -> 
            {ok, NewState1} = set_current_owner(Req, State), 
            OwnerReq = current_owner_for_name_short(Req#req.name, NewState1),
            % {reply, {response, OwnerReq}, NewState}; % "response" is too general...
            {ok, OwnerReq, NewState1}; % "response" is too general...
        {ok, CurrentOwner} -> 
            case is_there_a_request_from_owner_in_the_queue(Req, State) of
                % NOTE: here we're saying that if this owner has ANY other
                % requests in the queue then it can't add any more. I'm not
                % sure if this is true to the algorithm. We may want to change
                % this to say "dont put on the exact same request". Not sure
                % though.  Actually, this clause should never be called, right?
                % b/c if someone calls to put a newer request in the queue then
                % it should be replaced. todo, verify
                % {true, _OtherReqs} -> {reply, {response, CurrentOwner}, State};
                {true, _OtherReqs} -> {ok, CurrentOwner, State};
                false -> 
                    {ok, NewState1} = append_request_to_queue(Req, State),
                    % {reply, {response, CurrentOwner}, NewState}
                    {ok, CurrentOwner, NewState1}
            end
    end,
    ReqPid = Req#req.owner,
    gen_cluster:cast(ReqPid, {mutex, response, CurrentOwnerReq}), 
    {noreply, NewState}.

handle_mutex_release(Req, State) ->
    {ok, CurrentOwner, NewState} = delete_request(Req, State),
    % {reply, {response, CurrentOwner}, NewState}.
    {noreply, NewState}.
    
have_previous_request_from_this_client(Req, State) -> % {true, OtherReq} | false
    case is_request_from_current_owner(Req, State) of
        {true, CurrentOwner} ->
            {true, CurrentOwner};
        _  ->
            is_there_a_request_from_owner_in_the_queue(Req, State)
    end.

is_request_stale(Req, State) ->
    case have_previous_request_from_this_client(Req, State) of
        {true, OtherReq} ->
            case Req#req.timestamp < OtherReq#req.timestamp of 
                true -> true;
                   _ -> false
            end;
        _ ->
            false
    end.

% look at Req.owner, see if it is from our current owner, namespaced by name
% TODO shorten this method
is_request_from_current_owner(Req, State) when is_record(Req, req) -> % {true, CurrentOwner} | false
    Name = Req#req.name,
    case current_owner_for_name(Name, State) of
        {ok, CurrentOwner} ->
            #req{owner=CurrentOwnerId} = CurrentOwner,
            #req{owner=ThisOwnerId}    = Req,
            case CurrentOwnerId =:= ThisOwnerId of
                true  -> {true, CurrentOwner};
                false -> false
            end;
        _ ->
            false
    end.

% checks to see if Req matches the #req in State#srv_state.owners / namespace 
is_request_the_current_owner_exactly(Req, State) -> % true | false
    case current_owner_for_name(Req#req.name, State)  of
        {ok, CurrentOwner} -> Req =:= CurrentOwner;
        _ -> false
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
             Queue),
             ReqsForOwnerSorted = stoplight_request:sort_by_timestamp(ReqsForOwner),
             {true, lists:nth(1, ReqsForOwnerSorted)};
        _ ->
            false
    end.

is_this_request_in_the_queue(Req, State) ->
    Q = queue_for_name(Req#req.name, State),
    case Q of
        {ok, Queue} -> lists:any(fun(Elem) -> Elem =:= Req end, Queue);
        _ -> false
    end.

% checks the current owners list, namespaced by name
% CurrentOwner = #req
current_owner_for_name(Name, State) -> % {ok, req#CurrentOwner} | undefined
    Owners = State#srv_state.owners,
    case dict:find(Name, Owners) of
        {ok, CurrentOwner} -> 
            OwnerName = CurrentOwner#req.owner,
            case OwnerName of  % also return undefined if we have an undefined owner in the record
                undefined -> 
                    undefined;
                _ -> 
                    {ok, CurrentOwner}
            end;
        error              -> undefined
    end.

% just give the current owner or undefined. 
current_owner_for_name_short(Name, State) -> % CurrentOwner | undefined
    case current_owner_for_name(Name, State) of
        {ok, CurrentOwner} -> CurrentOwner;
        Other -> Other
    end.

is_queue_empty(Name, State) -> % true | false
    case queue_for_name(Name, State) of
        {ok, Queue} -> 
            length(Queue) =:= 0;
        _ -> true
    end.
    
% Queue = list() of #req
queue_for_name(Name, State) -> % {ok, Queue} | undefined
    #srv_state{reqQs=Q0} = State,
    case dict:find(Name, Q0) of
        {ok, Queue} -> {ok, Queue};
        error       -> undefined
    end.

queue_for_name_short(Name, State) -> % Queue | undefined
    case queue_for_name(Name, State) of
        {ok, Queue} -> Queue;
        Other -> Other
    end.


%%--------------------------------------------------------------------
%% Function: handle_join(JoiningPid, Pidlist, State) -> {ok, State} 
%%     JoiningPid = pid(),
%%     Pidlist = list() of pids()
%% Description: Called whenever a node joins the cluster via this node
%% directly. JoiningPid is the node that joined. Note that JoiningPid may
%% join more than once. Pidlist contains all known pids. Pidlist includes
%% JoiningPid.
%%--------------------------------------------------------------------
handle_join(_JoiningPid, _Pidlist, State) ->
    % io:format(user, "~p:~p handle join called: ~p Pidlist: ~p~n", [?MODULE, ?LINE, JoiningPid, Pidlist]),
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_node_joined(JoiningPid, Pidlist, State) -> {ok, State} 
%%     JoiningPid = pid(),
%%     Pidlist = list() of pids()
%% Description: Called whenever a node joins the cluster via another node and
%%     the joining node is simply announcing its presence.
%%--------------------------------------------------------------------

handle_node_joined(_JoiningPid, _Pidlist, State) ->
    % io:format(user, "~p:~p handle node_joined called: ~p Pidlist: ~p~n", [?MODULE, ?LINE, JoiningPid, Pidlist]),
    {ok, State}.

handle_leave(_LeavingPid, _Pidlist, _Info, State) ->
    % io:format(user, "~p:~p handle leave called: ~p, Info: ~p Pidlist: ~p~n", [?MODULE, ?LINE, LeavingPid, Info, Pidlist]),
    {ok, State}.


delete_request(Req, State) -> % {ok, CurrentOwner, NewState}
    ?TRACE("deleting req", Req),
    case is_request_the_current_owner_exactly(Req, State) of
        true -> 
            ?TRACE("request is current owner", Req),
            case is_queue_empty(Req#req.name, State) of 
                false ->
                    ?TRACE("q not empty", val),
                    {ok, NewState} = promote_request_in_queue(Req#req.name, State),
                    CurrentOwner = current_owner_for_name_short(Req#req.name, NewState),
                    % if we cast back saying they are the current owner, that
                    % technically isn't true at this point, bc NewState hasn't
                    % been delivered up the chain. However, gen_server won't
                    % handle another call or cast until NewState has taken
                    % hold, so this is acceptable.
                    gen_cluster:cast(CurrentOwner#req.owner, {mutex, response, CurrentOwner}),
                    {ok, CurrentOwner, NewState};
                true ->
                    ?TRACE("q empty", val),
                    EmptyReq = empty_request_named(Req#req.name),
                    {ok, NewState} = set_current_owner(EmptyReq, State),
                    {ok, EmptyReq, NewState}
            end;
        false -> 
            CurrentOwner = current_owner_for_name_short(Req#req.name, State),
            case is_this_request_in_the_queue(Req, State) of
                true -> 
                    {ok, NewState} = remove_request_from_queue(Req, State),
                    {ok, CurrentOwner, NewState};
                false -> 
                    {ok, CurrentOwner, State}
            end
    end.

promote_request_in_queue(QName, State) -> % {ok, NewState}
    {ok, NewState} = case pop_queue(QName, State) of
        {ok, undefined, State} -> {ok, State};
        {ok, Request, State2} ->
            {ok, State3} = set_current_owner(Request, State2),
            {ok, State3}
    end,
    {ok, NewState}.

pop_queue(QName, State) -> % {ok, Request, NewState} |
                           % {ok, undefined, State} 
    ReqQs0 = State#srv_state.reqQs,
    Q = queue_for_name(QName, State),
    case Q of
        {ok, Queue} -> 
           [Head|Q1] = Queue,
           ReqQs1 = dict:store(QName, Q1, ReqQs0),
           NewState = State#srv_state{reqQs=ReqQs1},
           {ok, Head, NewState};
        _ ->          
           {ok, undefined, State} % hmm, silently ignores non-existent request, okay for now
   end.

remove_request_from_queue(Req, State) -> % {ok, RemovedReq, NewState}
    ReqQs = State#srv_state.reqQs,
    Q = queue_for_name(Req#req.name, State),
    case Q of
        {ok, Queue} -> 
           Q1 = lists:delete(Req, Queue),
           ReqQs1 = dict:store(Req#req.name, Q1, ReqQs),
           NewState = State#srv_state{reqQs=ReqQs1},
           {ok, Req, NewState};
        _ ->          
           {ok, Req, State} % hmm, silently ignores non-existent request, okay for now
   end.

set_current_owner(Req, State) -> % {ok, NewState}
    Owners0 = State#srv_state.owners,
    Owners1 = dict:store(Req#req.name, Req, Owners0), 
    NewState = State#srv_state{owners=Owners1},
    {ok, NewState}.

append_request_to_queue(Req, State) -> % {ok, NewState}
    Q0  = State#srv_state.reqQs,
    Q1  = dict:append(Req#req.name, Req, Q0),
    NewState = State#srv_state{reqQs=Q1},
    {ok, NewState}.

% is_req_owner_in_request_queue(Req, State) ->
%     ReqO = Req#req.owner,
%     case queue_for_name(Req#req.name, State) of
%         {ok, Queue} -> % Queue is full of #req records
%             lists:any(fun(Elem) ->
%                         Elem#req.owner =:= ReqO
%                 end, Queue)
%         _ -> false
%     end.

empty_request() ->
    #req{name=undefined, owner=undefined, timestamp=undefined}.

empty_request_named(Name) ->
    #req{name=Name, owner=undefined, timestamp=undefined}.
