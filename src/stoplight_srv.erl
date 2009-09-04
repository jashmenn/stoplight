%%%-------------------------------------------------------------------
%%% File    : stoplight_srv.erl
%%% Author  : nmurray
%%% Description : desc
%%% Created     : 2009-07-30
%%% TODO 
%%% * CHECK send owner periodically
%%%-------------------------------------------------------------------
-module(stoplight_srv).
-behaviour(gen_cluster).
-include_lib("../include/defines.hrl").

-export([start_link/2, start_named/2]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

% gen_cluster callback
-export([handle_join/3, handle_node_joined/3, handle_leave/4, seed_nodes/1]).

% debug
-compile(export_all).

%% Macros
-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
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
    % process_flag(trap_exit, true),
    InitialState = #srv_state{
                      pid=self(),
                      nodename=node(),
                      reqQs=dict:new(),
                      owners=dict:new(),
                      monitors=[]
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
    {ok, State1} = add_monitor_if_needed(Req#req.owner, State),
    case is_request_stale(Req, State1) of
        true -> 
            % CurrentOwner = current_owner_for_name_short(Req#req.name, State),
            % {reply, {stale, CurrentOwner}, State}; % it is stale, don't give the mutex
            {noreply, State1}; % it is stale, don't give the mutex
           _ -> handle_non_stale_mutex_cast({mutex, Tag, Req}, State1)
    end;
handle_cast(_Msg, State) -> 
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, State) ->
    % ?TRACE("received 'DOWN'. Removing node's requests. Info:", Info),
    {ok, NewState} = delete_requests_from_pid_for_all_requests(Pid, State),
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
%% gen_cluster req'd callbacks
%%--------------------------------------------------------------------
handle_join(_JoiningPid, _Pidlist, State) -> {ok, State}.
handle_node_joined(_JoiningPid, _Pidlist, State) -> {ok, State}.
handle_leave(_LeavingPid, _Pidlist, _Info, State) -> {ok, State}.

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
        {true, _CurrentOwner} ->
            {noreply, State};
        false ->
            case current_owner_for_name(Req#req.name, State) of
              {ok, CurrentOwner} -> 
                  % gen_cluster:cast(Req#req.owner, {mutex, response, CurrentOwner}),
                  send_response(Req#req.owner, CurrentOwner),
                  {noreply, State};
              undefined          -> {noreply, State}
            end
    end.

handle_mutex_yield(Req, State) ->
    ?TRACE("yielding request", Req),
    case is_request_the_current_owner_exactly(Req, State) of
        true ->
            % this yield handling is wrong,
            % 1) dont append yield to the back of the queue, the yielded should be the next to last
            % 2) poping the queue shouldn't sort by timestamp, sorts should be done on insert. 

            % second to last
            % {ok, State1} = promote_request_in_queue(Req#req.name, State),
            % CurrentOwner = current_owner_for_name_short(Req#req.name, State1),
            % {ok, State2} = append_request_to_queue(Req, State1),

            % appending this to the queue, really all that is doing is moving it back to the front of the queue (not nec, read below)
            % hrm... why? b/c its being sorted. so the same reqeust is moved to the front each time. 
            % so someone yields their request, but it ends up being the same request as the current owner anyhow (nope, read more)
            %
            % this is right actually! b/c if the request yielded is NOT the
            % earliest request then things will be ordered properly
            {ok, State1} = append_request_to_queue(Req, State),
            {ok, State2} = promote_request_in_queue(Req#req.name, State1),
            CurrentOwner = current_owner_for_name_short(Req#req.name, State2),

            send_response(CurrentOwner#req.owner, CurrentOwner),
            case CurrentOwner#req.owner =:= Req#req.owner of
                true -> ok;
                false -> 
                    send_response(Req#req.owner, CurrentOwner)
            end,
            {noreply, State2};
        false ->
            {noreply, State}
    end.

handle_mutex_request_from_not_owner(Req, State) ->
    {ok, CurrentOwnerReq, NewState} = case current_owner_for_name(Req#req.name, State) of
        undefined -> 
            {ok, NewState1} = set_current_owner(Req, State), 
            OwnerReq = current_owner_for_name_short(Req#req.name, NewState1),
            {ok, OwnerReq, NewState1}; 
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
                    {ok, CurrentOwner, NewState1}
            end
    end,
    ReqPid = Req#req.owner,
    send_response(ReqPid, CurrentOwnerReq), 
    {noreply, NewState}.

handle_mutex_release(Req, State) ->
    {ok, _CurrentOwner, NewState} = delete_request(Req, State),
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
             case length(ReqsForOwnerSorted) > 0 of 
                 true -> {true, lists:nth(1, ReqsForOwnerSorted)};
                 false -> false
             end;
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


delete_request(Req, State) -> % {ok, CurrentOwner, NewState}
    ?TRACE("deleting request", Req),
    case is_request_the_current_owner_exactly(Req, State) of
        true -> 
            case is_queue_empty(Req#req.name, State) of 
                false ->
                    {ok, NewState} = promote_request_in_queue(Req#req.name, State),
                    CurrentOwner = current_owner_for_name_short(Req#req.name, NewState),
                    % ?TRACE("promoted CurrentOwner", [CurrentOwner, to, CurrentOwner#req.owner]),
                    % if we cast back saying they are the current owner, that
                    % technically isn't true at this point, bc NewState hasn't
                    % been delivered up the chain. However, gen_server won't
                    % handle another call or cast until NewState has taken
                    % hold, so this is acceptable.
                    % gen_cluster:cast(CurrentOwner#req.owner, {mutex, response, CurrentOwner}),
                    send_response(CurrentOwner#req.owner, CurrentOwner),
                    {ok, CurrentOwner, NewState};
                true ->
                    EmptyReq = empty_request_named(Req#req.name),
                    {ok, NewState} = set_current_owner(EmptyReq, State),
                    {ok, EmptyReq, NewState}
            end;
        false -> 
            CurrentOwner = current_owner_for_name_short(Req#req.name, State),
            case is_this_request_in_the_queue(Req, State) of
                true -> 
                    {ok, _RmdReq, NewState} = remove_request_from_queue(Req, State),
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
    % just trace that this happened
    CurrentOwner = current_owner_for_name_short(QName, NewState),
    ?TRACE("promoted CurrentOwner", [CurrentOwner, to, CurrentOwner#req.owner]),
    {ok, NewState}.

pop_queue(QName, State) -> % {ok, Request, NewState} |
                           % {ok, undefined, State} 
    ReqQs0 = State#srv_state.reqQs,
    Q = queue_for_name(QName, State),
    case Q of
        {ok, Queue} -> 
           % Queue0 = stoplight_request:sort_by_timestamp(Queue),
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
    % add the request to the queue for that name
    D0  = State#srv_state.reqQs,
    D1  = dict:append(Req#req.name, Req, D0),
    NewState = State#srv_state{reqQs=D1},
    {ok, NewState1} = sort_queue_for_name(Req#req.name, NewState),
    {ok, NewState1}.
    % {ok, NewState}.

sort_queue_for_name(Name, State) -> % {ok, NewState}
    D0 = State#srv_state.reqQs,
    Q0 = queue_for_name_short(Name, State),
    Q1 = stoplight_request:sort_by_timestamp(Q0),
    % Q1 = Q0,
    D1 = dict:store(Name, Q1, D0), 
    NewState = State#srv_state{reqQs=D1},
    {ok, NewState}.

% prepend_request_

empty_request() ->
    #req{name=undefined, owner=undefined, timestamp=undefined}.

empty_request_named(Name) ->
    #req{name=Name, owner=undefined, timestamp=undefined}.

send_response(Pid, CurrentOwner) ->
    % ?TRACE("casting response", [to,Pid,response,CurrentOwner]),
    gen_server:cast(Pid, {mutex, response, CurrentOwner, self()}).

% for now, just to a brute search. a refactoring would keep a dict to lookup
% pid -> name so we dont have to search through all queues and current owners.
delete_requests_from_pid_for_all_requests(Pid, State) -> % {ok, NewState}
    Reqs = find_all_reqs_from_pid(Pid, State),
    {ok, NewState} = delete_requests(Reqs, State),
    {ok, NewState}.

delete_requests([Req|Others], State) ->
    {ok, _CurrentOwner, NewState} = delete_request(Req, State),
    delete_requests(Others, NewState);
delete_requests([], State) ->
    {ok, State}.

find_all_reqs_from_pid(Pid, State) -> % list() of #reqs
    {ok, Owners} = find_all_current_owners_from_pid(Pid, State),
    {ok, Qs    } = find_all_requests_in_queues_from_pid(Pid, State),
    lists:append([Owners, Qs]).

find_all_current_owners_from_pid(Pid, State) ->
    find_all_current_owners_from_pid(dict:fetch_keys(State#srv_state.owners), Pid, [], State).

find_all_current_owners_from_pid([Key|OtherKeys], Pid, Acc, State) ->
    CurrentOwner = current_owner_for_name_short(Key, State),
    NewAcc = case erlang:is_record(CurrentOwner, req) of 
        true ->
            case CurrentOwner#req.owner =:= Pid of
                true -> [CurrentOwner|Acc];
                false -> Acc
            end;
        false -> Acc
    end,
    find_all_current_owners_from_pid(OtherKeys, Pid, NewAcc, State);
find_all_current_owners_from_pid([], _Pid, Acc, _State) -> % {ok, Acc}
    {ok, Acc}.

find_all_requests_in_queues_from_pid(Pid, State) ->
    find_all_requests_in_queues_from_pid(dict:fetch_keys(State#srv_state.reqQs), Pid, [], State).

find_all_requests_in_queues_from_pid([Key|OtherKeys], Pid, Acc, State) -> 
    Q = queue_for_name_short(Key, State),
    Reqs = requests_in_queue_for_pid(Pid, Q),
    NewAcc = lists:append(Acc, Reqs),
    find_all_requests_in_queues_from_pid(OtherKeys, Pid, NewAcc, State);
find_all_requests_in_queues_from_pid([], _Pid, Acc, _State) -> 
    {ok, Acc}.

delete_requests_from_pid_for_all_requests_old(Pid, State) -> % {ok, NewState}
    {ok, State1} = delete_requests_from_pid_for_all_current_owners(Pid, State),
    {ok, State2} = delete_requests_from_pid_for_all_reqQs(Pid, State1),
    {ok, State2}.

delete_requests_from_pid_for_all_current_owners(Pid, State) ->
    do_delete_requests_from_pid_for_all_current_owners(dict:fetch_keys(State#srv_state.owners), Pid, State).

do_delete_requests_from_pid_for_all_current_owners([Key|OtherKeys], Pid, State) ->
    CurrentOwner = current_owner_for_name_short(Key, State),
    NewState = case erlang:is_record(CurrentOwner, req) of 
        true ->
            case CurrentOwner#req.owner =:= Pid of
                true -> 
                    EmptyReq = empty_request_named(Key),
                    {ok, State1} = set_current_owner(EmptyReq, State),
                    State1;
                false ->
                    State
            end;
        false -> 
            State
    end,
    do_delete_requests_from_pid_for_all_current_owners(OtherKeys, Pid, NewState);

do_delete_requests_from_pid_for_all_current_owners([], _Pid, State) ->
    {ok, State}.

delete_requests_from_pid_for_all_reqQs(Pid, State) ->
    do_delete_requests_from_pid_for_all_reqQs(dict:fetch_keys(State#srv_state.reqQs), Pid, State).

do_delete_requests_from_pid_for_all_reqQs([Key|OtherKeys], Pid, State) ->
    Q = queue_for_name_short(Key, State),
    Reqs = requests_in_queue_for_pid(Pid, Q),
    {ok, NewState} = do_delete_requests_in_queue(Reqs, Key, State),
    do_delete_requests_from_pid_for_all_reqQs(OtherKeys, Pid, NewState);

do_delete_requests_from_pid_for_all_reqQs([], _Pid, State) ->
    {ok, State}.

do_delete_requests_in_queue([Req|Others], Name, State) ->
    {ok, _CurrentOwner, NewState} = delete_request(Req, State),
    do_delete_requests_in_queue(Others, Name, NewState);
do_delete_requests_in_queue([], _Name, State) ->
    {ok, State}.

requests_in_queue_for_pid(Pid, Q) ->
    lists:filter(fun(Elem) ->
                Elem#req.owner =:= Pid
        end, Q).

add_monitor_if_needed(Pid, State) ->
    M = State#srv_state.monitors,
    case lists:any(fun(Elem) -> Elem =:= Pid end, M) of 
        true -> {ok, State};
        false ->
            _MRef = erlang:monitor(process, Pid),
            NewState = State#srv_state{monitors=[Pid|M]}, % todo, you need to store the MRef if you ever want to get rid of the monitor
            {ok, NewState}
    end.

% remove_monitor(MRef, State) ->
%     ?TRACE("removing monitoring", MRef),
%     erlang:demonitor(MRef),
%     M0 = State#srv_state.monitors,
%     M1 = lists:delete(MRef, M0),
%     {ok, State#srv_state{monitors=M1}}.

seed_nodes(State) ->
    stoplight_misc:existing_servers_list().

