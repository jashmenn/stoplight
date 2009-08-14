%%%-------------------------------------------------------------------
%%% File    : stoplight_lobbyist.erl
%%% Author  : Nate Murray <nmurray@attinteractive>
%%% Created     : 2009-08-07
%%% Description : 
%%% 
%%%  Lobbyist petitions the servers on behalf of the clients. This allows us to
%%%  have multiple lobbyists working asynchronously with the servers but the
%%%  client can work with the user synchronously
%%%
%%%  Each lobbyist is dedicated to getting one (and only one) specific request.
%%%
%%%  TODO - needs to be linked to the servers for the duration of the
%%%  interaction
%%%-------------------------------------------------------------------

%% Stoplight lobbyist
-module(stoplight_lobbyist).
-include_lib("../include/defines.hrl").
-behaviour(gen_server).

-export([start_named/2]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

% debug
-compile(export_all).

%% Macros
-record(state, {name, pid, client, servers, responses, request, pendingInquiries, numInquiryRounds}).

-define(tupleSearchVal(Key, TupleList), 
    ((fun (K, TL) ->
        {value, {K, Value}} = lists:keysearch(K, 1, TL),
        Value
      end)(Key, TupleList))
).

-define(STOPLIGHT_SRV_LOCAL, stoplight_srv_local).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start(Config) ->
   gen_server:start(?MODULE, Config, _GenServerOpts=[]).

%% for testing multiple servers
start_named(Name, Config) ->
    gen_server:start({local, Name}, ?MODULE, Config, [{debug, [{log_to_file, "/dev/null"}]}]).

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

init(Args) -> 
    Lockname  = ?tupleSearchVal(name, Args),
    Client    = ?tupleSearchVal(client, Args),
    Servers   = get_servers(Args),

    Responses = servers_dict_init(Servers),
    PendingInquiries = servers_dict_init(Servers),
    Request   = #req{name=Lockname, owner=self(), timestamp=stoplight_util:unix_seconds_since_epoch()},

    InitialState = #state{
                      pid=self(),
                      name=Lockname, 
                      client=Client,
                      request=Request,
                      responses=Responses,
                      pendingInquiries=PendingInquiries,
                      numInquiryRounds=0
                   },
    ?TRACE("starting new lobbyist", [self, self(), client, Client, name, Lockname]),
    link(Client), % monitor the client
    ?enable_tracing,
    {ok, InitialState}.

%%--------------------------------------------------------------------
%% Function: handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(state, _From, State) ->
    {reply, {ok, State}, State};
handle_call(request, _From, State) ->
    {reply, {ok, State#state.request}, State};
handle_call(responses, _From, State) ->
    {reply, {ok, State#state.responses}, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(petition, _From, State) ->
    handle_petition(State),
    {reply, ok, State};
handle_call(release, _From, State) ->
    handle_release(State),
    {stop, normal, ok, State};
handle_call(_Request, _From, State) -> 
    {reply, okay, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({mutex, response, CurrentOwner, From}, State) ->
    {ok, NewState} = handle_cast_mutex_response(CurrentOwner, From, State),
    {noreply, NewState};

handle_cast({mutex, check, CurrentOwner, From}, State) ->
    {ok, State} = handle_mutex_check(CurrentOwner, From, State),
    {noreply, State};

handle_cast(_Msg, State) -> 
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(Info, State) -> 
    ?TRACE("lobbyist got info", [self(), Info]),
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
%% Func: handle_petition(State) -> ok
%% Description: sends request to all known servers
%%--------------------------------------------------------------------
handle_petition(State) ->
    multicast_servers({mutex, request, State#state.request}, State),
    ok.

handle_release(State) ->
    ?TRACE("sending release to", [from, self(), to, servers(State)]),
    multicast_servers({mutex, release, State#state.request}, State),
    ok.

% todo - test what happens when we already have crit and we get another supporting response
handle_cast_mutex_response(CurrentOwner, From, State) -> % {ok, NewState}
    % ?TRACE("lobbyist rec'd response ", [CurrentOwner, From, State]),
    {ok, State1} = update_responses_if_needed(CurrentOwner, From, State),
    % ?TRACE("updated resp from ", [CurrentOwner, From, State1]),

    {Resp, State2} = try_for_lock(CurrentOwner, From, State1), 
    % ?TRACE("try for lock", [Resp, State2]),

    Client = State2#state.client,
    % ?TRACE("client is", Client),
    % ?TRACE("pid?", is_pid(Client)),

    case Resp of
        crit ->
            % TODO - this Crit is sent once for m + every server >m . is that bad?
            Client ! {crit, State#state.request, self()},
            ok;
        no -> 
            % Client ! {no, State#state.request, self()},
            ok
    end,
    {ok, State2}.
    
% The server periodically sends a check for the current request it supports. If
% somehow our RELEASE message wasn't delivered and we've moved on to a new
% timestamp, then we need to send a RELEASE to that server.
% 
% Here's the thing, in our setup we only use the lobbyist for one request. The
% timestamp is set only once, and the process is shutdown after that request is
% handled. That means this process shouldn't ever get conflicting timestamps in
% our setup.
%
% This check is left in here for algorithm completeness. It is worth ensuring
% that this does not break something fundamental in the algorithm.

handle_mutex_check(CurrentOwner, From, State) -> % {ok, NewState}
    Request = State#state.request,
    case CurrentOwner#req.timestamp =/= Request#req.timestamp of
        true  -> gen_cluster:cast(From, {mutex, release, Request});
        false -> ok
    end,
    {ok, State}.

% return a dict where the keys are server pids and values are `undef`
servers_dict_init(Servers) -> 
    servers_dict_init(Servers, dict:new()).
servers_dict_init([H|T], D0) ->
    servers_dict_init(T, dict:store(H, undef, D0));
servers_dict_init([], D0) -> D0.

multicast_servers(Msg, State) ->
    [ gen_cluster:cast(Server, Msg) || Server <- servers(State) ].

servers(State) ->
    dict:fetch_keys(State#state.responses).

update_responses_if_needed(CurrentOwner, From, State) -> % {ok, NewState}
    {ok, State2} = case have_different_response_from_this_server(From, CurrentOwner, State) andalso
                (response_owner_is_not_us(CurrentOwner, State) orelse 
                 response_timestamp_matches_ours(CurrentOwner, State)) of
        true  -> add_server_response(CurrentOwner, From, State);
        false -> {ok, State}
        end,
    {ok, State2}.

try_for_lock(CurrentOwner, From, State) -> % {crit, NewState} | {no, NewState}
    {Resp, NewState} = case enough_responses_received(CurrentOwner, State) of
        true -> 
            case enough_servers_support_our_request(State) of
                true -> {crit, State} ; % you get the lock! Congratulations!
                false -> lobby_for_more_support(State)
            end;
        false -> {no, State}
        end,
    {Resp, NewState}.

lobby_for_more_support(State) -> % {no, NewState}
    R0 = State#state.responses,
    {ok, NewState} = do_lobby_for_more_support(dict:fetch_keys(R0), State),
    {no, NewState}.

do_lobby_for_more_support([ServerPid|Rest], State) -> % {ok, NewState}
    Request = State#state.request,
    R0 = State#state.responses,
    Response = dict:fetch(ServerPid, R0),  
    {ok, State1} = case have_response_from_server(ServerPid, State) of
        true ->
            RequestLt = request_lt(Request, Response),
            if
                Response#req.owner =:= Request#req.owner -> gen_cluster:cast(ServerPid, {mutex, yield, Request}), {ok, State};
                RequestLt                                -> gen_cluster:cast(ServerPid, {mutex, request, Request}), {ok, State};
                true                                     -> send_inquiry_if_needed(ServerPid, State)
            end;
        false ->
            {ok, State} % do nothing
    end,

    R1 = dict:store(ServerPid, undef, State1#state.responses), % clear out response for this server
    NewState = State1#state{responses=R1},
    do_lobby_for_more_support(Rest, NewState);

do_lobby_for_more_support([], State) -> % {ok, NewState}
    {ok, State}.

send_inquiry_if_needed(ServerPid, State) -> % {ok, NewState}
    Request = State#state.request,

    {ok, NewState} = case have_pending_inquiry_for(ServerPid, State) of
        true -> 
            {ok, State};
        false ->
            Timeout = inquiry_delay_time(State),
            % ?TRACE("Timeout is", [self(), Timeout, State#state.numInquiryRounds]),
            Pid = spawn(
                fun() ->
                  ?enable_tracing,
                  receive stop -> ok
                  after Timeout ->
                      gen_cluster:cast(ServerPid, {mutex, inquiry, Request})
                  end
                end),
            P2 = dict:store(ServerPid, Pid, State#state.pendingInquiries),
            Ntry = State#state.numInquiryRounds,
            {ok, State#state{pendingInquiries=P2, numInquiryRounds=(Ntry+1)}}
    end,
     
    {ok, NewState}.

have_different_response_from_this_server(From, CurrentOwner, State) -> % bool()
    Responses = State#state.responses,
    case dict:find(From, Responses) of
        {ok, Response} -> Response =/= CurrentOwner;
        error -> false
    end.

have_response_from_server(ServerPid, State) -> % bool()
    Responses = State#state.responses,
    case dict:find(ServerPid, Responses) of
        {ok, Response} -> 
            case Response of
                undef -> false;
                _ -> true
            end;
        error -> false
    end.

response_owner_is_not_us(CurrentOwner, State) -> % bool()
    Request = State#state.request,
    CurrentOwner#req.owner =/= Request#req.owner.

response_timestamp_matches_ours(CurrentOwner, State) -> % bool()
    Request = State#state.request,
    CurrentOwner#req.timestamp =:= Request#req.timestamp.

add_server_response(CurrentOwner, From, State) -> % {ok, NewState}
    R0 = State#state.responses,
    R1 = dict:store(From, CurrentOwner, R0),
    NewState = State#state{responses=R1},
    {ok, NewState}.

enough_responses_received(_CurrentOwner, State) -> % bool()
    number_of_responses(State) >= quorum_threshold(State).

quorum_threshold(State) -> % int()
    Responses = State#state.responses,
    Servers = dict:fetch_keys(Responses),
    NumServers = length(Servers),
    stoplight_util:ceiling( ( 2 * NumServers ) / 3 ).

% returns int() count of the number of defined responses 
number_of_responses(State) -> % int()
    Responses = State#state.responses,
    dict:fold( 
        fun(_ServerPid, Value, AccIn) -> 
            case Value =:= undef of
                true -> AccIn;
                false -> AccIn + 1
            end
        end,
        0, 
        Responses).

enough_servers_support_our_request(State) -> % bool()
    number_of_servers_that_support_our_request(State) >= quorum_threshold(State).

number_of_servers_that_support_our_request(State) -> % int()
    Request = State#state.request,
    Responses = State#state.responses,
    dict:fold( 
        fun(_ServerPid, Response, AccIn) -> 
            case Response =:= Request of
                true -> AccIn + 1;
                false -> AccIn
            end
        end,
        0, 
        Responses).

get_servers(Args) -> % []
  case lists:keymember(servers, 1, Args) of
    true  -> ?tupleSearchVal(servers, Args);
    false -> 
      {ok, Plist} = gen_cluster:plist(?STOPLIGHT_SRV_LOCAL),
      Plist
  end.

% TODO - need a tiebreaker based on client ID
request_lt(Request, OtherRequest) -> % bool()
     Request#req.timestamp < OtherRequest#req.timestamp.

have_pending_inquiry_for(ServerPid, State) -> % bool()
    case dict:find(ServerPid, State#state.pendingInquiries) of
        {ok, Response} -> 
            case Response of
                undef -> false;
                InqPid -> 
                    case is_process_alive(InqPid) of 
                        true -> true;
                        false -> false
                    end
            end;
        error -> false
    end.

inquiry_delay_time(State) ->
    Ntry = State#state.numInquiryRounds,
    Max  = 3000, % 3 seconds 
    case stoplight_util:floor(stoplight_util:random_exponential_delay(500, Ntry, Max)) of
       1 -> 0;
       Other -> Other
    end.
