%% Stoplight lobbyist
-module(stoplight_benchmarking_tracer).
-include_lib("../include/defines.hrl").
% ttb:format("trace", [{handler,{{stoplight_benchmarking_tracer,print},NumClients}}]).

-compile(export_all).
-record(state, {crit, inquiry, response, request, yield, release, clients}). % counts of each

%%% --------Internal functions--------
%%% ----------------------------------
%%% Format handler

print(Out,end_of_trace,_TI,State) ->
    % M = 4, % TODO - this should be dynamically the number of servers, just here for testing
    % ExtraResponses = State#state.crit * 
    % the number of extra reponses is only exactly the number of inquiries if messages are delivered perfectly. 
    io:format(user, "crit ~5B | INQUIRY ~5B | REQUEST ~5B | YIELD ~5B | RELEASE ~5B | RESPONSE ~5B ~n", 
        [State#state.crit, State#state.inquiry, 
            State#state.request,  State#state.yield, State#state.release, State#state.response]),
    State;
print(Out,Trace,TI,Clients) when is_integer(Clients) ->
    InitialState = #state{ crit=0, inquiry=0, response=0, request=0, yield=0, release=0, clients=Clients },
    print(Out,Trace,TI,InitialState);
print(Out,Trace,_TI,State) when is_record(State, state) ->
    NewState = do_print(Out,Trace,State),
    NewState.
      
% * every crit sent to client from lobbyist
% * every INQUIRY rec'd by stoplight_srv     
% * every RESPOSE rec'd by stoplight_lobbyist 

do_print(Out,{trace,P,send,
        {crit,Req,APid},
        P2}, State) ->
    NewState = State#state{crit=State#state.crit+1},
    NewState;

do_print(Out,{trace,P,send,
        {'$gen_cast',{mutex,inquiry,Req}},
        P2}, State) ->
    NewState = State#state{inquiry=State#state.inquiry+1},
    NewState;

do_print(Out,{trace,P,send,
        {'$gen_cast',{mutex,request,Req}},
        P2}, State) ->
    NewState = State#state{request=State#state.request+1},
    NewState;

do_print(Out,{trace,P,send,
        {'$gen_cast',{mutex,yield,Req}},
        P2}, State) ->
    NewState = State#state{yield=State#state.yield+1},
    NewState;

do_print(Out,{trace,P,send,
        {'$gen_cast',{mutex,release,Req}},
        P2}, State) ->
    NewState = State#state{release=State#state.release+1},
    NewState;

do_print(Out,{trace,P,send,
        {'$gen_cast',{mutex,response,Req,AnotherPid}}, 
        P2}, State) ->
    NewState = State#state{response=State#state.response+1},
    NewState;

do_print(Out,{trace,P,send,Message,Info}, State) ->
    State;

do_print(Out,Ignored,State) ->
    State.
