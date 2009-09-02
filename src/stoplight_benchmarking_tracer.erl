%% Stoplight lobbyist
-module(stoplight_benchmarking_tracer).
-include_lib("../include/defines.hrl").
% ttb:format("trace", [{handler,{{stoplight_benchmarking_tracer,print},NumClients}}]).

-compile(export_all).
-record(state, {crit, inquiry, response, clients}). % counts of each

%%% --------Internal functions--------
%%% ----------------------------------
%%% Format handler

print(Out,end_of_trace,_TI,State) ->
    io:format(user, "~nclients ~10B | crit ~10B | INQUIRY ~10B | RESPONSE ~10B ~n", [State#state.clients, State#state.crit, State#state.inquiry, State#state.response]),
    State;
print(Out,Trace,TI,Clients) when is_integer(Clients) ->
    InitialState = #state{ crit=0, inquiry=0, response=0, clients=Clients },
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
        {'$gen_cast',{mutex,response,Req,AnotherPid}}, 
        P2}, State) ->
    NewState = State#state{response=State#state.response+1},
    NewState;

do_print(Out,{trace,P,send,Message,Info}, State) ->
    State;

do_print(Out,Ignored,State) ->
    State.
