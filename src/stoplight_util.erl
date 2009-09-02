%% Stoplight lobbyist
-module(stoplight_util).
-include_lib("../include/defines.hrl").

-compile(export_all).

% http://schemewiki.org/Erlang/NumberRounding
ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;
        Pos when Pos > 0 -> T;
        _ -> T
    end.

% should be mv'd to utils
% use unix epoch rather than erlang
unix_seconds_since_epoch() ->
    LocalDateTime = calendar:datetime_to_gregorian_seconds({date(),time()}),
    UnixEpoch = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    LocalDateTime - UnixEpoch.

% delay = MIN( Random * InitialTimeout * 2 ^ Ntry , Max )
random_exponential_delay(InitialTimeout, Ntry, Max) -> 
    R = random:uniform() + 1,
    F = 2,
    Try = lists:min([Ntry, 1000]),
    Base = floor(R * InitialTimeout * F),
    try math:pow(Base, Try) of
        Calculated -> lists:min([Calculated, Max])
    catch
        _:_ -> Max
    end.

random_element(List) ->
    I = crypto:rand_uniform(1, length(List)),
    lists:nth(I, List).

f() ->
   receive 
      From when pid(From) -> 
         Now = erlang:now(),
         From ! {self(),Now}
   end. 


%%% --------Internal functions--------
%%% ----------------------------------
%%% Format handler
print(_Out,end_of_trace,_TI,N) ->
    N;
print(Out,Trace,_TI,N) ->
    do_print(Out,Trace,N),
    N+1.

do_print(Out,{trace_ts,P,call,{M,F,A},Ts},N) ->
    io:format(Out,
              "~w: ~w, ~w:~n"
              "Call      : ~w:~w/~w~n"
              "Arguments :~p~n~n",
              [N,Ts,P,M,F,length(A),A]);
do_print(Out,{trace_ts,P,return_from,{M,F,A},R,Ts},N) ->
    io:format(Out,
              "~w: ~w, ~w:~n"
              "Return from  : ~w:~w/~w~n"
              "Return value :~p~n~n",
              [N,Ts,P,M,F,A,R]);
      
% * every crit sent to client from lobbyist
% * every INQUIRY rec'd by stoplight_srv     
% * every RESPOSE rec'd by stoplight_lobbyist 

do_print(Out,{trace,P,send,
        {crit,Req,APid},
        P2}, N) ->
    io:format(Out, "crit ~p~n", [{crit, Req, APid}]);

do_print(Out,{trace,P,send,
        {'$gen_cast',{mutex,inquiry,Req}},
        P2}, N) ->
    io:format(Out, "INQUIRY ~p~n", [{'$gen_cast',{mutex,inquiry,Req}}]);

do_print(Out,{trace,P,send,
        {'$gen_cast',{mutex,response,Req,AnotherPid}}, 
        P2}, N) ->
    io:format(Out, "RESPONSE ~p~n", [{'$gen_cast',{mutex,response,Req,AnotherPid}}]);

do_print(Out,{trace,P,send,Message,Info}, N) ->
    ok;

do_print(Out,Ignored,N) ->
    ok.
