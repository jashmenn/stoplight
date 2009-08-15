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
    Calculated = math:pow(Base, Try),
    lists:min([Calculated, Max]).

random_element(List) ->
    I = crypto:rand_uniform(1, length(List)),
    lists:nth(I, List).
