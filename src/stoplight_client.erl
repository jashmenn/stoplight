%% 
%% Stoplight client
%% 
-module(stoplight_client).
-include_lib("../include/defines.hrl").

-export([start/0]).

start()     -> 
    {ok, ServerRefs} = parse_args(),
    ?TRACE("serverpids", ServerRefs),
    {ok, #client_state{ pid=self() }}.

% private

% todo, cleanup how these arguments are parsed
parse_args() ->
    % {value, {stoplight, StoplightArgs}} = lists:keysearch(stoplight, 1, FullCommand),
    % {ServerArgs} = lists:keysearch("stoplight_servers", 1, StoplightArgs),
    % Map = lists:map(fun({A,B}) -> {A,B} end, StoplightArgs),
    % ?TRACE("serverpid", ServerArgs),

    {ok, StoplightArgs} = init:get_argument(stoplight),
    [ServerHostnameList | _Rest] = StoplightArgs, % just one server
    ServerHostname = lists:last(ServerHostnameList),

    % TODO get these as variables too
    ServerPidName = stoplight_srv,
    ServerPort    = 8649,

    % add all the servers when we support referencing more
    ServerDef = #noderef{pidname=ServerPidName, hostname=ServerHostname, port=ServerPort},
    {ok, [ServerDef]}.

