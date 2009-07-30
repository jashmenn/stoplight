%% 
%% Stoplight client
%% 
-module(stoplight_client).
-include_lib("../include/defines.hrl").

-export([start/0]).

start()     -> 
    {ok, ServerNames} = parse_args(),
    {ok, ServerRefs}  = connect_to_servers(ServerNames),

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
    ServerHostName = lists:last(ServerHostnameList),

    % TODO get these as variables too
    ServerPidName = "stoplight_srv",
    ServerPort    = 8649,

    % add all the servers when we support referencing more
    ServerDef = #noderef{pidname=ServerPidName, hostname=ServerHostName, port=ServerPort,pidref=list_to_atom(ServerPidName ++ "@" ++ ServerHostName)},
    {ok, [ServerDef]}.

connect_to_servers(ServerNames) ->
   ServerRefs = lists:map(fun(Server) ->
      ?TRACE("connecting to server: ", Server),
      #noderef{pidref=PidRef} = Server,
      pong = net_adm:ping(PidRef)
    end,
    ServerNames),
   {ok, ServerRefs}.

