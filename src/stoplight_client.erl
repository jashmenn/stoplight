%% 
%% Stoplight client
%% 
-module(stoplight_client).
-behaviour(application).
-include_lib("../include/defines.hrl").

-export([start/2, stop/1]).

start(_, [])     -> 
    {ok, ServerNames} = parse_args(),
    {ok, ServerRefs}  = stoplight_misc:connect_to_servers(ServerNames),

    {ok, self(), #client_state{ pid=self() }}.


% start(_, []) ->
%     Handler = get_sasl_error_logger(),
%     Type = get_sasl_error_logger_type(),
%     Mf = get_error_logger_mf(),
%     add_sasl_error_logger(Handler, Type),
%     add_error_logger_mf(Mf),
%     State = #state{sasl_error_logger = Handler, error_logger_mf = Mf},
%     case supervisor:start_link({local, sasl_sup}, sasl, []) of
% ?   {ok, Pid} -> {ok, Pid, State};
% ?   Error -> Error
%     end.

stop(State) ->
    ok.

% private

% todo, cleanup how these arguments are parsed
parse_args() ->
    % {value, {stoplight, StoplightArgs}} = lists:keysearch(stoplight, 1, FullCommand),
    % {ServerArgs} = lists:keysearch("stoplight_servers", 1, StoplightArgs),
    % Map = lists:map(fun({A,B}) -> {A,B} end, StoplightArgs),
    % ?TRACE("serverpid", ServerArgs),

    % {ok, StoplightArgs} = init:get_argument(stoplight),
    {ok, StoplightArgs} = init:get_argument(stoplight),

    % StoplightArgs = application:get_env(stoplight, servers),
    % ?TRACE("args", StoplightArgs),
    % ?TRACE("app", application:get_application()),
    % ?TRACE("all env", application:get_all_env()),
    % {ok, []}.

    [ServerHostnameList | _Rest] = StoplightArgs, % just one server
    ServerHostName = lists:last(ServerHostnameList),

    % % TODO get these as variables too
    ServerPidName = "stoplight_srv",
    ServerPort    = 8649,

    % % add all the servers when we support referencing more
    % ServerDef = #noderef{pidname=ServerPidName, hostname=ServerHostName, port=ServerPort,pidref=list_to_atom(ServerPidName ++ "@" ++ ServerHostName)},
    ServerDef = #noderef{name=list_to_atom(ServerPidName ++ "@" ++ ServerHostName)},
    {ok, [ServerDef]}.


% get_sasl_error_logger() ->
%     case application:get_env(sasl, sasl_error_logger) of
%     {ok, false} -> undefined;
%     {ok, tty} -> tty;
%     {ok, {file, File}} when is_list(File) -> {file, File};
%     {ok, Bad} -> exit({bad_config, {sasl, {sasl_error_logger, Bad}}});
%     _ -> undefined
%     end.
