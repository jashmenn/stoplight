%% Stoplight client
-module(stoplight_client_app).
-behaviour(application).
-include_lib("../include/defines.hrl").

-export([start/2, stop/1]).

start(_, [])     -> 
    % Servers = stoplight_misc:get_existing_servers(stoplight_client),
    % {ok, ServerRefs}  = stoplight_misc:connect_to_servers(Servers),
    % {ok, self(), #client_state{ pid=self() }}.
    ok.

stop(_State) ->
    ok.

% private
