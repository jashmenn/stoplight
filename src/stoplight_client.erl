%% 
%% Stoplight client
%% 
-module(stoplight_client).
-include_lib("../include/defines.hrl").

-export([start/0]).

start()     -> 
    {ok, #client_state{ pid=self() }}.

