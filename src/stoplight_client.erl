%%%-------------------------------------------------------------------
%%% File        : stoplight_client.erl
%%% Author      : Nate Murray <nmurray@attinteractive>
%%% Description : desc
%%% Created     : 2009-08-07
%%%-------------------------------------------------------------------
-module(stoplight_client).
-include_lib("../include/defines.hrl").
-compile(export_all).

lock(Name) ->
    lock(Name, 5000).
lock(Name, Timeout) ->
    {ok, LobPid} = gen_server:call(?STOPLIGHT_LISTENER, {try_mutex, Name}),
    receive
       {crit, Request, LobbyPid} -> 
           {crit, LobbyPid}
    after Timeout -> 
       {no, LobPid}
    end.

release(Lobbyist) ->
    ok = gen_server:call(Lobbyist, release).
