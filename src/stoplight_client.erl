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
lock(Name, Timeout) when is_integer(Timeout) ->
    lock(Name, find_listener(), Timeout).
lock(Name, Listener, Timeout) ->
    {ok, LobPid} = gen_server:call(Listener, {try_mutex, Name, Timeout}),
    receive
       {crit, Request, LobbyPid} -> 
           {crit, LobbyPid}
    after Timeout -> 
       {no, LobPid}
    end.

release(Lobbyist) ->
    ok = gen_server:call(Lobbyist, release).

find_listener() ->
    case whereis(?STOPLIGHT_LISTENER) of
        undefined ->
            ServerNode = list_to_atom("stoplight@" ++ net_adm:localhost()),
            net_adm:ping(ServerNode),
            ListenerPid = rpc:call(ServerNode, ?STOPLIGHT_LISTENER, pid,  []),
            ListenerPid;
        Pid -> Pid
    end.
