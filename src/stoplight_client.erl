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
       release(LobPid),
       {no, LobPid}
    end.

release(Lobbyist) ->
    ok = gen_server:call(Lobbyist, release).

find_listener() ->
    case whereis(?STOPLIGHT_LISTENER) of
        undefined ->
            ServerNode = find_server_node(),
            pong = net_adm:ping(ServerNode),
            ListenerPid = rpc:call(ServerNode, ?STOPLIGHT_LISTENER, pid,  []),
            ListenerPid;
        Pid -> Pid
    end.

find_server_node() ->
    find_server_node(servers_to_try()).
find_server_node([Server|Rest]) ->
    case net_adm:ping(Server) of
        pong -> Server;
        pang -> find_server_node(Rest)
    end;
find_server_node([]) -> no.

servers_to_try() ->
    Servers = [],
    Servers1 = case os:getenv("STOPLIGHT_SERVER") of 
        false -> Servers;
        Server -> [list_to_atom(Server)|Servers]
    end,
    Servers2 = [list_to_atom("stoplight@" ++ net_adm:localhost())|Servers1],
    Servers2.
