%% 
-module(stoplight_misc).
-include_lib("../include/defines.hrl").

% debug
-compile(export_all).

connect_to_servers(ServerNames) ->
   ServerRefs = lists:map(fun(Server) ->
      case Server of
      #noderef{name=undefined} -> 
          ?TRACE("warning, skipping server", Server),
          skip; % do nothing
      _ -> 
         ?TRACE("connecting to server: ", Server),
         #noderef{name=Name} = Server,
         pong = net_adm:ping(Name)
      end
    end,
    ServerNames),
   {ok, ServerRefs}.

existing_servers_list() ->
    Servers = [],
    Servers1 = Servers, % Servers1 = [list_to_atom("stoplight@" ++ net_adm:localhost())|Servers],

    Servers2 = case application:get_env(stoplight, servers) of
        {ok, EnvServers}  ->  lists:append([EnvServers, Servers1]);
        _ -> Servers1
    end,

    Servers3 = case os:getenv("STOPLIGHT_SERVER") of 
        false -> Servers2;
        Server -> [list_to_atom(Server)|Servers2]
    end,
    Servers3.
