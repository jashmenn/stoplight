%% 
-module(stoplight_misc).
-include_lib("../include/defines.hrl").

% debug
-compile(export_all).

connect_to_servers(ServerNames) ->
   ServerRefs = lists:map(fun(Server) ->

      case Server of
      #noderef{name=undefined} -> 
          skip; % do nothing
      _ -> 
         ?TRACE("connecting to server: ", Server),
         #noderef{name=Name} = Server,
         pong = net_adm:ping(Name)
      end
    end,
    ServerNames),
   {ok, ServerRefs}.
