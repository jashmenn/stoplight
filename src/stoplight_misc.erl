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

get_existing_servers(Namespace) ->
    ?TRACE("env servers:", application:get_env(Namespace, servers)),
    case application:get_env(Namespace, servers) of
    {ok, Servers}  -> 
        [ #noderef{name=Name} || Name <- Servers];
    _ -> []
    end.


% get_indexed_pid(Namespace) ->


