-module(lock_tester).
-compile(export_all).

-include_lib("../../include/defines.hrl").
-define(LOCK_DIR, "/tmp/stoplight_locks").

try_for(Name, ListenerPool) ->
    file:make_dir(?LOCK_DIR),
    Filename = ?LOCK_DIR ++ "/" ++ atom_to_list(Name),
    case filelib:is_file(Filename) of 
        true ->  exit(list_to_atom("file_exists-" ++ atom_to_list(Name)));
        false -> 
            RandomListener = stoplight_util:random_element(ListenerPool),
            {Resp, Lobbyist} = stoplight_client:lock(Name, RandomListener, 5000),
            case Resp of
                crit ->
                    % ?TRACE("got crit", self()),
                    case file:write_file(Filename, []) of
                        ok -> 
                            timer:sleep(random:uniform(1000)),
                            % ?TRACE("releasing", Filename),
                            ok = file:delete(Filename),
                            ok = stoplight_client:release(Lobbyist);
                        {error, Reason} -> 
                            exit(Reason)
                    end;
                no -> 
                    % ?TRACE("got no", self()),
                    nope % didnt get it, oh well
            end
    end,
    ok.
