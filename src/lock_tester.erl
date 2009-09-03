-module(lock_tester).
-compile(export_all).

-include_lib("../include/defines.hrl").
-define(LOCK_DIR, "/tmp/stoplight_locks").

try_for(Name, ListenerPool) ->
    try_for(Name, ListenerPool, 5000, 1000).

try_for(Name, ListenerPool, Timeout, Sleeptime) ->
    file:make_dir(?LOCK_DIR),
    Filename = ?LOCK_DIR ++ "/" ++ atom_to_list(Name),
    RandomListener = stoplight_util:random_element(ListenerPool),
    % ?TRACE("trying for", [Name]),
    {Resp, Lobbyist} = stoplight_client:lock(Name, RandomListener, Timeout),
    % ?TRACE("tried for", [Name, lobbyist, Lobbyist, resp, Resp]),
    case Resp of
        crit ->
            % ?TRACE("got crit", self()),

            case filelib:is_file(Filename) of 
                true ->  exit(list_to_atom("file_exists-" ++ atom_to_list(Name)));
                false -> 
                    case file:write_file(Filename, []) of
                        ok -> 
                            % timer:sleep(random:uniform(1000)),
                            % ?TRACE("releasing", Filename),
                            timer:sleep(Sleeptime),
                            Deleted = file:delete(Filename),
                            % ?TRACE("Deleted", [Deleted, Filename]),
                            ok = Deleted,
                            ok = stoplight_client:release(Lobbyist);
                        {error, Reason} -> 
                            exit(Reason);
                        Other ->
                            exit(Other)

                    end
            end;
        no -> 
            % ?TRACE("got no", self()),
            nope % didnt get it, oh well
    end,
    ok.
