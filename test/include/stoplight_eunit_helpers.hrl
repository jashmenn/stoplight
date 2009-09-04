-define(stop_and_unregister_servers(Servers),
    ((fun () ->
        lists:map(fun(Pname) -> 
            % Pid = whereis(Pname),
            Pid = case is_atom(Pname) of
                true -> whereis(Pname);
                false -> Pname
            end,
            gen_server:cast(Pid, stop), 
            try unregister(Pname)
            catch _:_ -> ok
            end,
            exit(Pid, kill)
        end, Servers)
      end)())).

-define(stop_and_unregister_globals,
    ((fun () ->
        lists:map(fun(Pname) -> 
                Pid = global:whereis_name(Pname),
                gen_server:cast(Pid, stop), 
                global:unregister_name(Pname),
                exit(Pid, kill)
            end, global:registered_names())
      end)())).

-define(sleep(T),
    ((fun () ->
        receive 
        after T -> 
                true 
        end 
      end)())).
