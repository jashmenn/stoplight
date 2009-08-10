-module(eunit_stoplight_client).

-include_lib("eunit/include/eunit.hrl").
-include_lib("../../include/defines.hrl").

setup() ->
    {ok, Node1Pid} = stoplight_client:start_named(client1, {seed, undefined}),
    [client1].

teardown(Servers) ->
    lists:map(fun(Pname) -> 
        Pid = whereis(Pname),
        gen_cluster:cast(Pid, stop), 
        unregister(Pname)
     end, Servers),

    lists:map(fun(Pname) -> 
        Pid = global:whereis_name(Pname),
        gen_cluster:cast(Pid, stop), 
        global:unregister_name(Pname)
    end, global:registered_names()),
    ok.

node_state_test_() ->
  {
      setup, fun setup/0, fun teardown/1,
      fun () ->
         {ok, State1} = gen_cluster:call(client1, {state}),
         % ?assert(is_record(State1, state) =:= true),
         % {ok, Plist} = gen_cluster:call(client1, {'$gen_cluster', plist}),
         % ?assertEqual(3, length(Plist)),
         {ok}
      end
  }.


