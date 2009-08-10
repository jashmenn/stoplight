-module(eunit_stoplight_lobbyist).

-include_lib("eunit/include/eunit.hrl").
-include_lib("../../include/defines.hrl").

setup() ->
    {ok, Node1Pid} = stoplight_srv:start_named(srv1, {seed, undefined}),
    {ok, _} = stoplight_srv:start_named(srv2, {seed, Node1Pid}),
    {ok, _} = stoplight_srv:start_named(srv3, {seed, Node1Pid}),

    {ok, Servers} = gen_cluster:call(srv1, {'$gen_cluster', plist}),
    ?assertEqual(3, length(Servers)),

    {ok, _} = stoplight_lobbyist:start_named(lobbyist1, [{name, food}, {servers, Servers}]),
    [srv1, srv2, srv3, lobbyist1].

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
         {ok, State1} = gen_cluster:call(lobbyist1, {state}),
         % ?assert(is_record(State1, state) =:= true),
         % {ok, Plist} = gen_cluster:call(lobbyist, {'$gen_cluster', plist}),
         % ?assertEqual(3, length(Plist)),
         {ok}
      end
  }.


