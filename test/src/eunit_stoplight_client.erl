-module(eunit_stoplight_client).

-include_lib("eunit/include/eunit.hrl").
-include_lib("../../include/defines.hrl").
-include_lib("../include/stoplight_eunit_helpers.hrl").

setup() ->
    {ok, _Pid} = stoplight_listener:start_link([], []),
    [stoplight_listener].

teardown(Servers) ->
    ?stop_and_unregister_servers(Servers),
    ?stop_and_unregister_globals,
    ok.

node_state_test_() ->
  {
      setup, fun setup/0, fun teardown/1,
      fun () ->
         % {ok, State1} = gen_cluster:call(client1, {state}),
         % ?assert(is_record(State1, state) =:= true),
         % {ok, Plist} = gen_cluster:call(client1, {'$gen_cluster', plist}),
         % ?assertEqual(3, length(Plist)),
         {ok}
      end
  }.


