-module(eunit_stoplight_srv).

-include_lib("eunit/include/eunit.hrl").
-include_lib("../../include/defines.hrl").

setup() ->
    {ok, Node1Pid} = stoplight_srv:start_named(node1),
    application:set_env(stoplight, servers, Node1Pid),
    stoplight_srv:start_named(node2),
    stoplight_srv:start_named(node3),
    ok.

node_state_test_() ->
  {
      setup, fun setup/0,
      fun () ->
         ?assert(true =:= true),
         {ok, State1} = gen_server:call(node1, {state}),
         ?assert(is_record(State1, srv_state) =:= true),
         % ?assertEqual(testnode1, gen_server:call(testnode1, {registered_name})),
         {ok}
      end
  }.
