-module(eunit_stoplight_listener).

-include_lib("eunit/include/eunit.hrl").
-include_lib("../../include/defines.hrl").
-include_lib("../include/stoplight_eunit_helpers.hrl").

setup() ->
    {ok, _Node1Pid} = stoplight_listener:start_link([], []),
    [stoplight_listener].

teardown(Servers) ->
    ?stop_and_unregister_servers(Servers),
    ?stop_and_unregister_globals,
    ok.

node_state_test_() ->
  {
      setup, fun setup/0, fun teardown/1,
      fun () ->
         % register Mock1 as our hook server
         {ok, Mock1} = gen_server_mock:new(),
         register(stoplight_srv_local, Mock1),
         gen_server_mock:expect_call(Mock1, fun({'$gen_cluster', plist}, _From, State) -> {ok, {ok, [Mock1]}, State} end),
         gen_server_mock:expect_cast(Mock1, fun({mutex, request, _Request}, _State) -> ok end),

         {ok, State1} = gen_server:call(stoplight_listener, state),           {state, _} = State1,
         {ok, Lob} = gen_server:call(stoplight_listener, {try_mutex, bobby}), ?assert(is_pid(Lob)),

         {ok, Request} = gen_server:call(Lob, request),
         ?assertEqual(bobby, Request#req.name),

         {ok, _State} = gen_server:call(Lob, state), % sync
         ?sleep(1),
         gen_server_mock:assert_expectations(Mock1),
         unregister(stoplight_srv_local),

         {ok}
      end
  }.


