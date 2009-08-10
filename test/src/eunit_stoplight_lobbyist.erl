-module(eunit_stoplight_lobbyist).

-include_lib("eunit/include/eunit.hrl").
-include_lib("../../include/defines.hrl").

setup() ->
    {ok, Node1Pid} = stoplight_srv:start_named(srv1, {seed, undefined}),
    {ok, _} = stoplight_srv:start_named(srv2, {seed, Node1Pid}),
    {ok, _} = stoplight_srv:start_named(srv3, {seed, Node1Pid}),

    {ok, Servers} = gen_cluster:call(srv1, {'$gen_cluster', plist}),
    ?assertEqual(3, length(Servers)),

    {ok, _} = stoplight_lobbyist:start_named(lobbyist1, [{name, food}, {servers, Servers}, {client, self()}]),
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
         {ok, State1} = gen_cluster:call(lobbyist1, state),
         % ?assert(is_record(State1, state) =:= true),
         % {ok, Plist} = gen_cluster:call(lobbyist, {'$gen_cluster', plist}),
         % ?assertEqual(3, length(Plist)),
         {ok}
      end
  }.

node_multicast_request_test_() ->
  {
      setup, fun setup/0, fun teardown/1,
      fun () ->
         {ok, Mock1} = gen_server_mock:new(),
         {ok, Mock2} = gen_server_mock:new(),
         {ok, Mock3} = gen_server_mock:new(),
         Servers = [Mock1, Mock2, Mock3],

         gen_server_mock:expect_cast(Mock1, fun({mutex, request, _R}, _State) -> ok end),
         gen_server_mock:expect_cast(Mock2, fun({mutex, request, _R}, _State) -> ok end),
         gen_server_mock:expect_cast(Mock3, fun({mutex, request, _R}, _State) -> ok end),

         {ok, LobPid} = stoplight_lobbyist:start_named(lobbyist2, [{name, cats}, {servers, Servers}, {client, self()}]),
         ok = gen_server:call(LobPid, petition),

         gen_server_mock:assert_expectations([Mock1, Mock2, Mock3]),
         gen_server:call(LobPid, stop),
         gen_server_mock:stop([Mock1, Mock2, Mock3]),
         {ok}
      end
  }.

node_responses_test_() ->
  {
      setup, fun setup/0, fun teardown/1,
      fun () ->
         {ok, Mock1} = gen_server_mock:new(),
         {ok, Mock2} = gen_server_mock:new(),
         {ok, Mock3} = gen_server_mock:new(),
         Servers = [Mock1, Mock2, Mock3],

         {ok, Lob} = stoplight_lobbyist:start_named(lobbyist2, [{name, cats}, {servers, Servers}, {client, self()}]),
         {ok, R0} = gen_server:call(Lob, request),

         % respond with support 
         gen_server:cast(Lob, {mutex, response, R0, Mock1}),

         % verify we have support from that server 
         {ok, Resps0} = gen_server:call(Lob, responses),
         ?TRACE("Resps", Resps0),
         {ok, R0} = dict:find(Mock1, Resps0),

         % gen_server_mock:expect_cast(Mock1, fun({mutex, request, _R}, _State) -> ok end),
         % gen_server_mock:expect_cast(Mock2, fun({mutex, request, _R}, _State) -> ok end),
         % gen_server_mock:expect_cast(Mock3, fun({mutex, request, _R}, _State) -> ok end),
         % ok = gen_server:call(LobPid, petition),

         gen_server_mock:assert_expectations([Mock1, Mock2, Mock3]),
         gen_server:call(Lob, stop),
         gen_server_mock:stop([Mock1, Mock2, Mock3]),
         {ok}
      end
  }.

