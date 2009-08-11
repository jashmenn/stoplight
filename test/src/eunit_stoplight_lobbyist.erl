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

node_state_test_not() ->
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

node_multicast_request_test_not() ->
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

node_responses_get_crit_test_not() ->
  {
      setup, fun setup/0, fun teardown/1,
      fun () ->
         Servers = gen_server_mock:new(3),
         [Mock1, Mock2, Mock3] = Servers,

         {ok, Client} = gen_server_mock:new(),

         {ok, Lob} = stoplight_lobbyist:start_named(lobbyist2, [{name, cats}, {servers, Servers}, {client, Client}]),
         {ok, R0} = gen_server:call(Lob, request),

         % expect getting crit
         gen_server_mock:expect_info(Client, fun({crit, Request, _From}, _State) -> ok end),

         % respond with support 
         gen_server:cast(Lob, {mutex, response, R0, Mock1}),
         gen_server:cast(Lob, {mutex, response, R0, Mock2}),

         % verify we have support from that server 
         {ok, Resps0} = gen_server:call(Lob, responses),
         ?assertEqual({ok, R0}, dict:find(Mock1, Resps0)),
         ?assertEqual({ok, R0}, dict:find(Mock2, Resps0)),

         gen_server_mock:assert_expectations([Client|Servers]),
         gen_server:call(Lob, stop),
         gen_server_mock:stop([Client|Servers]),
         {ok}
      end
  }.

node_responses_dont_get_crit_test_() ->
  {
      setup, fun setup/0, fun teardown/1,
      fun () ->
         Servers = gen_server_mock:new(3),
         [Mock1, Mock2, Mock3] = Servers,

         {ok, Client} = gen_server_mock:new(),

         {ok, Lob1} = stoplight_lobbyist:start_named(lobbyist2, [{name, cats}, {servers, Servers}, {client, Client}]),
         {ok, R0} = gen_server:call(Lob1, request),

         {ok, Lob2} = stoplight_lobbyist:start_named(lobbyist3, [{name, cats}, {servers, Servers}, {client, Client}]),
         {ok, R1} = gen_server:call(Lob2, request),

         % expect inquiry
         gen_server_mock:expect_cast(Mock1, fun({mutex, inquiry, Request}, _State) -> ok end),
         gen_server_mock:expect_cast(Mock2, fun({mutex, inquiry, Request}, _State) -> ok end),

         % gen_server_mock:expect_info(Client, fun({crit, Request, _From}, _State) -> ok end),

         % respond with supporting someone else 
         gen_server:cast(Lob1, {mutex, response, R1, Mock1}),
         gen_server:cast(Lob1, {mutex, response, R1, Mock2}),

         % % verify we have support from that server 
         % {ok, Resps0} = gen_server:call(Lob, responses),
         % ?assertEqual({ok, R0}, dict:find(Mock1, Resps0)),
         % ?assertEqual({ok, R0}, dict:find(Mock2, Resps0)),

         gen_server:call(Lob1, state),

         gen_server_mock:assert_expectations([Client|Servers]),
         gen_server:call(Lob1, stop),
         gen_server:call(Lob2, stop),
         gen_server_mock:stop([Client|Servers]),
         {ok}
      end
  }.

