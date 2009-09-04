-module(eunit_stoplight_lobbyist).

-include_lib("eunit/include/eunit.hrl").
-include_lib("../../include/defines.hrl").

setup() ->
    {ok, Node1Pid} = stoplight_srv:start_named(srv1, {seed, undefined}),
    {ok, _} = stoplight_srv:start_named(srv2, {seed, Node1Pid}),
    {ok, _} = stoplight_srv:start_named(srv3, {seed, Node1Pid}),

    {ok, Servers} = gen_cluster:call(srv1, {'$gen_cluster', plist}),
    ?assertEqual(3, length(Servers)),

    {ok, _} = stoplight_lobbyist:start_named(lobbyist1, [{name, food}, {servers, Servers}, {client, self()}, {request_ttl, 5000}]),
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
         {ok, _State1} = gen_cluster:call(lobbyist1, state),
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
         _Servers = [Mock1, Mock2, Mock3],

         % register Mock1 as our hook server
         register(stoplight_srv_local, Mock1),
         gen_server_mock:expect_call(Mock1, fun({'$gen_cluster', plist}, _From, State) -> {ok, {ok, [Mock1, Mock2, Mock3]}, State} end),

         gen_server_mock:expect_cast(Mock1, fun({mutex, request, _R}, _State) -> ok end),
         gen_server_mock:expect_cast(Mock2, fun({mutex, request, _R}, _State) -> ok end),
         gen_server_mock:expect_cast(Mock3, fun({mutex, request, _R}, _State) -> ok end),

         {ok, LobPid} = stoplight_lobbyist:start_named(lobbyist2, [{name, cats}, {client, self()}, {request_ttl, 5000}]),
         ok = gen_server:call(LobPid, petition),

         gen_server_mock:assert_expectations([Mock1, Mock2, Mock3]),
         gen_server:call(LobPid, stop),
         gen_server_mock:stop([Mock1, Mock2, Mock3]),
         unregister(stoplight_srv_local),
         {ok}
      end
  }.

node_responses_get_crit_test_() ->
  {
      setup, fun setup/0, fun teardown/1,
      fun () ->
         Servers = gen_server_mock:new(3),
         [Mock1, Mock2, _Mock3] = Servers,

         {ok, Client} = gen_server_mock:new(),

         {ok, Lob} = stoplight_lobbyist:start_named(lobbyist2, [{name, cats}, {servers, Servers}, {client, Client}, {request_ttl, 5000}]),
         {ok, R0} = gen_server:call(Lob, request),

         % expect getting crit
         gen_server_mock:expect_info(Client, fun({crit, _Request, _From}, _State) -> ok end),

         % respond with support 
         gen_server:cast(Lob, {mutex, response, R0, Mock1}),
         gen_server:cast(Lob, {mutex, response, R0, Mock2}),

         % verify we have support from that server 
         {ok, Resps0} = gen_server:call(Lob, responses),
         ?assertEqual({ok, R0}, dict:find(Mock1, Resps0)),
         ?assertEqual({ok, R0}, dict:find(Mock2, Resps0)),

         % expect releases
         lists:map(fun(ServerPid) ->
             gen_server_mock:expect_cast(ServerPid, fun({mutex, release, Request}, _State) when Request =:= R0 -> ok end)
         end, Servers),

         % check normally
         gen_server:cast(Lob, {mutex, check, R0, Mock1}),

         % check with the wrong timestamp, expect release
         R1 = R0#req{timestamp=0},
         gen_server_mock:expect_cast(Mock2, fun({mutex, release, Request}, _State) when Request =:= R0 -> ok end),
         gen_server:cast(Lob, {mutex, check, R1, Mock2}),

         % release the lock
         ok = gen_server:call(Lob, release),

         gen_server_mock:assert_expectations([Client|Servers]),
         gen_server_mock:stop([Client|Servers]),
         {ok}
      end
  }.

node_responses_dont_get_crit_test_() ->
  {
      setup, fun setup/0, fun teardown/1,
      fun () ->
         Servers = gen_server_mock:new(5),
         [Mock1, Mock2, Mock3, Mock4, _Mock5] = Servers,
         ?TRACE("Servers", Servers),

         {ok, Client} = gen_server_mock:new(),

         % create two lobbists, get their requests
         % R1 is older
         {ok, Lob2} = stoplight_lobbyist:start_named(lobbyist3, [{name, cats}, {servers, Servers}, {client, Client}, {request_ttl, 5000}]),
         {ok, R1} = gen_server:call(Lob2, request),

         {ok, Lob1} = stoplight_lobbyist:start_named(lobbyist2, [{name, cats}, {servers, Servers}, {client, Client}, {request_ttl, 5000}]),
         {ok, R0} = gen_server:call(Lob1, request),

         R2 = R0#req{owner=self(), timestamp=9999999999999999}, % fake req with way newer timestamp

         % expect inquiry

         % Mock1 gets yield b/c Lob1 does not have enough support
         gen_server_mock:expect_cast(Mock1, fun({mutex, yield,   _R}, _State) -> ok end),
         % Mock2 gets request because our request is before R2 
         gen_server_mock:expect_cast(Mock2, fun({mutex, request, _R}, _State) -> ok end),
         % Mock3 & Mock4 get inquiry because R1 is older than R0
         gen_server_mock:expect_cast(Mock3, fun({mutex, inquiry, _R}, _State) -> ok end),
         gen_server_mock:expect_cast(Mock4, fun({mutex, inquiry, _R}, _State) -> ok end),

         % respond with support
         gen_server:cast(Lob1, {mutex, response, R0, Mock1}),
         gen_server:cast(Lob1, {mutex, response, R2, Mock2}),

         % respond with supporting someone else 
         gen_server:cast(Lob1, {mutex, response, R1, Mock3}),
         gen_server:cast(Lob1, {mutex, response, R1, Mock4}),

         % sync
         timer:sleep(2000), % wait for the second inquiry
         gen_server:call(Lob1, state),
         gen_server_mock:assert_expectations([Client|Servers]),
         gen_server:call(Lob1, stop),
         gen_server:call(Lob2, stop),
         gen_server_mock:stop([Client|Servers]),
         {ok}
      end
  }.

