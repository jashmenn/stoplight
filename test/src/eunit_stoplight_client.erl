-module(eunit_stoplight_client).

-include_lib("eunit/include/eunit.hrl").
-include_lib("../../include/defines.hrl").
-include_lib("../include/stoplight_eunit_helpers.hrl").

setup() ->
    register(eunit_stoplight_client, self()),
    ttb:tracer(node(), [{file,"trace/ttb"},{process_info,false}]),
    % ttb:tracer(node(), [{file,"trace/ttb"},{process_info,true}]),
    ttb:p(self(), [call,send,messages,sos,sol]),

    {ok, Node1Pid} = stoplight_srv:start_named(stoplight_srv_local, {seed, undefined}),
    {ok, Node2Pid} = stoplight_srv:start_named(node2, {seed, Node1Pid}),
    {ok, Node3Pid} = stoplight_srv:start_named(node3, {seed, Node1Pid}),

    {ok, ListenerPid} = stoplight_listener:start_link([], []),
    ?assert(is_pid(ListenerPid)),

    lists:map(fun(Pid) ->
       ttb:p(Pid, [call,send,messages,sos,sol])
    end, [ListenerPid, Node1Pid, Node2Pid, Node3Pid]),

    MS1 = [{'_',[],[{return_trace},{message,{caller}}]}], % dbg:fun2ms(fun(_) -> return_trace(),message(caller()) end),
    ttb:tpl(gen_server, loop, MS1),
    ttb:tpl(gen_server, cast, MS1),

    [stoplight_listener, stoplight_srv_local, node2, node3].

teardown(Servers) ->
    ttb:stop(),
    ?stop_and_unregister_servers(Servers),
    ?stop_and_unregister_globals,
    ttb:format("trace"),
    ok.

node_state_test_() ->
  {
      setup, fun setup/0, fun teardown/1,
      {timeout, 300, 
      fun () ->

         {ok, State} = gen_server:call(stoplight_listener, state),

         {crit, Lobbyist} = stoplight_client:lock(tree, 10),
         register(tree_lobbyist, Lobbyist),

         Parent = self(),

         Pid1 = spawn_link(fun() ->
           {Resp, Lob2} = stoplight_client:lock(tree, 10),
           ?assertEqual(no, Resp),
           ok = stoplight_client:release(Lob2),
           Parent ! {self(), done}
         end),
         receive {Pid1, done} -> ok
         after 500 -> timeout
         end,

         Pid2 = spawn_link(fun() ->
           {Resp2, Lob3} = stoplight_client:lock(tree, 1000),
           ?assertEqual(crit, Resp2),
           Parent ! {self(), done}
         end),
         ok = stoplight_client:release(Lobbyist),
         receive {Pid2, done} -> ok
         after 500 -> timeout
         end,

         
         {ok}
      end
      }
  }.


