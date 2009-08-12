-module(eunit_stoplight_client).

-include_lib("eunit/include/eunit.hrl").
-include_lib("../../include/defines.hrl").
-include_lib("../include/stoplight_eunit_helpers.hrl").

setup() ->
    {ok, Node1Pid}  = stoplight_srv:start_named(stoplight_srv_local, {seed, undefined}),
    {ok, _Node2Pid} = stoplight_srv:start_named(node2, {seed, Node1Pid}),
    {ok, _Node3Pid} = stoplight_srv:start_named(node3, {seed, Node1Pid}),

    {ok, ListenerPid} = stoplight_listener:start_link([], []),
    ?assert(is_pid(ListenerPid)),

% erlang:monitor(process,Pid),
% erlang:trace(Node1Pid,true,[send,call,{tracer,self()}]),
% erlang:trace_pattern({gen_server,loop,'_'},true,[local]),

    ttb:tracer(),
    ttb:p(ListenerPid, [call,send]),
    % MS1 = dbg:fun2ms(fun(_) -> return_trace(),message(caller()) end),
    MS1 = [{'_',[],[{return_trace},{message,{caller}}]}],
    ttb:tp(gen_server, call, MS1),


    [stoplight_listener, stoplight_srv_local, node2, node3].

teardown(Servers) ->
    ttb:stop(),
    ?stop_and_unregister_servers(Servers),
    ?stop_and_unregister_globals,

    ttb:format("nonode@nohost-ttb"),
    ttb:format("nonode@nohost-ttb", [{handler, et}]),
    ok.

node_state_test_() ->
  {
      setup, fun setup/0, fun teardown/1,
      {timeout, 300, 
      fun () ->

         {ok, State} = gen_server:call(stoplight_listener, state),

         {crit, Lobbyist} = stoplight_client:lock(tree),

         Parent = self(),

         spawn_link(fun() ->
           ?TRACE("trying to get tree also", val),
           Resp = stoplight_client:lock(tree),
           ?assertEqual(no, Resp),
           ?TRACE("Resp", Resp),
           Parent ! {self(), done}
         end),

         receive
             {Pid, done} -> 
                 ?TRACE("rec'd my message!", val),
                 ok
         after 5000 -> timeout
         end,


         
         {ok}
      end
      }
  }.


