-module(eunit_stoplight_client).

-include_lib("eunit/include/eunit.hrl").
-include_lib("../../include/defines.hrl").
-include_lib("../include/stoplight_eunit_helpers.hrl").

setup() ->
    {ok, Node1Pid}  = stoplight_srv:start_named(stoplight_srv_local, {seed, undefined}),
    {ok, Node2Pid} = stoplight_srv:start_named(node2, {seed, Node1Pid}),
    {ok, Node3Pid} = stoplight_srv:start_named(node3, {seed, Node1Pid}),

    {ok, ListenerPid} = stoplight_listener:start_link([], []),
    ?assert(is_pid(ListenerPid)),

    ttb:tracer(node(), [{file,"trace/ttb"}]),

    lists:map(fun(Pid) ->
       ttb:p(Pid, [call,send])
    end, [ListenerPid, Node1Pid, Node2Pid, Node3Pid]),

    MS1 = [{'_',[],[{return_trace},{message,{caller}}]}], % dbg:fun2ms(fun(_) -> return_trace(),message(caller()) end),
    ttb:tpl(gen_server, loop, MS1),

    [stoplight_listener, stoplight_srv_local, node2, node3].

teardown(Servers) ->
    ttb:stop(),
    ?stop_and_unregister_servers(Servers),
    ?stop_and_unregister_globals,

    % ttb:format(atom_to_list(node()) ++ "-ttb"),
    % ttb:format(atom_to_list(node()) ++ "-ttb", [{handler, et}]),
    ttb:format("trace"),
    % ttb:format("trace", [{handler, et}]),
 
    ok.

node_state_test_() ->
  {
      setup, fun setup/0, fun teardown/1,
      {timeout, 300, 
      fun () ->

         {ok, State} = gen_server:call(stoplight_listener, state),

         {crit, Lobbyist} = stoplight_client:lock(tree),
         register(tree_lobbyist, Lobbyist),

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
         after 2000 -> timeout
         end,


         
         {ok}
      end
      }
  }.


