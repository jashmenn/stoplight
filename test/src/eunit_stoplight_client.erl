-module(eunit_stoplight_client).

-include_lib("eunit/include/eunit.hrl").
-include_lib("../../include/defines.hrl").
-include_lib("../include/stoplight_eunit_helpers.hrl").
-define(LOCK_DIR, "/tmp/stoplight_locks").

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

node_state_test_not() ->
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


setup2() ->
    crypto:start(),
    ok.

teardown2(_) ->
    crypto:stop(),
    ok.

node_fuzz_one_test_() ->
  {
      setup, fun setup2/0, fun teardown2/1,
      {timeout, 300, 
      fun () ->
         os:cmd("rm -rf " ++ ?LOCK_DIR),

         {ok, Node1Pid} = stoplight_srv:start_named(stoplight_srv_local, {seed, undefined}),
         {ok, Node2Pid} = stoplight_srv:start_named(node2, {seed, Node1Pid}),
         {ok, Node3Pid} = stoplight_srv:start_named(node3, {seed, Node1Pid}),
         {ok, Node4Pid} = stoplight_srv:start_named(node4, {seed, Node1Pid}),
         {ok, Node5Pid} = stoplight_srv:start_named(node5, {seed, Node1Pid}),
         ServerPool = [Node1Pid, Node2Pid, Node3Pid, Node4Pid, Node5Pid],

         {ok, Listener1Pid} = stoplight_listener:start_named_link(listener1, [], []),
         {ok, Listener2Pid} = stoplight_listener:start_named_link(listener2, [], []),
         {ok, Listener3Pid} = stoplight_listener:start_named_link(listener3, [], []),
         {ok, Listener4Pid} = stoplight_listener:start_named_link(listener4, [], []),
         {ok, Listener5Pid} = stoplight_listener:start_named_link(listener5, [], []),
         ListenerPool = [Listener1Pid, Listener2Pid, Listener3Pid, Listener4Pid, Listener5Pid],

         erlang:monitor(process, Listener1Pid),
         erlang:monitor(process, Listener2Pid),
         erlang:monitor(process, Listener3Pid),
         erlang:monitor(process, Listener4Pid),
         erlang:monitor(process, Listener5Pid),

          %% create a pool of 5 servers
          %% create a pool of 5 listeners
          %% create a pool of N clients all trying to get a lock on tree
          %% create a pool of M clients all trying to get a lock on apple
         Parent = self(),

         lists:map(fun(I) ->
                     spawn_link(fun() -> 
                     timer:sleep(random:uniform(300)),
                        lock_tester:try_for(tree, ListenerPool),
                        Parent ! {done, self()}
             end)
          end,
         lists:seq(1, 20)),

         lists:map(fun(I) ->
                     spawn_link(fun() -> 
                     timer:sleep(random:uniform(300)),
                        lock_tester:try_for(apple, ListenerPool),
                        Parent ! {done, self()}
             end)
          end,
         lists:seq(1, 20)),


         flush_buffer(40),

        % receive 
        %      {'DOWN',MRef,process,_,_} -> 
        %          ?TRACE("got the monitor val!", val)
        % after 1000 -> 
        %         ?TRACE("never got the monitor val", val)
        % end, 
         {ok}
      end
      }
  }.


flush_buffer(0) ->
    ok;
flush_buffer(N) -> 
    receive 
        {done, Pid} ->
            % ?TRACE("Pid done ", [Pid, left, N-1]),
            flush_buffer(N-1);
        _Any -> 
            flush_buffer(N) 
    after 5000 -> 
            true 
    end. 

