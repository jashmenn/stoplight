-module(eunit_benchmark_stoplight_sigma_changes).

-include_lib("eunit/include/eunit.hrl").
-include_lib("../../include/defines.hrl").
-include_lib("../include/stoplight_eunit_helpers.hrl").
-define(LOCK_DIR, "/tmp/stoplight_locks").
% -define(TRACEP(Pid), ttb:p(Pid, [call,send,messages,sos,sol])).
-define(TRACEP(Pid), ttb:p(Pid, [call,send,sos,sol])).


%% Include ms_transform.hrl so that I can use dbg:fun2ms/2 to
%% generate match specifications.
-include_lib("stdlib/include/ms_transform.hrl").

% subtract out m responses for every crit
% what we need to trace:
% * every crit sent to client from lobbyist
% * every INQUIRY rec'd by stoplight_srv     
% * every RESPOSE rec'd by stoplight_lobbyist 

setup2() ->
    crypto:start(),
    ok.

teardown2(_) ->
    crypto:stop(),
    ok.

flush_buffer(0) ->
    ok;
flush_buffer(N) -> 
    receive 
        {done, _Pid} ->
            % ?TRACE("Pid done ", [Pid, left, N-1]),
            flush_buffer(N-1);
        _Any -> 
            flush_buffer(N) 
    after 5000 -> 
            true 
    end. 


node_benchmark_test_() ->
  {
      setup, fun setup2/0, fun teardown2/1,
      {timeout, 300, 
      fun () ->

          %% create a pool of 5 servers
          %% create a pool of 5 listeners
          %% create a pool of N clients all trying to get a lock on tree
          %% create a pool of M clients all trying to get a lock on apple
         Parent = self(),
                 io:format(user, "~n", []),

         lists:map(fun(NumClients) ->
             lists:map(fun(RunCount) ->

                 io:format(user, "Testing ~p run ~p ... ", [NumClients, RunCount]),
                 [ListenerPool, ServerPool] = start_run(),

                 lists:map(fun(_I) ->
                             spawn_link(fun() -> 
                                lock_tester:try_for(apple, ListenerPool),
                                Parent ! {done, self()}
                     end)
                  end,
                 lists:seq(1, 3)),

                 flush_buffer(3),
                 stop_run(lists:append(ListenerPool, ServerPool)),
                 ok


             end,
             lists:seq(1, 3))
         end,
         lists:seq(1, 3)),

         {ok}
      end
      }
  }.


start_run() ->
     os:cmd("rm -rf trace/* && mkdir -p trace"),
     erlang:monitor(process,self()),
     ttb:tracer(node(), [{file,"trace/ttb"},{process_info,true}]),
     ?TRACEP(self()),

     MS1 = [{'_',[],[{return_trace},{message,{caller}}]}], % dbg:fun2ms(fun(_) -> return_trace(),message(caller()) end),
     ttb:tpl(gen_server, cast, MS1),

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

     [ListenerPool, ServerPool].

 stop_run(NodesToStop) ->
    ?stop_and_unregister_servers(NodesToStop),
    ?stop_and_unregister_globals,

     ttb:stop(),
     ttb:format("trace", [{handler,{{stoplight_benchmarking_tracer,print},3}}]),
     ?sleep(100),
     lists:map(fun(P) ->
                 exit(P, kill)
         end, NodesToStop),

     ok.

