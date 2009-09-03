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


flush_buffer(N) -> 
    flush_buffer(N, 5000).

flush_buffer(0, _Timeout) ->
    ok;
flush_buffer(N, Timeout) -> 
    receive 
        {done, Pid} ->
            % ?TRACE("Pid done ", [Pid, left, N-1]),
            flush_buffer(N-1, Timeout);
        _Any -> 
            flush_buffer(N, Timeout) 
    after Timeout -> 
            ?TRACE("buffer flush timeout", []),
            true 
    end. 


node_benchmark_test_() ->
  {
      setup, fun setup2/0, fun teardown2/1,
      {timeout, 1000000, 
      fun () ->

          %% create a pool of 5 servers
          %% create a pool of 5 listeners
          %% create a pool of N clients all trying to get a lock on tree
          %% create a pool of M clients all trying to get a lock on apple
                 io:format(user, "~n", []),
                 % io:format(user, "clients,run,crit,REQUEST,YIELD,RELEASE,INQUIRY,RESPONSE~n", []), 
                 io:format(user, "~7s,~7s,~7s,~7s,~7s,~7s,~7s,~7s ~n", 
                     ["clients",
                         "run",
                         "crits",
                         "REQUEST",
                         "YIELD",
                         "RELEASE",
                         "INQUIRY",
                         "RESPONSE"]),

         lists:map(fun(NumClients) ->
             lists:map(fun(RunCount) ->

                 Parent = self(),
                 % Timeout = 60000 * 3,
                 Timeout = 60000,

                 io:format(user, "~7B,~7B,", [NumClients, RunCount]),
                 [ListenerPool, ServerPool] = start_run(),

                 % ?TRACE("starting a new round of lock_testers", val),
                 lists:map(fun(I) ->
                             % ?TRACE("spawning tester", I),
                            timer:sleep(random:uniform(1000)),
                             spawn_link(fun() -> 
                                lock_tester:try_for(apple, ListenerPool, Timeout, 100),
                                Parent ! {done, self()}
                     end)
                  end,
                 lists:seq(1, NumClients)),

                 flush_buffer(NumClients, Timeout + 5000),
                 stop_run(lists:append(ListenerPool, ServerPool)),
                 ok


             end,
             % lists:seq(1, 3))
             lists:seq(1, 1))
         end,
         % lists:seq(1, 3)),
         % [1, 5, 10, 15, 30, 50, 100]),
         % [1, 5, 10, 15]),
         % [59]),
         [100]),

         {ok}
      end
      }
  }.


start_run() ->
     os:cmd("rm -rf trace/* && mkdir -p trace"),
     % erlang:monitor(process,self()),
     ttb:tracer(node(), [{file,"trace/ttb"},{process_info,false}]),
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

     % erlang:monitor(process, Listener1Pid),
     % erlang:monitor(process, Listener2Pid),
     % erlang:monitor(process, Listener3Pid),
     % erlang:monitor(process, Listener4Pid),
     % erlang:monitor(process, Listener5Pid),

     [ListenerPool, ServerPool].

 stop_run(NodesToStop) ->
    ?stop_and_unregister_servers(NodesToStop),
    ?stop_and_unregister_globals,

     ttb:stop(),
     ttb:format("trace", [{handler,{{stoplight_benchmarking_tracer,print},3}}]),
     lists:map(fun(P) ->
                 exit(P, kill)
         end, NodesToStop),

     ?sleep(4000),
     ok.

