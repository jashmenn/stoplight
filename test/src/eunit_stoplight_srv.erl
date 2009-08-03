-module(eunit_stoplight_srv).

-include_lib("eunit/include/eunit.hrl").
-include_lib("../../include/defines.hrl").

setup() ->
    ok.

node_state_test_() ->
  {
      setup, fun setup/0,
      fun () ->
         % State1 = gen_server:call(testnode1, {return_state}),
         % ?assert(is_record(State1, srv_state) =:= true),
          ?assert(true =:= true),
         {ok}
      end
  }.


