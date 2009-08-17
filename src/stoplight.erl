-module (stoplight).
-include_lib("../include/defines.hrl").
-behaviour (application).

-export([start/2, stop/1]).

start(Type, _Args) ->
  stoplight_srv_sup:start_link(Type, [{module, ?MODULE}]).

stop(_State) -> ok.
