-define (DEBUG, true).
-define (DICT, dict).
-define (SERVER_MODULE, stoplight_srv).
-define (SERVER_GLOBAL, stoplight_srv).
-define (TRACE(X, M),  io:format(user, "TRACE ~p:~p ~p ~p~n",           [?MODULE, ?LINE, X, M])).
-define (NTRACE(X, M), io:format(user, "NTRACE ~p:~p ~p ~p ~p ~p ~p~n", [?MODULE, ?LINE, ?SERVER_MODULE:registered_name(), node(), self(), X, M])).
-define (RECONNECT_TIMEOUT, 10000).
-define (DEFAULT_CONFIG,  []).

-record(noderef, {
    name
  }).

-record(srv_state, {
    pid,
    ring,
    nodename
  }).

-record(client_state, {
    pid
  }).
