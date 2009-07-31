-define (DEBUG, true).
-define (DICT, dict).
-define (SERVER_MODULE, stoplight_srv).
-define (TRACE(X, M),  io:format(user, "TRACE ~p:~p ~p ~p~n",           [?MODULE, ?LINE, X, M])).
-define (NTRACE(X, M), io:format(user, "NTRACE ~p:~p ~p ~p ~p ~p ~p~n", [?MODULE, ?LINE, ?SERVER_MODULE:registered_name(), node(), self(), X, M])).

-define (RECONNECT_TIMEOUT, 10000).

-define (DEFAULT_CONFIG,  [
                             % {comm, chordjerl_com}
                          ]).

-record(noderef, {
    pidname,
    hostname,
    port,
    ip,
    pidref
  }).

-record(srv_state, {
    pid,
    ring
  }).

-record(client_state, {
    pid
  }).


% -record(finger, {
%     sha,
%     node,
%     pid,
%     ip,
%     port
%   }).

