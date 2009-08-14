-define (DEBUG, true).
-define (DICT, dict).
-define (SERVER_MODULE, stoplight_srv).
-define (SERVER_GLOBAL, stoplight_srv).

-define (TRACE(X, M), case ?DEBUG of
  true -> io:format(user, "TRACE ~p:~p ~p ~p~n", [?MODULE, ?LINE, X, M]);
  false -> ok
end).

-define (NTRACE(X, M), io:format(user, "NTRACE ~p:~p ~p ~p ~p ~p ~p~n", [?MODULE, ?LINE, ?SERVER_MODULE:registered_name(), node(), self(), X, M])).
-define (RECONNECT_TIMEOUT, 10000).
-define (DEFAULT_CONFIG,  []).
-define (STOPLIGHT_LISTENER,  stoplight_listener).

-record(noderef, {
    name
  }).

-define (TRACING, true).
% -define (enable_tracing, ttb:p(self(), [call,send])).
-define (enable_tracing, ok).

% cowner: the client it accepts, initially nil. 
% towner: time stamp of cowner, initially nil. 
% ReqQ: queue storing requests, initially empty. 

-record(srv_state, {
    pid,
    ring,
    nodename,
    reqQs, % #req queues, namespaced by lock name
    owners % owner #reqs, namespaced by lock name
  }).

-record(req, {
    name,      % lock name
    owner,     % owner pid
    timestamp  % timestamp
 }).

