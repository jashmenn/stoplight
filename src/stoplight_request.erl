%%%-------------------------------------------------------------------
%%% File    : stoplight_request.erl
%%% Author  : nmurray
%%% Description : desc
%%% Created     : 2009-08-03
%%%-------------------------------------------------------------------

-module(stoplight_request).
-include_lib("../include/defines.hrl").
-compile(export_all).

sort_by_timestamp(ReqList) ->
    lists:sort(fun(Elem1, Elem2) ->
                #req{timestamp=T1} = Elem1,
                #req{timestamp=T2} = Elem2,
                T1 < T2
        end,
        ReqList).
