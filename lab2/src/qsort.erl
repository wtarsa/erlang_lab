%%%-------------------------------------------------------------------
%%% @author wiktor
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Mar 2020 00:30
%%%-------------------------------------------------------------------
-module(qsort).
-author("wiktor").

%% API
-export([qs/1, randomElems/3, compareSpeeds/3]).

lessThan(List, Arg)->
  [X || X <- List, X < Arg].

grtEqThan(List, Arg)->
  [X || X <- List, X >= Arg].

qs([]) -> [];
qs([Pivot|Tail]) ->
  qs( lessThan(Tail,Pivot) ) ++ [Pivot] ++ qs( grtEqThan(Tail,Pivot) ) .

randomElems(N, Min, Max) ->
  [Min + rand:uniform(Max-Min) || X <- lists:seq(1, N)].

compareSpeeds(List, Fun1, Fun2) ->
  {Time_qs, _} = timer:tc(Fun1, [List]),
  {Time_sort, _} = timer:tc(Fun2, [List]),
  io:format("quicksort: ~p, sort: ~p ", [Time_qs, Time_sort]).