%%%-------------------------------------------------------------------
%%% @author wiktor
%%% @doc
%%%
%%% @end
%%% Created : 22. Mar 2020 12:55
%%%-------------------------------------------------------------------
-module(fun_mod).
-author("wiktor").

%% API
-export([map_2/2, filter_2/2, foldl_2/3, countSum/1, numbersFilter/0]).

map_2(_, []) -> [];
map_2(Fun, [Head|Tail]) ->
  [Fun(Head)|map_2(Fun, Tail)].

filter_2(_, []) -> [];
filter_2(Fun, List)->
  [X || X <- List, Fun(X)].

foldl_2(_, Acc, []) -> Acc;
foldl_2(Fun, Acc, [Head|Tail])->
  foldl_2(Fun, Fun(Head, Acc), Tail).

countSum(Number) ->
  Sum = fun(X, Y) -> X + Y - 48 end,
  List = integer_to_list(Number),
  lists:foldl(Sum, 0, List).

numbersFilter()->
  NumberList = qsort:randomElems(1000000, 1, 2000000),
  Rem3 = fun(X) -> fun_mod:countSum(X) rem 3 == 0 end,
  lists:filter(Rem3, NumberList).