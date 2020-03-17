%%%-------------------------------------------------------------------
%%% @author wiktor
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Mar 2020 10:56
%%%-------------------------------------------------------------------
-module(myLists).
-author("wiktor").

%% API
-export([contains/2, duplicateElements/2, sumFloats/1, sumFloats2/2]).

contains([], Param)->
  false;
contains([Head | Tail], Param)->
  (Head == Param) or contains(Tail, Param).

duplicateElements([], [H | T])->
  [H | T];
duplicateElements([Head | Tail], [H | T])->
  duplicateElements(Tail, [H| lists:append(T, [Head, Head])]);
duplicateElements([Head | Tail], [])->
  duplicateElements(Tail, [Head , Head]).

sumFloats([])->
  0;
sumFloats([Head|Tail])->
  if
    is_float(Head) == true -> Head + sumFloats(Tail);
    true -> sumFloats(Tail)
  end.

sumFloats2([], Sum)->
  Sum;
sumFloats2([Head|Tail], Sum)->
  if
    is_float(Head) == true -> sumFloats2(Tail, Sum+Head);
    true -> sumFloats2(Tail, Sum)
  end.