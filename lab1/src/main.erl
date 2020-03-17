%%%-------------------------------------------------------------------
%%% @author wiktor
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Mar 2020 10:31
%%%-------------------------------------------------------------------
-module(main).
-author("wiktor").

%% API
-export([power/2]).

fu() -> 123.

power(Number, 1) ->
  Number * 1;
power(Number, Pwr) ->
  Number * power(Number, Pwr-1).
