%%%-------------------------------------------------------------------
%%% @author wiktor
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Mar 2020 16:06
%%%-------------------------------------------------------------------
-module(onp).
-author("wiktor").

%% API
-export([onp/1]).

onp(Expression) ->
  List = string:tokens(Expression, " "),
  evaluate_expression(List, []).

list_to_number(L) ->
  try list_to_float(L)
  catch
    error:badarg ->
      list_to_integer(L)
  end.

evaluate_expression([], [H]) -> H;
evaluate_expression([H|T], [H2|T2])->
  if
    H == "+" -> evaluate_expression(T, lists:append([lists:nth(1, T2)+H2], lists:delete(lists:nth(1, T2), T2)));
    H == "-" -> evaluate_expression(T, lists:append([lists:nth(1, T2)-H2], lists:delete(lists:nth(1, T2), T2)));
    H == "*" -> evaluate_expression(T, lists:append([lists:nth(1, T2)*H2], lists:delete(lists:nth(1, T2), T2)));
    H == "/" -> evaluate_expression(T, lists:append([lists:nth(1, T2)/H2], lists:delete(lists:nth(1, T2), T2)));
    H == "pow" -> evaluate_expression(T, lists:append([math:pow(lists:nth(1, T2), H2)], lists:delete(lists:nth(1, T2), T2)));
    H == "sin" -> evaluate_expression(T, lists:append([math:sin(H2)], T2));
    H == "cos" -> evaluate_expression(T, lists:append([math:cos(H2)], T2));
    H == "sqrt" -> evaluate_expression(T, lists:append([math:sqrt(H2)], T2));
    true ->
      Number = list_to_number(H),
      if
        is_number(Number) -> evaluate_expression(T, lists:append([Number], [H2|T2]));
        true -> io:format("Invalid character")
      end
  end;
evaluate_expression([H|T], [])->
  Number = list_to_number(H),
  if
    is_number(Number) ->  evaluate_expression(T, [Number]);
    true -> io:format("You have to enter a number first!")
  end.





