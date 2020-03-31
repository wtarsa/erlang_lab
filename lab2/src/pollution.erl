%%%-------------------------------------------------------------------
%%% @author wiktor
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Mar 2020 22:39
%%%-------------------------------------------------------------------
-module(pollution).
-author("wiktor").

%% API
-export([createMonitor/0, addStation/3, checkCoord/2, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getEntrySet/1, getDailyMean/3, getHourlyMean/3, getStationCountAboveValue/4]).

%Tworzy monitor - listę złożoną z mapy stacji oraz słownika pomiarów stacji.
createMonitor() -> [maps:new(), dict:new()].

%Funkcja pomocnicza - sprawdza, czy system zawiera stację o konkretnych współrzędnych.
checkCoord({LatX, LatY}, I) ->
  Val = maps:next(I),
  if
    Val == none -> false;
    true -> {K, V, I2} = Val,
    if
      V == {LatX, LatY} -> true;
      true -> checkCoord({LatX, LatY}, I2)
    end
  end.

%Dodaje stacje - sprawdza, czy nazwa oraz lokalizacja stacji nie jest zajęta przez jakąś inną, wcześniej dodaną do systemu stację.
addStation(Name, {LatX, LatY}, Monitor) ->
  IsOccupied = checkCoord({LatX, LatY}, maps:iterator(lists:nth(1, Monitor))),
  if
    IsOccupied == true -> io:format("Station with this co-ordinates has already been added.");
    IsOccupied == false ->
        case maps:find(Name, lists:nth(1, Monitor)) of
          error -> [maps:put(Name, {LatX, LatY}, lists:nth(1, Monitor)), lists:nth(2, Monitor)];
          _ -> io:format("Station with this name has already been added.")
        end
  end.

%Dodaje wartość pomiaru do monitora. Wymaga podania nazwy stacji, w której pomiaru dokonujemy.
%Sprawdza czy stacja o danej nazwie istnieje, jeżeli tak - zapisuje jej współrzędne, by móc dołączyć je do pomiaru.
%Sprawdza czy pomiar o takiej samej godzinie, typie i dla tej samej stacji nie został już dodany do systemu.
addValue(Name,  {{Year, Month, Day},{Hour, Minute, Second}}, Type, Value, Monitor) ->
  case maps:find(Name, lists:nth(1, Monitor)) of
    error -> io:format("There is no station with providen name");
    {ok, {LatX, LatY}} ->
      case dict:is_key(Name, lists:nth(2, Monitor)) of
        false ->
          [lists:nth(1, Monitor), dict:append(Name, {{LatX, LatY}, {{Year, Month, Day},{Hour, Minute, Second}}, Type, Value}, lists:nth(2, Monitor))];
        true -> case [{LX, LY, Y, Mon, D, H, Min, S, T, V} || {{LX, LY}, {{Y, Mon, D},{H, Min, S}}, T, V} <- dict:fetch(Name, lists:nth(2, Monitor)),
          {LX, LY, Y, Mon, D, H, Min, S, T} == {LatX, LatY, Year, Month, Day, Hour, Minute, Second, Type}] of
                  [] ->  [lists:nth(1, Monitor), dict:append(Name, {{LatX, LatY}, {{Year, Month, Day},{Hour, Minute, Second}}, Type, Value}, lists:nth(2, Monitor))];
                  _ -> io:format("This record has already been added.")
                end
      end
  end.

%Usuwa pomiar z monitora. Sprawdza, czy stacja o podanej nazwie istnieje.
%Jeśli tak - sprawdza, czy stacja posiada jakiekolwiek wpisy.
%Jeśli wpis o podanych na wejściu parametrach zostanie odnaleziony w monitorze, funkcja usuwa go.
removeValue(Name, {{Year, Month, Day},{Hour, Minute, Second}}, Type, Monitor) ->
  case maps:find(Name, lists:nth(1, Monitor)) of
    error -> io:format("There is no station with providen name");
    {ok, {LatX, LatY}} ->
      Fun = fun(List) ->
        [{LX, LY, Y, Mon, D, H, Min, S, T, V} || {{LX, LY}, {{Y, Mon, D},{H, Min, S}}, T, V} <- List, {LX, LY, Y, Mon, D, H, Min, S, T} /= {LatX, LatY, Year, Month, Day, Hour, Minute, Second, Type}] end,
      case dict:is_key(Name, lists:nth(2, Monitor)) of
        false -> io:format("There are no records linked with this station");
        true -> [lists:nth(1, Monitor), dict:update(Name, Fun, lists:nth(2, Monitor))]
      end
  end.

%Zwraca wartość odczytu o podanych parametrach. W tym celu sprawdza, czy stacja o podanej nazwie istnieje i posiada jakiekolwiek pomiary w monitorze.
%Jeśli wpis o podanych parametrach zostanie znaleziony, zostaje zwrócona wartość pomiaru.
getOneValue(Name, {{Year, Month, Day},{Hour, Minute, Second}}, Type, Monitor) ->
  case maps:find(Name, lists:nth(1, Monitor)) of
    error -> io:format("There is no station with providen name");
    {ok, {LatX, LatY}} ->
      case dict:is_key(Name, lists:nth(2, Monitor)) of
        false -> io:format("There are no records linked with this station");
        true -> case [{LX, LY, Y, Mon, D, H, Min, S, T, V} || {{LX, LY}, {{Y, Mon, D},{H, Min, S}}, T, V} <- dict:fetch(Name, lists:nth(2, Monitor)),
          {LX, LY, Y, Mon, D, H, Min, S, T} == {LatX, LatY, Year, Month, Day, Hour, Minute, Second, Type}] of
                  [] -> io:format("There is no record with provided data");
                  [{LX, LY, Y, Mon, D, H, Min, S, T, V}] -> V
                end
      end
  end.

%Funkcja pomocnicza - oblicza średnią wartość elementów w liście.
getMean([], Sum, Count) -> Sum/Count;
getMean([Head| Tail], Sum, Count) -> getMean(Tail, Sum+Head, Count+1).

%Funkcja pomocnicza - oblicza średnią wartość wartości wpisów monitora, które mają konkretny typ.
getTypeMean(Type, List)->
  Values = [V || {{LX, LY}, {{Y, Mon, D},{H, Min, S}}, T, V} <- List, T == Type],
  getMean(Values, 0, 0).

%Oblicza średnią wartość pomiarów stacji.
getStationMean(Name, Type, Monitor) ->
  List = dict:fetch(Name, lists:nth(2, Monitor)),
  if
    ((Type == "PM10") or (Type == "PM2.5") or (Type == "temp")) -> getTypeMean(Type, List);
    true -> io:format("Uncorrect type.")
  end.

%Zwraca listę zawierającą wszystkie wpisy z wszystkich stacji monitora.
getEntrySet(Monitor)->
  Dict = lists:nth(2, Monitor),
  Keys = dict:fetch_keys(Dict),
  EntrySet = [],
  Lists = [EntrySet ++ dict:fetch(X, Dict) || X <- Keys],
  lists:merge(Lists).

%Zwraca średnią wartość pomiarów podanego typu, dokonanych w określonym dniu.
getDailyMean({Year, Month, Day}, Type, Monitor) ->
  Values = [V || {{LX, LY}, {{Y, Mon, D},{H, Min, S}}, T, V} <- pollution:getEntrySet(Monitor),
    (T == Type) and (Day == D) and (Month == Mon) and (Year == Y)],
  getMean(Values, 0, 0).

%Zwraca średnią wartość pomiarów podanego typu, dokonanych w określonym dniu i godzinie.
getHourlyMean({{Year, Month, Day},{Hour, Minute, Second}}, Type, Monitor) ->
  Values = [V || {{LX, LY}, {{Y, Mon, D},{H, Min, S}}, T, V} <- pollution:getEntrySet(Monitor),
    (T == Type) and (Day == D) and (Month == Mon) and (Year == Y) and (Hour == H)],
  getMean(Values, 0, 0).

%Zwraca liczbę wpisów dla podanej stacji i podanego typu pomiaru, które przekroczyły podaną wartość.
getStationCountAboveValue(Name, Type, Value, Monitor) ->
  Dict = dict:fetch(Name, lists:nth(2, Monitor)),
  Values = [V || {{LX, LY}, {{Y, Mon, D},{H, Min, S}}, T, V} <- Dict, (V > Value), (T == Type)],
  lists:flatlength(Values).