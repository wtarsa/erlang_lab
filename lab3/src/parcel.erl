%%%-------------------------------------------------------------------
%%% @author wiktor
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Apr 2020 22:45
%%%-------------------------------------------------------------------
-module(parcel).
-author("wiktor").

%% API
-export([init/0, createList/1, findMyParcelLocker/2, findLockersParallel/2, oneProcessForEveryone/3, getLockerForPerson/2, getNearestLockers/2, findLockerForEachPerson/2, findLockersTwoProcesses/2 ]).

%Funkcja pomocnicza - tworzy listę punktów 2d o długości podanej jako argument
createList(N) ->
  [{rand:uniform(10001)-1, rand:uniform(10001)-1} || X <- lists:seq(1, N)].

%Funkcja która tworzy dwie listy punktów - lokalizację ludzi i paczkomatów.
%Następnie znajduje nabliższy paczkomat dla każdej osoby na trzy sposoby i mierzy czas wykonania
%każdego sposobu. Na końcu funkcja wypisuje zmierzone czasy. 
init()->
  People = parcel:createList(10000),
  Lockers = parcel:createList(1000),
  {Time1, _} = timer:tc(?MODULE, findLockerForEachPerson, [People, Lockers]),
  {Time2, _} = timer:tc(?MODULE, findLockersParallel,[People, Lockers]),
  {Time3, _} = timer:tc(?MODULE, findLockersTwoProcesses, [People, Lockers]),
  io:format("Czas wykonania sekwencyjnie: ~p~n", [Time1]),
  io:format("Czas wykonania bardzo rownolegle: ~p~n", [Time2]),
  io:format("Czas wykonania rownolegle: ~p~n", [Time3]).

%Funkcja pomocnicza - wyznacza kwadrat odległości pomiędzy dwoma punktami. 
%Funkcja wyznacza kwadarat odległości, a nie samą odległość, ponieważ w zadaniu mamy znaleźć najbliższy paczkomat,
% nie odległość do najbliższego paczkomatu.
calculateDistance(A, B) ->
  ((element(1, A)-element(1, B))*(element(1, A)-element(1, B)))+
    ((element(2, A)-element(2, B))*(element(2, A)-element(2, B))).

%Funkcja pomocnicza - wyszukuje w liście numer punktu o podanej wartości
findElementWithMinDistance(Min, Distance) ->
  lists:nth(1, [X || X <- lists:seq(1, lists:flatlength(Distance)), Min == lists:nth(X, Distance)]).

%Funkcja wyznaczająca współrzędne najbliższego paczkomatu dla osoby o lokalizacji PersonLocation
findMyParcelLocker(PersonLocation, LockerLocations) ->
  Distance = [calculateDistance(X, PersonLocation) || X <- LockerLocations],
  lists:nth(findElementWithMinDistance(lists:min(Distance), Distance), LockerLocations).

%Funkcja pomocnicza - znajduje najbliższy paczkomat dla każdej osoby używając findMyParcelLocker i list comprehensions
findLockerForEachPerson(People, Lockers)->
  [{lists:nth(X, People), findMyParcelLocker(lists:nth(X, People), Lockers)} || X<-lists:seq(1,lists:flatlength(People))].

%% bardzo rownolegle

%Funkcja pomocnicza - wyznacza lokalizację najbliższego paczkomatu i odsyła ją do procesu macierzystego
getLockerForPerson(PersonLocation, LockerLocations)->
  receive
    {From, take} -> From ! {PersonLocation, findMyParcelLocker(PersonLocation, LockerLocations)}
  end.

%Funkcja pomocnicza - tworzy osobny proces który wylicza lokalizację najbliższego paczkomatu dla każdej osoby.
%Zapisuje PID procesu w liście Acc którą zwraca po utworzeniu wszystkich procesów.
oneProcessForEveryone([], _, Acc) -> Acc;
oneProcessForEveryone([Head|Tail], Lockers, Acc) ->
  PID = spawn(?MODULE, getLockerForPerson, [Head, Lockers]),
  oneProcessForEveryone(Tail, Lockers, [PID] ++ Acc).

%Funkcja pomocnicza - wysyła zapytanie do każdego procesu z listy będącej pierwszym argumentem funkcji.
%Zapisuje wyniki do listy Acc, która zwraca po odebraniu wyników od każdego procesu. 
receiveResults([], Acc) -> Acc;
receiveResults([Head|Tail], Acc)->
  Head ! {self(), take},
  receive
    Result -> receiveResults(Tail, [Result] ++ Acc)
  end.

%Funkcja która wywołuje powyższe dwie funkcje realizując wyszukiwanie najbliższego paczkomatu dla każdej osoby
%poprzez utworzenie osobnego procesu dla każdego człowieka. 
findLockersParallel(People, Lockers) ->
  PIDs = oneProcessForEveryone(People, Lockers, []),
  receiveResults(PIDs, []).

%%jedna funkcja na jeden rdzeń

%Funkcja która tworzy dwa procesy, dzieli listę osób na pół i oblicza współrzędne najbliższego paczkomatu dla każdej osoby.
%Pierwszy proces jest odpowiedzialny za obliczanie współrzędnych pierwszej połowy osób, a drugi - drugiej. 
%Otrzymane wyniki są łączone w jedną listę.
findLockersTwoProcesses(People, Lockers)->
  {First, Second} = lists:split(5000, People),
  PID1 = spawn(?MODULE, getNearestLockers, [First, Lockers]),
  PID2 = spawn(?MODULE, getNearestLockers, [Second, Lockers]),
  [A, B] = receiveResults([PID1, PID2], []),
  A ++ B.

%Funkcja pomocnicza - wyznacza współrzędne najbliższego paczkomatu dla każdej osoby z listy People (używając findMyParcelLocker)
getNearestLockers(People, LockerLocations) ->
  receive
    {From, take} -> From ! [{PersonLocation, findMyParcelLocker(PersonLocation, LockerLocations)} || PersonLocation <- People]
  end.
