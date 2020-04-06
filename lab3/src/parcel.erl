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


createList(N) ->
  [{rand:uniform(10001)-1, rand:uniform(10001)-1} || X <- lists:seq(1, N)].

init()->
  People = parcel:createList(10000),
  Lockers = parcel:createList(1000),
  {Time1, _} = timer:tc(?MODULE, findLockerForEachPerson, [People, Lockers]),
  {Time2, _} = timer:tc(?MODULE, findLockersParallel,[People, Lockers]),
  {Time3, _} = timer:tc(?MODULE, findLockersTwoProcesses, [People, Lockers]),
  io:format("Czas wykonania sekwencyjnie: ~p~n", [Time1]),
  io:format("Czas wykonania bardzo rownolegle: ~p~n", [Time2]),
  io:format("Czas wykonania rownolegle: ~p~n", [Time3]).

calculateDistance(A, B) ->
  ((element(1, A)-element(1, B))*(element(1, A)-element(1, B)))+
    ((element(2, A)-element(2, B))*(element(2, A)-element(2, B))).

findElementWithMinDistance(Min, Distance) ->
  lists:nth(1, [X || X <- lists:seq(1, lists:flatlength(Distance)), Min == lists:nth(X, Distance)]).

findMyParcelLocker(PersonLocation, LockerLocations) ->
  Distance = [calculateDistance(X, PersonLocation) || X <- LockerLocations],
  lists:nth(findElementWithMinDistance(lists:min(Distance), Distance), LockerLocations).

findLockerForEachPerson(People, Lockers)->
  [{lists:nth(X, People), findMyParcelLocker(lists:nth(X, People), Lockers)} || X<-lists:seq(1,lists:flatlength(People))].

%% parallel
getLockerForPerson(PersonLocation, LockerLocations)->
  receive
    {From, take} -> From ! {PersonLocation, findMyParcelLocker(PersonLocation, LockerLocations)}
  end.

oneProcessForEveryone([], _, Acc) -> Acc;
oneProcessForEveryone([Head|Tail], Lockers, Acc) ->
  PID = spawn(?MODULE, getLockerForPerson, [Head, Lockers]),
  oneProcessForEveryone(Tail, Lockers, [PID] ++ Acc).

receiveResults([], Acc) -> Acc;
receiveResults([Head|Tail], Acc)->
  Head ! {self(), take},
  receive
    Result -> receiveResults(Tail, [Result] ++ Acc)
  end.

findLockersParallel(People, Lockers) ->
  PIDs = oneProcessForEveryone(People, Lockers, []),
  receiveResults(PIDs, []).

%one function for each thread

findLockersTwoProcesses(People, Lockers)->
  {First, Second} = lists:split(5000, People),
  PID1 = spawn(?MODULE, getNearestLockers, [First, Lockers]),
  PID2 = spawn(?MODULE, getNearestLockers, [Second, Lockers]),
  [A, B] = receiveResults([PID1, PID2], []),
  A ++ B.

getNearestLockers(People, LockerLocations) ->
  receive
    {From, take} -> From ! [{PersonLocation, findMyParcelLocker(PersonLocation, LockerLocations)} || PersonLocation <- People]
  end.

