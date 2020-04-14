%%%-------------------------------------------------------------------
%%% @author wiktor
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Apr 2020 17:44
%%%-------------------------------------------------------------------
-module(pollution_test).
-author("wiktor").

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

addStationTest() ->
  A = pollution:createMonitor(),
  A1 = pollution:addStation("A", {1, 2}, A),
  A2 = pollution:addStation("B", {2, 3}, A1),
  ?assertEqual(maps:get("A", lists:nth(1, A2)), {1,2}),
  ?assertEqual(maps:get("B", lists:nth(1, A2)), {2,3}).

addValueTest() ->
  A = pollution:createMonitor(),
  A1 = pollution:addStation("A", {1, 2}, A),
  A2 = pollution:addStation("B", {2, 3}, A1),
  A3 = pollution:addValue("A", {{1, 2, 3},{4, 5, 6}}, "PM10", 10, A2),
  A4 = pollution:addValue("B", {{4, 5, 6},{7, 8, 9}}, "PM2.5", 30, A3),
  ?assertEqual(dict:fetch("A", lists:nth(2, A4)), [{{1,2},{{1,2,3},{4,5,6}},"PM10",10}]),
  ?assertEqual(dict:fetch("B", lists:nth(2, A4)), [{{2,3},{{4,5,6},{7,8,9}},"PM2.5",30}]).

removeValueTest() ->
  A = pollution:createMonitor(),
  A1 = pollution:addStation("A", {1, 2}, A),
  A2 = pollution:addStation("B", {2, 3}, A1),
  A3 = pollution:addValue("A", {{1, 2, 3},{4, 5, 6}}, "PM10", 10, A2),
  ?assertEqual(dict:fetch("A", lists:nth(2, A3)), [{{1,2},{{1,2,3},{4,5,6}},"PM10",10}]),
  A4 = pollution:removeValue("A", {{1, 2, 3}, {4, 5, 6}}, "PM10", A3),
  ?assertEqual(dict:fetch("A", lists:nth(2, A4)), []).

getOneValueTest() ->
  A = pollution:createMonitor(),
  A1 = pollution:addStation("A", {1, 2}, A),
  A2 = pollution:addStation("B", {2, 3}, A1),
  A3 = pollution:addValue("A", {{1, 2, 3},{4, 5, 6}}, "PM10", 10, A2),
  A4 = pollution:addValue("B", {{4, 5, 6},{7, 8, 9}}, "PM2.5", 30, A3),
  ?assertEqual(pollution:getOneValue("A", {{1, 2, 3},{4, 5, 6}}, "PM10", A4), 10),
  ?assertEqual(pollution:getOneValue("B", {{4, 5, 6},{7, 8, 9}}, "PM2.5", A4), 30).

getStationMeanTest() ->
  A = pollution:createMonitor(),
  A1 = pollution:addStation("A", {1, 2}, A),
  A2 = pollution:addValue("A", {{1, 2, 3},{4, 5, 6}}, "PM10", 10, A1),
  A3 = pollution:addValue("A", {{1, 2, 3},{5, 6, 7}}, "PM10", 20, A2),
  A4 = pollution:addValue("A", {{1, 2, 3},{6, 7, 8}}, "PM10", 60, A3),
  ?assertEqual(pollution:getStationMean("A", "PM10", A4), 30.0).

getDailyMeanTest() ->
  A = pollution:createMonitor(),
  A1 = pollution:addStation("A", {1, 2}, A),
  A2 = pollution:addValue("A", {{1, 2, 3},{4, 5, 6}}, "PM10", 10, A1),
  A3 = pollution:addValue("A", {{1, 2, 3},{5, 6, 7}}, "PM10", 20, A2),
  A4 = pollution:addStation("B", {2, 3}, A3),
  A5 = pollution:addValue("B", {{1, 2, 3},{7, 8, 9}}, "PM10", 30, A4),
  ?assertEqual(pollution:getDailyMean({1, 2, 3}, "PM10", A5), 20.0).

getHourlyMeanTest() ->
  A = pollution:createMonitor(),
  A1 = pollution:addStation("A", {1, 2}, A),
  A2 = pollution:addValue("A", {{1, 2, 3},{3, 5, 6}}, "PM10", 10, A1),
  A3 = pollution:addValue("A", {{1, 2, 3},{3, 6, 7}}, "PM10", 20, A2),
  A4 = pollution:addStation("B", {2, 3}, A3),
  A5 = pollution:addValue("B", {{1, 2, 3},{3, 8, 9}}, "PM10", 30, A4),
  ?assertEqual(pollution:getHourlyMean({{1, 2, 3},{3, 0, 0}}, "PM10", A5), 20.0).

getStationCountAboveValueTest()->
  A = pollution:createMonitor(),
  A1 = pollution:addStation("A", {1, 2}, A),
  A2 = pollution:addValue("A", {{1, 2, 3},{4, 5, 6}}, "PM10", 10, A1),
  A3 = pollution:addValue("A", {{1, 2, 3},{5, 6, 7}}, "PM10", 20, A2),
  A4 = pollution:addValue("A", {{1, 2, 3},{6, 7, 8}}, "PM10", 60, A3),
  ?assertEqual(pollution:getStationCountAboveValue("A", "PM10", 15, A4), 2).