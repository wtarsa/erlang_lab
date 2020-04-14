%%%-------------------------------------------------------------------
%%% @author wiktor
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Apr 2020 20:35
%%%-------------------------------------------------------------------
-module(pollution_server_test).
-author("wiktor").

%% API
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

runTests() ->
  startTest(),
  addStationTest(),
  addValueTest(),
  removeValueTest(),
  getOneValueTest(),
  getStationMeanTest(),
  getDailyMeanTest(),
  getHourlyMeanTest(),
  getStationCountAboveValueTest(),
  stopTest().

startTest() ->
  pollution_server:start(),
  ?assert(lists:member(monitor, registered())).

addStationTest() ->
  ?assertEqual(
    maps:get("stacja", lists:nth(1, pollution_server:addStation("stacja", {1,1}))),
    {1,1}),
  ?assertEqual(
    maps:get("stacja2", lists:nth(1, pollution_server:addStation("stacja2", {1,2}))),
    {1,2}).

addValueTest() ->
  ?assertEqual(
    dict:fetch("stacja", lists:nth(2, pollution_server:addValue("stacja", {{2020, 4, 14},{21, 36, 59}}, "PM10", 10))),
    [{{1,1},{{2020, 4, 14},{21, 36, 59}}, "PM10", 10}]),
  ?assertEqual(
    dict:fetch("stacja2", lists:nth(2, pollution_server:addValue("stacja2", {{2020, 4, 14},{21, 38, 0}}, "PM10", 10))),
    [{{1,2},{{2020, 4, 14},{21, 38, 00}}, "PM10", 10}]),
  ?assertEqual(
    dict:fetch("stacja", lists:nth(2, pollution_server:addValue("stacja", {{2020, 4, 14},{21, 39, 34}}, "PM10", 20))),
    [{{1,1},{{2020, 4, 14},{21, 36, 59}}, "PM10", 10},{{1,1},{{2020, 4, 14},{21, 39, 34}}, "PM10", 20}]),
  ?assertEqual(
    dict:fetch("stacja", lists:nth(2, pollution_server:addValue("stacja", {{2020, 4, 14},{22, 1, 4}}, "PM10", 60))),
    [{{1,1},{{2020, 4, 14},{21, 36, 59}}, "PM10", 10},{{1,1},{{2020, 4, 14},{21, 39, 34}}, "PM10", 20},
      {{1,1},{{2020, 4, 14},{22, 1, 4}}, "PM10", 60}]).


removeValueTest() ->
  ?assertEqual(
    dict:fetch("stacja2", lists:nth(2, pollution_server:removeValue("stacja2", {{2020, 4, 14},{21, 38, 0}}, "PM10"))),
    []).

getOneValueTest() ->
  ?assertEqual(
    pollution_server:getOneValue("stacja", {{2020, 4, 14}, {21, 36, 59}}, "PM10"),
    10).

getStationMeanTest() ->
  ?assertEqual(
    pollution_server:getStationMean("stacja", "PM10"),
    30.0).

getDailyMeanTest() ->
  ?assertEqual(
    pollution_server:getDailyMean({2020, 4, 14}, "PM10"),
    30.0).

getHourlyMeanTest() ->
  ?assertEqual(
    pollution_server:getHourlyMean({{2020, 4, 14}, {21, 0, 0}}, "PM10"),
    15.0).

getStationCountAboveValueTest() ->
  ?assertEqual(
    pollution_server:getStationCountAboveValue("stacja", "PM10", 19),
    2).

stopTest() ->
  pollution_server:stop(),
  ?assert(not lists:member(monitor, registered())).
