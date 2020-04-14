-module(pollution_server).
-author("wiktor").

-export([
	start/0,
	init/0,
	stop/0,
	addStation/2,
	addValue/4,
	removeValue/3,
	getOneValue/3,
	getStationMean/2,
	getDailyMean/2,
	getHourlyMean/2,
	getStationCountAboveValue/3
]).

start() ->
	register(monitor, spawn_link(?MODULE, init, [])).

stop() ->
	takeAction(stop, []).

addStation(Name, {LatX, LatY}) ->
	takeAction(addStation, [Name, {LatX, LatY}]).

addValue(Name, {{Year, Month, Day},{Hour, Minute, Second}}, Type, Value) ->
	takeAction(addValue, [Name, {{Year, Month, Day},{Hour, Minute, Second}}, Type, Value]).

removeValue(Name, {{Year, Month, Day},{Hour, Minute, Second}}, Type) ->
	takeAction(removeValue, [Name, {{Year, Month, Day},{Hour, Minute, Second}}, Type]).

getOneValue(Name, {{Year, Month, Day},{Hour, Minute, Second}}, Type) ->
	takeAction(getOneValue, [Name, {{Year, Month, Day},{Hour, Minute, Second}}, Type]).

getStationMean(Name, Type) ->
	takeAction(getStationMean, [Name, Type]).

getDailyMean({Year, Month, Day}, Type) ->
	takeAction(getDailyMean, [{Year, Month, Day}, Type]).

getHourlyMean({{Year, Month, Day},{Hour, Minute, Second}}, Type) ->
	takeAction(getHourlyMean, [{{Year, Month, Day},{Hour, Minute, Second}}, Type]).

getStationCountAboveValue(Name, Type, Value) ->
	takeAction(getStationCountAboveValue, [Name, Type, Value]).


init() ->
	Monitor = pollution:createMonitor(),
	loop(Monitor).

takeAction(Action, Arguments) when is_list(Arguments) ->
	monitor ! {Action, self(), Arguments},
	receive
		{monitor, Monitor} -> Monitor;
		{value, Value} -> Value
	end.


loop(Monitor) ->
	receive
		{addStation, Pid, [Name, {LatX, LatY}]} ->
			UpdatedMonitor = pollution:addStation(Name, {LatX, LatY}, Monitor),
			Pid ! {monitor, UpdatedMonitor},
			loop(UpdatedMonitor);

		{addValue, Pid, [Name, {{Year, Month, Day},{Hour, Minute, Second}}, Type, Value]} ->
			UpdatedMonitor = pollution:addValue(Name, {{Year, Month, Day},{Hour, Minute, Second}}, Type, Value, Monitor),
			Pid ! {monitor, UpdatedMonitor},
			loop(UpdatedMonitor);

		{removeValue, Pid, [Name, {{Year, Month, Day},{Hour, Minute, Second}}, Type]} ->
			UpdatedMonitor = pollution:removeValue(Name, {{Year, Month, Day},{Hour, Minute, Second}}, Type, Monitor),
			Pid ! {monitor, UpdatedMonitor},
			loop(UpdatedMonitor);

		{getOneValue, Pid, [Name, {{Year, Month, Day},{Hour, Minute, Second}}, Type]} ->
			Value = pollution:getOneValue(Name, {{Year, Month, Day},{Hour, Minute, Second}}, Type, Monitor),
			Pid ! {value, Value},
			loop(Monitor);

		{getStationMean, Pid, [Name, Type]} ->
			Value = pollution:getStationMean(Name, Type, Monitor),
			Pid ! {value, Value},
			loop(Monitor);

		{getDailyMean, Pid, [{Year, Month, Day}, Type]} ->
			Value = pollution:getDailyMean({Year, Month, Day}, Type, Monitor),
			Pid ! {value, Value},
			loop(Monitor);

		{getHourlyMean, Pid, [{{Year, Month, Day},{Hour, Minute, Second}}, Type]} ->
			Value = pollution:getHourlyMean({{Year, Month, Day},{Hour, Minute, Second}}, Type, Monitor),
			Pid ! {value, Value},
			loop(Monitor);

		{getStationCountAboveValue, Pid, [Name, Type, Value]} ->
			Val = pollution:getStationCountAboveValue(Name, Type, Value, Monitor),
			Pid ! {value, Val},
			loop(Monitor);

		{stop, Pid, []} ->
			Pid ! {monitor, Monitor}

	end.
