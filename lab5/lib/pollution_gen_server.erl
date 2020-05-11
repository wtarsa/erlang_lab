%%%-------------------------------------------------------------------
%%% @author wiktor
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. May 2020 11:07
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-behaviour(gen_server).
-author("wiktor").


%% API
-export([init/1, handle_call/3, handle_cast/2]).
-export([crash/0, start_link/0, addStation/2, addValue/4, removeValue/3, getOneValue/3,
  getMonitor/0, getStationMean/2, getDailyMean/2, getHourlyMean/2, getStationCountAboveValue/3]).

%% START %%
start_link()   -> gen_server:start_link({local,?MODULE},?MODULE,pollution:createMonitor(),[]).
init(Monitor)        -> {ok,Monitor}.

%% INTERFEJS KLIENT -> SERWER %%
getMonitor()->
  gen_server:call(?MODULE, getMonitor).

addStation(Name, {LatX, LatY}) ->
  gen_server:call(?MODULE, {addStation, Name, {LatX, LatY}}).

addValue(Name, {{Year, Month, Day},{Hour, Minute, Second}}, Type, Value) ->
  gen_server:call(?MODULE, {addValue, Name, {{Year, Month, Day},{Hour, Minute, Second}}, Type, Value}).

removeValue(Name, {{Year, Month, Day},{Hour, Minute, Second}}, Type) ->
  gen_server:call(?MODULE, {removeValue, Name, {{Year, Month, Day},{Hour, Minute, Second}}, Type}).

getOneValue(Name, {{Year, Month, Day},{Hour, Minute, Second}}, Type) ->
  gen_server:call(?MODULE, {getOneValue, Name, {{Year, Month, Day},{Hour, Minute, Second}}, Type}).

getStationMean(Name, Type) ->
  gen_server:call(?MODULE, {getStationMean, Name, Type}).

getDailyMean({Year, Month, Day}, Type) ->
  gen_server:call(?MODULE, {getDailyMean, {Year, Month, Day}, Type}).

getHourlyMean({{Year, Month, Day},{Hour, Minute, Second}}, Type) ->
  gen_server:call(?MODULE, {getHourlyMean, {{Year, Month, Day},{Hour, Minute, Second}}, Type}).

getStationCountAboveValue(Name, Type, Value) ->
  gen_server:call(?MODULE, {getStationCountAboveValue, Name, Type, Value}).

%% OBSŁUGA WIADOMOŚCI %%
handle_call(getMonitor, From, Monitor) -> {reply, Monitor, Monitor};

handle_call({addStation, Name, {LatX, LatY}}, From, Monitor) ->
  NewMonitor = pollution:addStation(Name, {LatX, LatY}, Monitor),
  {reply, NewMonitor, NewMonitor};

handle_call({addValue, Name, {{Year, Month, Day},{Hour, Minute, Second}}, Type, Value}, From, Monitor) ->
  NewMonitor = pollution:addValue(Name, {{Year, Month, Day},{Hour, Minute, Second}}, Type, Value, Monitor),
  {reply, ok, NewMonitor};

handle_call({removeValue, Name, {{Year, Month, Day},{Hour, Minute, Second}}, Type}, From, Monitor) ->
  NewMonitor = pollution:removeValue(Name, {{Year, Month, Day},{Hour, Minute, Second}}, Type, Monitor),
  {reply, NewMonitor, NewMonitor};

handle_call({getOneValue, Name, {{Year, Month, Day},{Hour, Minute, Second}}, Type}, From, Monitor) ->
  Value = pollution:getOneValue(Name, {{Year, Month, Day},{Hour, Minute, Second}}, Type, Monitor),
  {reply, Value, Monitor};

handle_call({getStationMean, Name, Type}, From, Monitor) ->
  Value = pollution:getStationMean(Name, Type, Monitor),
  {reply, Value, Monitor};

handle_call({getDailyMean, {Year, Month, Day}, Type}, From, Monitor) ->
  Value = pollution:getDailyMean({Year, Month, Day}, Type, Monitor),
  {reply, Value, Monitor};

handle_call({getHourlyMean, {{Year, Month, Day},{Hour, Minute, Second}}, Type}, From, Monitor) ->
  Value = pollution:getHourlyMean({{Year, Month, Day},{Hour, Minute, Second}}, Type, Monitor),
  {reply, Value, Monitor};

handle_call({getStationCountAboveValue, Name, Type, Value}, From, Monitor) ->
  Val = pollution:getStationCountAboveValue(Name, Type, Value, Monitor),
  {reply, Val, Monitor}.

handle_cast(crash, State) ->
  1/0,
  {noreply, State}.

crash() -> gen_server:cast(?MODULE, crash).