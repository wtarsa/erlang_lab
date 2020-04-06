%%%-------------------------------------------------------------------
%%% @author wiktor
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Apr 2020 17:37
%%%-------------------------------------------------------------------
-module(pingpong).
-author("wiktor").

%% API
-export([start/0, stop/0, play/1, ping/1, pong/0]).

start() ->
  register(pong, spawn(pingpong, pong, [])),
  register(ping, spawn(pingpong, ping, [1])).


stop() ->
  pong ! stop,
  ping ! stop.


ping(X) ->
  receive
    {0} -> ping(X);
    {N} ->
      io:format("Ping received pong ~p~n", [X]),
      timer:sleep(500),
      pong ! {N-1},
      ping(X + 1),
      timer:kill_after(20000);
    stop -> io:format("Ping stop~n", [])
  end.

pong() ->
  receive
    {0} -> pong();
    {N} ->
      io:format("Pong received ping~n", []),
      timer:sleep(500),
      ping ! {N-1},
      pong(),
      timer:kill_after(20000);
    stop -> io:format("Pong stop~n", [])
  end.


play(N) ->
  pingpong:start(),
  ping ! {N}.