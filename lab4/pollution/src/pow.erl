-module(pow).
-behaviour(gen_server).
%% API
-export([start_link/0, step/0, read/0, close/0, crash/0, update/1]).
-export([init/1,handle_call/3,handle_cast/2,terminate/2, handle_info/2]).
 
%% START %%
start_link()   -> gen_server:start_link({local,?MODULE},?MODULE,2,[]).
init(N)        -> {ok,N}.
 
%% INTERFEJS KLIENT -> SERWER %%
step()      -> gen_server:cast(?MODULE,step).
read()      -> gen_server:call(?MODULE,read).
close()     -> gen_server:call(?MODULE,terminate).
crash()     -> gen_server:cast(?MODULE,crash).
update(N)   -> gen_server:call(?MODULE,{update, N}).

%% OBSŁUGA WIADOMOŚCI %%
handle_cast(step, N) -> {noreply, N*N};
handle_cast(crash, N) -> no:exist(), {noreply, N}.
 
handle_call(read,_From, N)      -> {reply, N, N};
handle_call(terminate,_From,N) -> {stop, normal, ok, N};
handle_call({update,NewVal},_From,N) -> {reply, NewVal, NewVal}.

handle_info(_Info, State) ->
	if
		_Info == czesc ->
			io:format("uszanowanko ~n"),
			{noreply, State};
		true -> {noreply, State}
	end.

terminate(normal, N) -> io:format("The number is: ~B~nBye.~n",[N]), ok.

