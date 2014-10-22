-module(client).
-author("shaxunyeman@gmail.com").

-export([fork/1]).
-export([connect/1]).

-define(HOST,"123.57.39.114").
-define(PORT,50000).

fork(N) when N > 0 ->
	spawn(fun() -> connect({"123.57.39.114", 50000}) end),
	fork(N - 1);
fork(0) ->
	ok.

connect({Host,Port}) ->
	{ok,Sock} = gen_tcp:connect(Host,Port,[binary,{packet,0},{active,false}]),
	%io:format("~p ~n", [Sock]),
	loop(Sock).
	
loop(Sock) ->
	Message = "hello,world",
	gen_tcp:send(Sock,Message),
	loop(Sock).
