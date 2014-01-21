-module(socket_dist).
-export([start/0,store/2,lookup/1]).


start() ->
	register(kvs,spawn(fun() -> loop() end)).

store(Key,Value) ->
	rcp({store,Key,Value}).
	
lookup(Key) ->
	rcp({lookup,Key}).
	
rcp(Request) ->
	kvs!{self(),Request},
	receive
		{kvs,Response} ->
			Response
	end.
	
loop() ->
	receive
		{From,{store,Key,Value}} ->
			put(Key,{ok,Value}),
			From!{kvs,true},
			loop();
		{From,{lookup,Key}} ->
			From!{kvs,get(Key)},
			loop()
	end.
