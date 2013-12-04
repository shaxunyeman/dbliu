-module(httpdown_events).
-export([start/0,stop/0]).
-export([httpdown_start/2,httpdown_end/2,progress/2]).

start() ->
	Pid = spawn(fun() -> events_loop() end),
	register(httpdown_events,Pid).
	
stop() ->
	unregister(httpdown_events).

postmessage(Msg) ->
	case whereis(httpdown_events) of
		undefined ->
			io:foramt("unregister httpdown_events~n");
		_Pid ->
			httpdown_events!Msg
	end.	
	
progress(RequestId,CurDownSize) ->
	postmessage({progress,RequestId,CurDownSize}).
	
httpdown_start(RequestId,Headers) ->
	postmessage({httpdown_start,RequestId,Headers}).
	
httpdown_end(RequestId,Headers) ->
	postmessage({httpdown_end,RequestId,Headers}).

events_loop() ->
	receive
		{progress,RequestId,CurDownSize} ->
			io:format("[~p] Cur = ~p~n",[RequestId,CurDownSize]);
		{httpdown_start,RequestId,Start_Headers} ->
			io:format("[~p] Headers = ~p~n",[RequestId,Start_Headers]);
		{httpdown_end,RequestId,End_Headers} ->
			io:format("[~p] Headers = ~p~n",[RequestId,End_Headers])
	end,
	events_loop().
