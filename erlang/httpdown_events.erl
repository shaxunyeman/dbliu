-module(httpdown_events).
-export([start/0,stop/0]).
-export([httpdown_start/2,httpdown_end/2,progress/2]).
-export([httpdown_file/2,httpdown_stream/2,httpdown_streamend/1]).

start() ->
	Pid = spawn(fun() -> events_loop([]) end),
	register(httpdown_events,Pid).
	
stop() ->
	unregister(httpdown_events).

postmessage(Msg) ->
	case whereis(httpdown_events) of
		undefined ->
			io:format("unregister httpdown_events ~n");
		_Pid ->
			httpdown_events!Msg
	end.	
	
progress(RequestId,CurDownSize) ->
	postmessage({progress,RequestId,CurDownSize}).
	
httpdown_start(RequestId,Headers) ->
	postmessage({httpdown_start,RequestId,Headers}).
	
httpdown_end(RequestId,Headers) ->
	postmessage({httpdown_end,RequestId,Headers}).

httpdown_file(RequestId,FileName) ->
	postmessage({httpdown_file,RequestId,FileName}).

httpdown_stream(RequestId,Bin) ->
	postmessage({httpdown_stream,RequestId,Bin}).

httpdown_streamend(RequestId) ->
	postmessage({httpdown_streamend,RequestId}).

progress_notiy_ui(RequestId,Total,DownSize) ->
	if
		Total == DownSize ->
			io:format("[~p] ~p/~p ~n",[RequestId,DownSize,RequestId]);
		Total > DownSize ->
			io:format("[~p] ~p/~p \w\b",[RequestId,DownSize,RequestId])
	end.

events_loop(MsgList) ->
	receive
		{progress,RequestId,CurDownSize} ->
			%DownSize = parse_loop_param(MsgList,"downsize"),
			%Total = parse_loop_param(MsgList,"content-length"),
			%CurSize = CurDownSize + DownSize,
			%progress_notiy_ui(RequestId,Total,CurSize),
			%NewMsgList = update_loop_param({"downsize",CurSize},MsgList),
			%io:format("~p ~n",[CurDownSize]),
			events_loop(NewMsgList);
		{httpdown_start,RequestId,Start_Headers} ->
			Len = content_length(Start_Headers),
			events_loop([{"content-length",Len}|MsgList]);
		{httpdown_end,RequestId,End_Headers} ->
			io:format("[~p] Headers = ~p~n",[RequestId,End_Headers]),
			events_loop(MsgList);
		{httpdown_file,RequestId,FileName} ->
			io:format("[~p] FileName = ~p~n",[RequestId,FileName]),
			events_loop(MsgList);
		{httpdown_stream,RequestId,Bin} ->
			%io:format("[~p] size = ~p~n",[RequestId,size(Bin)]);
			events_loop(MsgList);
		{httpdown_streamend,RequestId} ->
			io:format("[~p] stream end~n",[RequestId]),
			events_loop(MsgList)
	end.

content_length([H|T]) ->
	case H of
		{"content-length",Len} ->
			list_to_integer(Len);
		_Any ->
			content_length(T)
	end.

parse_loop_param([H|Msg],Key) ->
	case H of
		{Key,Len} ->
			Len;
		_Any ->
			parse_loop_param(Msg,Key)			
	end;
parse_loop_param([],Key) ->
	none.

update_loop_param({Key,CurSize},MsgList) ->
	Fun = fun(X) -> 
				case X of
					{Key,_Any} ->
						none;
					_Any ->
						X
				end
				end,
	NewList = [Fun(X) || X <- MsgList],
	[{Key,CurSize}|NewList].







