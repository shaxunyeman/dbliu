-module(httpdown_events).
-export([event_file/3,event_streamstart/3,event_streamend/3,event_stream/3]).

event_file(_RequestId,FileName,Status) ->
	%io:format("[~p ~p ~n]",[RequestId,FileName]),
	[{filename,FileName}|Status].


event_streamstart(_RequestId,Header,Status) ->
	%io:format("[~p] ~p ~n",[_RequestId,Header]),
	FileName = parse_status_param(Status,filename),
	io:format("begin downloading ~p ~n",[FileName]),
	Len = content_length(Header),
	[{'content-length',Len}|Status].

event_streamend(_RequestId,_Header,Status) ->
	Total = parse_status_param(Status,'content-length'),
	CurSize = parse_status_param(Status,cursize),
	io:format("~nfinished download [~p/~p]~n",[Total,CurSize]),
	Status.

event_stream(_RequestId,BinBodyPart,Status) ->
	io:format("."),
	CurSize = parse_status_param(Status,cursize), 
	if 
		CurSize =:= none ->
			[{cursize,size(BinBodyPart)}|Status];
		is_integer(CurSize) ->
			% update value of the cursize
			TempStatus = lists:delete({cursize,CurSize},Status),
			NewSize = CurSize + size(BinBodyPart),
			[{cursize,NewSize}|TempStatus]
	end.

content_length([H|T]) ->
	case H of
		{"content-length",Len} ->
			list_to_integer(Len);
		_Any ->
			content_length(T)
	end.

parse_status_param([H|Msg],Key) ->
	case H of
		{Key,Len} ->
			Len;
		_Any ->
			parse_status_param(Msg,Key)			
	end;
parse_status_param([],_Key) ->
	none.








