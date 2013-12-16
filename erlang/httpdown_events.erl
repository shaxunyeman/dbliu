-module(httpdown_events).
-export([event_file/3,event_streamstart/3,event_streamend/3,event_stream/3]).

%% macro
-define(RATE,50).

event_file(_RequestId,FileName,Status) ->
	%io:format("[~p ~p ~n]",[RequestId,FileName]),
	[{filename,FileName}|Status].


event_streamstart(_RequestId,Header,Status) ->
	%io:format("[~p] ~p ~n",[_RequestId,Header]),
	FileName = parse_status_param(Status,filename),
	Len = content_length(Header),
	io:format("begin downloading ~p[~.2f MB] ~n",[FileName,Len/1024/1024]),
	[{'content-length',Len},{rate,Len div ?RATE},{timestamp,now()}|Status].

event_streamend(_RequestId,_Header,Status) ->
	%Total = parse_status_param(Status,'content-length'),
	%CurSize = parse_status_param(Status,cursize),
	Status.

event_stream(_RequestId,BinBodyPart,Status) ->
	%io:format("."),
	CurSize = parse_status_param(Status,cursize), 
	if 
		CurSize =:= none ->
			[{cursize,size(BinBodyPart)}|Status];
		is_integer(CurSize) ->
			% update value of the cursize
			TempStatus = lists:delete({cursize,CurSize},Status),
			NewSize = CurSize + size(BinBodyPart),
			NewStatus = [{cursize,NewSize}|TempStatus],
			prograss(NewStatus),
			NewStatus
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


prograss(Status) ->
  Total = parse_status_param(Status,'content-length'),
  CurSize = parse_status_param(Status,cursize),
  Rate =  parse_status_param(Status,rate),

  %% evalute download rate
  PreTime = parse_status_param(Status,timestamp),
  Diff = timer:now_diff(now(),PreTime),
  DiffSeconds = Diff /(1000*1000),
  DownLoadRate = (CurSize / DiffSeconds),
  DownRate = DownLoadRate/1024/1024,

  if
	Total =:= CurSize ->
	  %% go back head line
	  io:format("\r"),
	  io:format("~50c",">"),
	  io:format(" [~.2f MB/s ~.2f MB]~n",[DownRate,CurSize/1024/1024]);
	Total > CurSize ->
	  R = CurSize div Rate,
	  if 
		R =:= 0 ->
		  void;
		R > 0 ->
		  %% bad effect
		  Str = "\~" ++ integer_to_list(R) ++ "c",

		  io:format(Str,">"),
		  io:format(" [~.2f MB/s ~.2f MB]\r",[DownRate,CurSize/1024/1024])
	  end
  end.



