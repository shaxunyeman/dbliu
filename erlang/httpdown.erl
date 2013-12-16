-module(httpdown).
-export([start/1]).

%% 
%% You must launch inets:start() and httpdown_evnet:start() 
%% before running httpdown:start()
%%
start(EventsMod) ->
	%start("http://10.27.35.73/tpl/cnpcmail/package/cnpcmail_1.0.0.7.dat").
	start("http://dldir1.qq.com/invc/qqpinyin/QQPinyin_Setup_4.6.2044.400.exe",EventsMod).

start(Uri,EventsMod) ->
	%Uri = "http://epush.petrochina.com.cn:8791/cnpcmail/mailclient_update/cnpcmail.dat",
	{ok,{http,_,_,_,Path,_}} = http_uri:parse(Uri),
	Pos = string:rstr(Path,"/"),
	PackageName = string:substr(Path,Pos + 1),
	case httpc:request(get,{Uri,[]},
			[{timeout,infinity},{connect_timeout, 5000}],
			[{sync, false},{stream,{self,once}}]) of
		{ok,RequestId} ->
			Status = EventsMod:event_file(RequestId,PackageName,[]),
			receiver_once(RequestId,EventsMod,Status),
			{ok,RequestId};
		{error,Reason} ->
			io:format("request ~p failed.[~p] ~n",[Uri,Reason]),
			{error,Reason}
	end.
	
parse_header(Arg,EventsMod,Status) ->
	case Arg of
		{stream_start,RequestId,Headers} ->
			EventsMod:event_streamstart(RequestId,Headers,Status);
		{stream_end,RequestId,Headers} ->
			EventsMod:event_streamend(RequestId,Headers,Status)
	end.

receiver_once(RequestId,EventsMod,Status) ->
	receive
		{http,{RequestId,stream_start, Headers,Pid}} ->
			NewStatus = parse_header({stream_start,RequestId,Headers},EventsMod,Status),
			%io:format("~p~n",[Headers]),
			%file:write(IoDevice,Headers),
			%io:format("[~p ~p] stream_start ~p ~n",[RequestId,Pid,Headers]),
			httpc:stream_next(Pid),
			receiver_left_data(RequestId,Pid,EventsMod,NewStatus);
		{http,{RequestId,{error,timeout}}} ->
			io:format("[~p] peformance timeout~n",[?LINE])
	after
		5000 ->
			timeout
	end.

receiver_left_data(RequestId,Pid,EventsMod,Status) ->
	receive
		{http, {RequestId, stream, BinBodyPart}} ->
			NewStatus = EventsMod:event_stream(RequestId,BinBodyPart,Status),
			httpc:stream_next(Pid),
			receiver_left_data(RequestId,Pid,EventsMod,NewStatus);
		{http, {RequestId, stream_end,Headers}} ->
			parse_header({stream_end,RequestId,Headers},EventsMod,Status);
		{http,{RequestId,{error,timeout}}} ->
			io:format("[~p] peformance timeout~n",[?LINE])
	end.
	
