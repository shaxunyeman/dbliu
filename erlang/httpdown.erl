-module(httpdown).
-export([start/0]).

%%
%% You must launch inets:start() and httpdown_evnet:start() 
%% before running httpdown:start()
%%
start() ->
	start("http://epush.petrochina.com.cn:8791/cnpcmail/mailclient_update/cnpcmail.dat").

start(Uri) ->
	%Uri = "http://epush.petrochina.com.cn:8791/cnpcmail/mailclient_update/cnpcmail.dat",
	{ok,{http,_,_,_,Path,_}} = http_uri:parse(Uri),
	Pos = string:rstr(Path,"/"),
	PackageName = string:substr(Path,Pos + 1),
	{ok,Cwd} = file:get_cwd(),
	CurrentPath = string:concat(Cwd,"/"),
	FileName = string:concat(CurrentPath,PackageName),
	case file:open(FileName,[read,write,binary]) of
		{ok,IoDevice} ->
			case httpc:request(get,{Uri,[]},
						[{timeout,5000},{connect_timeout, 5000}],
						[{sync, false},{stream,{self,once}}]) of
			{ok,RequestId} ->
				receiver_once(RequestId,IoDevice),
				{ok,RequestId};
			{error,Reason} ->
				io:format("request ~p failed.[~p] ~n",[Uri,Reason]),
				{error,Reason}
			end;
		{error, Reason} ->
			io:format("open ~p failed.[~p]~n",[FileName,Reason]),
			{error, Reason}
	end.
	
parse_header(Arg) ->
	case Arg of
		{stream_start,RequestId,Headers} ->
			httpdown_events:httpdown_start(RequestId,Headers);
		{stream_end,RequestId,Headers} ->
			httpdown_events:httpdown_end(RequestId,Headers)
	end.

receiver_once(RequestId,IoDevice) ->
	receive
		{http,{RequestId,stream_start, Headers,Pid}} ->
			parse_header({stream_start,RequestId,Headers}),
			%io:format("~p~n",[Headers]),
			%file:write(IoDevice,Headers),
			httpc:stream_next(Pid),
			receiver_left_data(RequestId,Pid,IoDevice);
		{http,{RequestId,{error,timeout}}} ->
			io:format("peformance timeout~n")
	after
		5000 ->
			timeout
	end.

receiver_left_data(RequestId,Pid,IoDevice) ->
	receive
		{http, {RequestId, stream, BinBodyPart}} ->
			%io:format("BodyPart -> ~p~n",[size(BinBodyPart)]),
			file:write(IoDevice,BinBodyPart),
			httpdown_events:progress(RequestId,size(BinBodyPart)),
			httpc:stream_next(Pid),
			receiver_left_data(RequestId,Pid,IoDevice);
		{http, {RequestId, stream_end,Headers}} ->
			parse_header({stream_end,RequestId,Headers}),
			%file:write(IoDevice,Headers),
			file:close(IoDevice);
		{http,{RequestId,{error,timeout}}} ->
			io:format("peformance timeout~n")
	end.
	
