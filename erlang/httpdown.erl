-module(httpdown).
-export([start/0]).

%% 
%% You must launch inets:start() and httpdown_evnet:start() 
%% before running httpdown:start()
%%
start() ->
	start("http://10.27.35.73/tpl/cnpcmail/package/cnpcmail_1.0.0.7.dat").

start(Uri) ->
	%Uri = "http://epush.petrochina.com.cn:8791/cnpcmail/mailclient_update/cnpcmail.dat",
	httpdown_events:start(),
	timer:sleep(1000),
	{ok,{http,_,_,_,Path,_}} = http_uri:parse(Uri),
	Pos = string:rstr(Path,"/"),
	PackageName = string:substr(Path,Pos + 1),
	case httpc:request(get,{Uri,[]},
			[{timeout,5000},{connect_timeout, 5000}],
			[{sync, false},{stream,{self,once}}]) of
		{ok,RequestId} ->
			httpdown_events:httpdown_file(RequestId,PackageName),
			receiver_once(RequestId),
			{ok,RequestId};
		{error,Reason} ->
			io:format("request ~p failed.[~p] ~n",[Uri,Reason]),
			{error,Reason}
	end.
	
parse_header(Arg) ->
	case Arg of
		{stream_start,RequestId,Headers} ->
			httpdown_events:httpdown_start(RequestId,Headers);
		{stream_end,RequestId,Headers} ->
			httpdown_events:httpdown_end(RequestId,Headers)
	end.

receiver_once(RequestId) ->
	receive
		{http,{RequestId,stream_start, Headers,Pid}} ->
			parse_header({stream_start,RequestId,Headers}),
			%io:format("~p~n",[Headers]),
			%file:write(IoDevice,Headers),
			httpc:stream_next(Pid),
			receiver_left_data(RequestId,Pid);
		{http,{RequestId,{error,timeout}}} ->
			io:format("peformance timeout~n")
	after
		5000 ->
			timeout
	end.

receiver_left_data(RequestId,Pid) ->
	receive
		{http, {RequestId, stream, BinBodyPart}} ->
			httpdown_events:httpdown_stream(RequestId,BinBodyPart),
			httpdown_events:progress(RequestId,size(BinBodyPart)),
			httpc:stream_next(Pid),
			receiver_left_data(RequestId,Pid);
		{http, {RequestId, stream_end,Headers}} ->
			parse_header({stream_end,RequestId,Headers}),
			httpdown_events:httpdown_streamend(RequestId),
			httpdown_events:stop();
		{http,{RequestId,{error,timeout}}} ->
			io:format("peformance timeout~n")
	end.
	
