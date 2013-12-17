-module(http_to_file).
-export([start/0,stop/0]).
-export([create_file/2,close_file/1,write_file/2]).

-define(DOWNLOADPATH,"C:\\cygwin64\\data\\").

start() ->
  Pid = spawn(fun() -> loop([]) end),
  register(?MODULE,Pid).


stop() ->
  unregister(?MODULE).

rpc(Request) ->
  ?MODULE!Request.

create_file(RequestId,FileName) ->
  FileFullName = string:concat(?DOWNLOADPATH,FileName),
  IsFile = filelib:is_file(FileFullName),
  if
	IsFile == true ->
	  case file:open(FileFullName,[write,binary]) of
		{ok,Fd} ->
		  rpc({set_fd,{RequestId,Fd}});
		{error,Reason}	->
		  io:format("~p ~n",[Reason]),
		  {error,Reason}
	  end;
	IsFile == false ->
	  {error,file_exist}
  end.

close_file(RequestId) ->
  rpc({del_fd,RequestId}).

write_file(RequestId,BinBody) ->
  rpc({write_file,{RequestId,BinBody}}).

loop(Args) ->  
  receive
	{set_fd,{RequestId,Fd}} ->
	  NewArgs = [{RequestId,Fd}|Args],
	  loop(NewArgs);
	{del_fd,RequestId} ->
	  case get_fd(Args,RequestId) of
		{RequestId,Fd} ->
		  file:close(Fd),
		  NewArgs = lists:delete({RequestId,Fd},Args),
		  loop(NewArgs);
		none ->
		  io:format("[~p] get_fd failed [~p] ~n",[?LINE,RequestId]),
		  loop(Args)
	  end;
	{write_file,{RequestId,BinBody}} ->
	  case get_fd(Args,RequestId) of
		{RequestId,Fd} ->
		  file:write(Fd,BinBody),
		  loop(Args);
		none ->
		  io:format("[~p] get_fd failed [~p] ~n",[?LINE,RequestId]),
		  loop(args)
	  end;
	_Any ->
	  io:format("~p~n",[_Any]),
	  loop(Args)
  end.


get_fd([H|T],RequestId) ->
  case H of 
	{RequestId,_Fd} ->
	  H;
	_Any ->
	  get_fd(T,RequestId)
  end;
get_fd([],_RequestId) ->
  none.









