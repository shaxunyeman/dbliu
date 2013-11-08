-module(deamon).
-export([start/2]).

start(Exit,Flag) ->
	A = spawn(fun() -> a() end),
	B = spawn(fun() -> b(A,Flag) end),
	C = spawn(fun() -> c(B,Exit) end),
	sleep(2000),
	process_status(a,A),
	process_status(b,B),
	process_status(c,C).

a() ->
	process_flag(trap_exit,true),
	wait(a).

b(Pid,Flag) ->
	process_flag(trap_exit,Flag),
	link(Pid),
	wait(b).

c(Pid,Exit) ->
	link(Pid),	
	case Exit of
		{die,Reason} ->
			exit(Reason);
		{divide,N} ->
			1/N;
		normal ->
			true
	end.

sleep(T) ->
	receive
	after T -> true
	end.

process_status(Name,Pid) ->
	case erlang:is_process_alive(Pid) of
		true ->
			io:format("process ~p (~p) is alive ~n",[Name,Pid]);
		false ->
			io:format("process ~p (~p) is dead ~n",[Name,Pid])
	end.

wait(Flag) ->
	receive
		Any ->
			io:format("process (~p) has received message ~p ~n",[Flag,Any]),
			wait(Flag)
	end.
