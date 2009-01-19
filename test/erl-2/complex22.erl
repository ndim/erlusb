-module(complex22).
-export([start/1, stop/0, init/1]).
-export([foo/1, bar/1, xxx/0, yyy/0, fff/0, aaa/0, bbb/0]).

start(ExtPrg) ->
    spawn(?MODULE, init, [ExtPrg]).
stop() ->
    complex ! stop.

foo(X) ->
    call_port({foo, X}).
bar(Y) ->
    call_port({bar, Y}).
xxx() ->
    call_port({xxx}).
yyy() ->
    call_port({yyy}).
fff() ->
    call_port({fff}).
aaa() ->
    call_port({aaa}).
bbb() ->
    call_port({bbb}).

call_port(Msg) ->
    complex ! {call, self(), Msg},
    receive
	{complex, Result} ->
	    Result
    end.

init(ExtPrg) ->
    register(complex, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [{packet, 2}, binary]),
    loop(Port).

loop(Port) ->
    receive
	{call, Caller, Msg} ->
	    Port ! {self(), {command, term_to_binary(Msg)}},
	    receive
		{Port, {data, Data}} ->
		    Caller ! {complex, binary_to_term(Data)}
	    end,
	    loop(Port);
	stop ->
	    Port ! {self(), close},
	    receive
		{Port, closed} ->
		    exit(normal)
	    end;
	{'EXIT', Port, _Reason} ->
	    exit(port_terminated)
    end.
