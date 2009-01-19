% erlusb.erl - Erlang interface to libusb
% Copyright (C) 2006 Hans Ulrich Niedermann <hun@n-dimensional.de>
%
% This library is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
% Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this library; if not, write to the Free Software
% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA

-module(erlusb).
-export([start/1, stop/0, init/1]).
-export([foo/1, bar/1, aaa/0, xxx/0, xxxyyy/0]).
-include("erlusb.hrl").

start(ExtPrg) ->
    spawn(?MODULE, init, [ExtPrg]).
stop() ->
    erlusb ! stop.

foo(X) ->
    call_port({foo, X}).
bar(Y) ->
    call_port({bar, Y}).
xxx() ->
    call_port({xxx}).
xxxyyy() ->
    call_port({xxxyyy}).
aaa() ->
    call_port({aaa}).

call_port(Msg) ->
    erlusb ! {call, self(), Msg},
    receive
	{erlusb, Result} ->
	    Result
    end.

init(ExtPrg) ->
    register(erlusb, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg},
		     [
		      %% {cd, "/path/to/wherever"},
		      %% {env, [{'PATH'...}]},
		      use_stdio,
		      {packet, 2},
		      hide,
		      binary
		     ]),
    loop(Port).

loop(Port) ->
    receive
	{call, Caller, Msg} ->
	    Port ! {self(), {command, term_to_binary(Msg)}},
	    receive
		{Port, {data, Data}} ->
		    Caller ! {erlusb, binary_to_term(Data)}
	    end,
	    loop(Port);
	stop ->
	    Port ! {self(), close},
	    io:format("Waiting for close ack~n", []),
	    receive
		{Port, closed} ->
		    io:format("Received data: ~p~n", [closed]),
		    exit(normal)
	    end;
	{'EXIT', Port, _Reason} ->
	    exit(port_terminated)
    end.
