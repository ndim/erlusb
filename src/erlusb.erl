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

%%% public API
-export([start/1, stop/0]).
-export([usb_bus_list/0]).

%%% internal functions
-export([init/1, loop/1]).
-export([ei_tests/0]).

-include("erlusb.hrl").

-record(state, {port}).

start(ExtPrg) ->
    spawn(?MODULE, init, [ExtPrg]).
stop() ->
    ?MODULE ! stop.

ei_tests() ->
    [ {Atom,Result}={Atom,call_port({Atom})}
      || {Atom, Result} <-
	     [{'test-1', "Humpf, Mops, Oerks!"},
	      {'test-2', "Humpf, Mops, "},
	      {'test-3', ["Humpf", "Mops", "Oerks"]},
	      {'test-u', {'unknown_function', 'test-u'}}
	     ]].
usb_bus_list() ->
    call_port({usb_bus_list}).

call_port(Msg) ->
    ?MODULE ! {call, self(), Msg},
    receive
	{?MODULE, Result} ->
	    Result
    end.

init(ExtPrg) ->
    register(?MODULE, self()),
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
    loop(#state{port=Port}).

loop(#state{port=Port} = State) ->
    receive
	{call, Caller, Msg} ->
	    Port ! {self(), {command, term_to_binary(Msg)}},
	    receive
		{Port, {data, Data}} ->
		    io:format("Received data for ~p: ~p~n", [Msg, Data]),
		    Caller ! {?MODULE, binary_to_term(Data)}
	    end,
	    loop(State);
	stop ->
	    Port ! {self(), close},
	    io:format("Waiting for close ack~n", []),
	    receive
		{Port, closed} ->
		    io:format("Received ~p~n", [closed]),
		    exit(normal)
	    end;
	{'EXIT', Port, _Reason} ->
	    exit(port_terminated)
    end.
