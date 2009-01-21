% garmin.erl - Erlang interface to Garmin GPS devices
% Copyright (C) 2009 Hans Ulrich Niedermann <hun@n-dimensional.de>
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

-module(garmin).

%%% public API
-export([start/1, stop/0]).
-export([foo/1]).

%%% internal functions
-export([init/1, loop/1]).

-include("erlusb.hrl").

-record(state, {usb, dev}).

start(DevName) ->
    spawn(?MODULE, init, [DevName]).
stop() ->
    ?MODULE ! stop.

call_port(GPS, Msg) ->
    GPS ! {call, self(), Msg},
    receive
	{GPS, Result} ->
	    Result
    end.

init(DevName) ->
    register(?MODULE, self()),
    process_flag(trap_exit, true),
    USB = erlusb:start([]),
    Dev = erlusb:device(DevName),
    loop(#state{usb=USB, dev=Dev}).

loop(#state{usb=USB, dev=Dev} = State) ->
    receive
	{call, Caller, Msg} ->
	    io:format("garmin:loop received msg from caller: ~p ~p~n",
		      [Caller, Msg]),
	    Caller ! {self(), 'moo'},
	    loop(State);
	stop ->
	    io:format("Received ~p~n", [stop]),
	    exit(normal);
	{'EXIT', Port, _Reason} ->
	    exit(port_terminated)
    end.


foo(GPS) ->
    call_port(GPS, {foo}).
