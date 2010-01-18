-module(lsusb).

-export([start/0, start/1]).

-include_lib("erlusb/include/erlusb.hrl").


show(#usb_device_descriptor{bDeviceClass=Class,
			    bDeviceSubClass=SubClass,
			    idVendor=IDVendor,
			    idProduct=IDProduct} = UDD) ->
    io:format("~p~n", [UDD]),
    io:format("  ID ~4.16.0b:~4.16.0b", [IDVendor, IDProduct]),
    io:format(" cls:sub ~w:~w~n", [Class, SubClass]).


start() ->
    start([]).

start([]) ->
    ok = application:start(erlusb),
    {ok, DeviceList} = erlusb:get_device_list(),
    %% io:format("~p~n", [DeviceList]),
    lists:foldl(fun(UDD,ok) -> show(UDD) end,
		ok, DeviceList).
