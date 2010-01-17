#!/bin/sh

set -x

erl \
    -noshell \
    -eval "application:start(erlusb)" \
    -eval 'io:format("~p~n", [application:loaded_applications()])' \
    -s init stop
