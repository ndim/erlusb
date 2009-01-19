#!/bin/sh

pwd
${ERL} +v -pa test/erl-1 < ${srcdir}/test/erl-1/test-erl.cmds
