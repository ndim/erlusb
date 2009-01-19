#!/bin/sh

pwd
${ERL} +v -pa test/erl-2 < ${srcdir}/test/erl-2/test-erl.cmds
