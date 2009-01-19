#!/bin/sh

echo "$0: start"
${ERL} +v -pa src < ${srcdir}/src/test-erlusb.cmds
echo "$0: end"
