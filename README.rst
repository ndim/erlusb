erlusb
======

.. contents::



What is erlusb?
---------------

erlusb might become an Erlang library with a general purpose interface
to USB devices some time.

As of 2009-01-21, Erlusb is just a private research project to see how
to interface an USB device to an Erlang program.



What is the plan?
-----------------

First off, we need to find out which underlying C API to use. Candidates
include libusb-0.1, libusb-1.0, and openusb.

The first milestone will be to get an Erlang program talking to (for
lack of another USB device) my Garmin GPS 60. As of 2009-01-21, that
milestone is far away, though.

After that, there will need to be a way to keep existing functionality
while changing the code to more generality.



How do I build it?
------------------

Run::

  $ autoreconf -ivs
  $ mkdir _b && cd _b
  $ ../configure --prefix=$PWD/../_i
  $ make all check install installcheck

This requires that a number of software packages are installed, such
as autoconf, automake, erlang, gcc, make, and more.



What is the license?
--------------------

No idea (yet). The complex2.erl and stuff are from the Erlang docs,
anyway. Strong contenders for the license are LGPL and MIT/Erlang.

As of 2009-01-21, the code is marked as LGPL, but that might change
later, depending on many factors internal and external.



