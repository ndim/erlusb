erlusb
======

.. contents::



What is erlusb?
---------------

erlusb might become an Erlang_ library with a general purpose interface
to USB devices some time.

As of 2009-01-21, erlusb is just a personal research project to see
how to interface an USB_ device to an Erlang program, and how to do an
Erlang/OTP application design implementing such an interface.

The hope is that Erlang's binary pattern matching, message passing,
and finite state machine support make writing device interfaces
(communicating FSMs) easy, clean, and fun - as opposed to writing such
interfaces in other languages.

.. _Erlang: http://www.erlang.org/
.. _USB:    http://www.usb.org/



What is the plan?
-----------------

Reached Milestones:

  0.5 Basic working OTP architecture

Planned Milestones:

  0.6 USB device list querying and tracking of device hotplugging

  0.7 Communication with USB device (example: Garmin GPS 60)

  0.8 Code generalized from Garmin access to general device access

  0.9 Polishing

  1.0 one dot zero!



How do I build it?
------------------

Run::

  $ autoreconf -ivs
  $ mkdir _b && cd _b
  $ ../configure --prefix=$PWD/../_i
  $ make all check install installcheck

This requires that a number of software packages are installed, such
as autoconf_, automake_, erlang_, gcc_, libtool_, GNU make_, and more.

It might also be possible to build in the source directory
("./configure && make && make install"), but this is not tested
regularly.

.. _autoconf: http://www.gnu.org/software/autoconf/
.. _automake: http://www.gnu.org/software/automake/
.. _erlang:   http://www.erlang.org/
.. _gcc:      http://gcc.gnu.org/
.. _libtool:  http://www.gnu.org/software/libtool/
.. _make:     http://www.gnu.org/software/make/



What is the license?
--------------------

No idea (yet). The complex2.erl and stuff are from the Erlang docs,
anyway. Strong contenders for the license are LGPL and MIT/Erlang.

As of 2009-01-21, the code is marked as LGPL, but that might change
later, depending on many factors internal and external.



Ideas
-----

 * gen_server for USB device access
    * One gen_server for each Erlang port / USB device, init/2 will open port.
    * handle_info/N being called by behaviour when non-OTP message is
      received (data from port)
 * USB devices being local, the usb server process only makes sense as
   {local, Foo}
 * A gen_fsm for devices might make more sense than a
   gen_server. Communicating with another device by messages is a
   classical example for communicating FSMs, after all.
 * Make a decision: One Erlang port (one libusb instance per USB host)
   for all devices, or one Erlang port (one libusb instance per USB
   device) for each device?



Design arguments
----------------

USB interface library
~~~~~~~~~~~~~~~~~~~~~

First off, we needed to find out which underlying C API to
use. Candidates included libusb-0.1_, libusb-1.0_, and openusb_.

  * libusb-0.1_ (or libusb-compat-0.1_)
     * compatible implementation for Win32 exists (libusb-win32)
     * no isochronous transfer mode support
     * very commonly used
  * libusb-1.0_
     * implementation for Win32 is in the works
     * isochronous transfer mode support
     * very commonly used
  * openusb_
     * does anyone use this at all?

The safest bet appeared to be libusb-1.0_. It is widely used, and
supports all transfer modes. So erlusb builds on libusb-1.0_.

.. _libusb-0.1:        http://www.libusb.org/
.. _libusb-compat-0.1: http://www.libusb.org/wiki/LibusbCompat0.1
.. _libusb-1.0:        http://www.libusb.org/wiki/Libusb1.0
.. _openusb:           http://sourceforge.net/projects/openusb/


