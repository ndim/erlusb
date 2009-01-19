# -*- Makefile -*-

SUBDIRS = . tests

bin_PROGRAMS =
check_PROGRAMS =
check_DATA =
check_SCRIPTS =
TESTS =
EXTRA_DIST =
CLEANFILES =

TESTS_ENVIRONMENT = env srcdir=$(srcdir) ERL=$(ERL)

include test/usb/Makefile-files
include test/erl-1/Makefile-files
include test/erl-2/Makefile-files
include src/Makefile-files

# libusb's usb.h does not work with --std=c99
AM_CFLAGS = -Wall -Werror -Wextra -Wno-long-long -Wno-pointer-sign -pedantic

