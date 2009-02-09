# -*- Makefile -*-

SUBDIRS = $(subdirs) . tests

bin_PROGRAMS =
check_PROGRAMS =
check_DATA =
check_SCRIPTS =
noinst_PROGRAMS =
TESTS =
EXTRA_DIST =
CLEANFILES =

TESTS_ENVIRONMENT = env srcdir=$(srcdir) ERL=$(ERL)

# package version from git stuff
EXTRA_DIST += GNUmakefile.in
include build-helpers/package-version.mk

# source code and tests
include test/usb/Makefile-files
include test/erl-1/Makefile-files
include test/erl-2/Makefile-files
include src/Makefile-files

AM_CFLAGS = -Wall -Werror -Wextra -Wno-long-long -Wno-pointer-sign -pedantic
AM_CPPFLAGS = -I$(top_srcdir)/src
AM_ERLCFLAGS =
# AM_ERLCFLAGS += -hybrid
AM_ERLCFLAGS += -v
AM_ERLCFLAGS += +debug_info
