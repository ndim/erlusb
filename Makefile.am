# -*- Makefile -*-

SUBDIRS = $(subdirs) erlusb . tests

ACLOCAL_AMFLAGS = -I m4

# bin_PROGRAMS =
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

tidy:
	cd $(top_srcdir) && $(ERL) -noshell -eval 'erl_tidy:dir("src",[verbose]).' -s erlang halt
