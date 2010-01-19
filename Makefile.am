# -*- Makefile -*-

SUBDIRS = $(subdirs) . erlusb

ACLOCAL_AMFLAGS = -I m4

EXTRA_DIST =
CLEANFILES =

# package version from git stuff
EXTRA_DIST += GNUmakefile.in
include build-helpers/package-version.mk

erl_tidy:
	cd $(top_srcdir) && \
		$(ERL) -noshell \
			-eval 'erl_tidy:dir("erlusb/src",[verbose]).' \
			-s erlang halt
