# -*- Makefile -*-

TESTS_ENVIRONMENT = env srcdir=$(srcdir) top_srcdir=$(top_srcdir) top_builddir=$(top_builddir) ERL=$(ERL) ERL_LIBS=$(PWD)

BUILT_SOURCES =
CLEANFILES =
EXTRA_DIST =
TESTS =

check_PROGRAMS =
noinst_PROGRAMS =

erlpkgcsrc_DATA =
erlpkgebin_DATA =
erlpkginclude_HEADERS =
erlpkgpriv_PROGRAMS =
erlpkgsrc_DATA =


common_usb_c_sources =


if HAVE_LIBUSB_10
EXTRA_DIST += test/test-libusb1.sh test/test-libusb1.cmds
TESTS += test/test-libusb1.sh
check_PROGRAMS += erlusb-libusb1

erlpkgpriv_PROGRAMS += erlusb-libusb1
erlusb_libusb1_SOURCES  = c_src/old_erlusb-main.c
erlusb_libusb1_SOURCES += c_src/erlusb-ei.c c_src/erlusb-ei.h
erlusb_libusb1_SOURCES += c_src/erlusb-log.c c_src/erlusb-log.h
erlusb_libusb1_SOURCES += c_src/old_driver-libusb1.c c_src/old_driver.h
common_usb_c_sources += $(erlusb_libusb1_SOURCES)
# libusb-1.0's usb.h works with neither --std=c90 nor --std=c99 :-(
# Using --std=c99 with locally patched libusb-1.0.
erlusb_libusb1_CPPFLAGS = $(AM_CPPFLAGS) $(ERL_INTERFACE_CPPFLAGS) --std=c99 $(LIBUSB_10_CFLAGS) $(CPPFLAGS)
erlusb_libusb1_LDADD = $(ERL_INTERFACE_LIBS) $(LIBUSB_10_LIBS)
erlusb_libusb1_LDFLAGS = -lpthread
endif


if HAVE_LIBUSB_10
noinst_PROGRAMS += gps-comm-test
gps_comm_test_SOURCES = c_src/gps-comm-test.c
gps_comm_test_LDADD   = $(LIBUSB_10_LIBS)
gps_comm_test_CFLAGS  = $(AM_CFLAGS) --std=c99 $(LIBUSB_10_CFLAGS)
endif


erlpkginclude_HEADERS += src/old_erlusb.hrl

erlpkgsrc_DATA  += src/old_erlusb.erl
erlpkgebin_DATA += ebin/old_erlusb.beam
CLEANFILES += ebin/old_erlusb.beam ebin/old_erlusb.log ebin/old_erlusb.vlog
EXTRA_DIST += src/old_erlusb.erl src/old_erlusb.hrl

# This rule works as the automake subdir-objects option prescribes.
ebin/old_erlusb.beam: $(srcdir)/src/old_erlusb.erl $(srcdir)/src/old_erlusb.hrl
	mkdir -p ebin
	$(ERLC) $(AM_ERLCFLAGS) $(ERLCFLAGS) -o ebin $<


erlpkgsrc_DATA  += src/old_garmin.erl
erlpkgebin_DATA += ebin/old_garmin.beam
CLEANFILES += ebin/old_garmin.beam
EXTRA_DIST += src/old_garmin.erl
ebin/old_garmin.beam: $(srcdir)/src/old_garmin.erl $(srcdir)/src/old_erlusb.hrl
	mkdir -p ebin
	$(ERLC) $(AM_ERLCFLAGS) $(ERLCFLAGS) -o ebin $<


SUFFIXES = .erl .beam
ebin/%.beam: $(srcdir)/src/%.erl
	$(ERLC) -I $(top_srcdir) $(AM_ERLCFLAGS) $(ERLCFLAGS) -o ebin $<


#### Here follows the new stuff ####


if HAVE_LIBUSB_10
TESTS      += test/test-manager.sh
EXTRA_DIST += test/test-manager.sh

erlpkgpriv_PROGRAMS    += erlusb-manager
erlusb_manager_SOURCES  = c_src/erlusb-manager.c
erlusb_manager_SOURCES += c_src/erlusb-ei.c c_src/erlusb-ei.h
erlusb_manager_SOURCES += c_src/erlusb-log.c c_src/erlusb-log.h
erlusb_manager_SOURCES += c_src/old_driver-libusb1.c c_src/old_driver.h
common_usb_c_sources   += $(erlusb_manager_SOURCES)
# libusb-1.0's usb.h works with neither --std=c90 nor --std=c99 :-(
# Using --std=c99 with locally patched libusb-1.0.
erlusb_manager_CPPFLAGS = $(AM_CPPFLAGS) $(ERL_INTERFACE_CPPFLAGS) --std=c99 $(LIBUSB_10_CFLAGS) $(CPPFLAGS)
erlusb_manager_LDADD    = $(ERL_INTERFACE_LIBS) $(LIBUSB_10_LIBS)
erlusb_manager_LDFLAGS  = -lpthread
endif


MODULES =

erlpkginclude_HEADERS += include/erlusb.hrl

erlpkgsrc_DATA += src/erlusb_internal.hrl
EXTRA_DIST     += src/erlusb_internal.hrl

MODULES += erlusb
ebin/erlusb.beam: $(srcdir)/src/erlusb.erl $(srcdir)/include/erlusb.hrl $(srcdir)/src/erlusb_internal.hrl

MODULES += erlusb_app
MODULES += erlusb_sup

erlpkgcsrc_DATA += $(sort $(common_usb_c_sources))
erlpkgebin_DATA += $(foreach mod,$(MODULES),ebin/$(mod).beam)
erlpkgsrc_DATA  += $(foreach mod,$(MODULES),src/$(mod).erl)
CLEANFILES      += $(foreach mod,$(MODULES),ebin/$(mod).beam ebin/$(mod).log ebin/$(mod).vlog)
EXTRA_DIST      += $(foreach mod,$(MODULES),src/$(mod).erl)


REGISTERED =
REGISTERED +=

commatize = $(firstword $(1))$(foreach x,$(wordlist 2,999,$(1)),,$(x))

erlpkgebin_DATA += ebin/erlusb.app
CLEANFILES      += ebin/erlusb.app
EXTRA_DIST      += src/erlusb.app.in
ebin/erlusb.app: $(srcdir)/src/erlusb.app.in Makefile $(top_builddir)/config.status
	mkdir -p ebin
	$(SED) \
		-e 's|[@]VERSION@|$(EU_PACKAGE_VERSION)|g' \
		-e "s|[@]MODULES@|$(call commatize,$(MODULES))|g" \
		-e "s|[@]REGISTERED@|$(call commatize,$(REGISTERED))|g" \
		< "$(srcdir)/src/erlusb.app.in" \
		> "ebin/erlusb.app"

all-local: $(erlpkgpriv_PROGRAMS)
	mkdir -p priv
	for p in $(erlpkgpriv_PROGRAMS); do \
		cp -p "$$p" "priv/$$p"; \
	done

distclean-local:
	rm -f priv/erlusb*.log
	for p in $(erlpkgpriv_PROGRAMS); do rm -f "priv/$$p"; done
