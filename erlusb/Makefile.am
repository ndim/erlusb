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
erlpkgtest_DATA =


common_usb_c_sources =


SUFFIXES = .erl .beam
ebin/%.beam: $(srcdir)/src/%.erl
	mkdir -p ebin
	$(ERLC) -I $(top_srcdir) $(AM_ERLCFLAGS) $(ERLCFLAGS) -o ebin $<

ebin/%.beam: $(srcdir)/test/%.erl
	mkdir -p ebin
	$(ERLC) -I $(top_srcdir) $(AM_ERLCFLAGS) $(ERLCFLAGS) -o ebin $<


#### Here follows the new stuff ####


if HAVE_LIBUSB_10
TESTS      += test/test-manager.sh
EXTRA_DIST += test/test-manager.sh

erlpkgpriv_PROGRAMS    += erlusb-manager
erlusb_manager_SOURCES  = c_src/erlusb-manager.c
erlusb_manager_SOURCES += c_src/erlusb-ei.c c_src/erlusb-ei.h
erlusb_manager_SOURCES += c_src/erlusb-log.c c_src/erlusb-log.h
erlusb_manager_SOURCES += c_src/driver-libusb1.c c_src/driver.h
common_usb_c_sources   += $(erlusb_manager_SOURCES)
# libusb-1.0's usb.h works with neither --std=c90 nor --std=c99 :-(
# Using --std=c99 with locally patched libusb-1.0.
erlusb_manager_CPPFLAGS = $(AM_CPPFLAGS) $(ERL_INTERFACE_CPPFLAGS) --std=c99 $(LIBUSB_10_CFLAGS) $(CPPFLAGS)
erlusb_manager_LDADD    = $(ERL_INTERFACE_LIBS) $(LIBUSB_10_LIBS)
erlusb_manager_LDFLAGS  = -lpthread
endif


MODULES =
REGISTERED =
TEST_MODULES =

erlpkginclude_HEADERS += include/erlusb.hrl

erlpkgsrc_DATA += src/erlusb_internal.hrl
EXTRA_DIST     += src/erlusb_internal.hrl

REGISTERED += erlusb
MODULES    += erlusb
ebin/erlusb.beam: $(srcdir)/src/erlusb.erl $(srcdir)/include/erlusb.hrl $(srcdir)/src/erlusb_internal.hrl

MODULES += erlusb_app

REGISTERED += erlusb_sup
MODULES    += erlusb_sup

TEST_MODULES    += lsusb
TESTS           += test/test-lsusb.sh
EXTRA_DIST      += test/test-lsusb.sh
erlpkgtest_DATA += test/test-lsusb.sh

erlpkgcsrc_DATA += $(sort $(common_usb_c_sources))
erlpkgebin_DATA += $(foreach mod,$(MODULES),ebin/$(mod).beam)
erlpkgebin_DATA += $(foreach mod,$(TEST_MODULES),ebin/$(mod).beam)
erlpkgsrc_DATA  += $(foreach mod,$(MODULES),src/$(mod).erl)
erlpkgtest_DATA += $(foreach mod,$(TEST_MODULES),test/$(mod).erl)
CLEANFILES      += $(foreach mod,$(MODULES),ebin/$(mod).beam)
CLEANFILES      += $(foreach mod,$(TEST_MODULES),ebin/$(mod).beam)
EXTRA_DIST      += $(foreach mod,$(MODULES),src/$(mod).erl)
EXTRA_DIST      += $(foreach mod,$(TEST_MODULES),test/$(mod).erl)

commatize = $(firstword $(1))$(foreach x,$(wordlist 2,999,$(1)),,$(x))

erlpkgebin_DATA += ebin/erlusb.app
CLEANFILES      += ebin/erlusb.app
EXTRA_DIST      += src/erlusb.app.in
ebin/erlusb.app: $(srcdir)/src/erlusb.app.in Makefile $(top_builddir)/config.status
	mkdir -p ebin
	$(SED) \
		-e 's|[@]EU_PACKAGE_VERSION@|$(EU_PACKAGE_VERSION)|g' \
		-e 's|[@]PACKAGE_TARNAME@|$(PACKAGE_TARNAME)|g' \
		-e 's|[@]PACKAGE_VERSION@|$(PACKAGE_VERSION)|g' \
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
