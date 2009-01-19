AC_PREREQ(2.61) dnl -*- Autoconf -*-
AC_INIT([Erlang USB interface], [0.3], [hun@n-dimensional.de], [erlusb])
AC_CONFIG_SRCDIR([src/erlusb.c])
AC_CONFIG_HEADER([config.h])
AC_CONFIG_AUX_DIR([auxdir])
AM_INIT_AUTOMAKE([
    -Wall -Werror
    1.10
    dist-bzip2 dist-lzma dist-zip
    check-news foreign
    filename-length-max=99
    subdir-objects
])

# Checks for programs.
AC_PROG_CC
AM_PROG_CC_C_O
AC_ERLANG_NEED_ERLC
AC_ERLANG_NEED_ERL
AC_ERLANG_SUBST_ROOT_DIR
AC_ERLANG_SUBST_LIB_DIR

# Checks for libraries.
AC_ERLANG_CHECK_LIB([stdlib],
            [],
            [AC_MSG_ERROR([stdlib was not found!])])
AC_ERLANG_CHECK_LIB([erl_interface],
            [],
            [AC_MSG_ERROR([erl_interface was not found!])])
AC_SUBST([ERL_INTERFACE_CPPFLAGS],
         ["-I\$(ERLANG_LIB_DIR_erl_interface)/include]")
AC_SUBST([ERL_INTERFACE_LIBS],
         ["\$(ERLANG_LIB_DIR_erl_interface)/lib/liberl_interface.a \$(ERLANG_LIB_DIR_erl_interface)/lib/libei.a"])

AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR([erlusb], [${PACKAGE_VERSION}])
AC_SUBST([erlpkgebeamdir],   ["\$(ERLANG_INSTALL_LIB_DIR_erlusb)/ebin"])
AC_SUBST([erlpkgincludedir], ["\$(ERLANG_INSTALL_LIB_DIR_erlusb)/include"])
AC_SUBST([erlpkgbindir],     ["\$(ERLANG_INSTALL_LIB_DIR_erlusb)/bin"])
AC_SUBST([erlpkglibdir],     ["\$(ERLANG_INSTALL_LIB_DIR_erlusb)/lib"])

AC_SUBST([AM_ERLCFLAGS], [-v])

AC_SUBST([LIBUSB_CPPFLAGS], [])
AC_SUBST([LIBUSB_LIBS], [-lusb])

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

# Initialize the test suite
AC_CONFIG_TESTDIR([tests])
AC_CONFIG_FILES([
tests/Makefile
tests/atlocal
])

# Output files
AC_CONFIG_FILES([
Makefile
])
AC_OUTPUT
