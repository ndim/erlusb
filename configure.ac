AC_PREREQ(2.61) dnl -*- Autoconf -*-
AC_INIT([Erlang USB interface],
        [m4_esyscmd([./build-helpers/package-version . version-stamp])],
        [hun@n-dimensional.de],
        [erlusb])
AC_CONFIG_SRCDIR([src/erlusb-main.c])
AC_CONFIG_HEADER([config.h])
AC_CONFIG_AUX_DIR([auxdir])
AM_INIT_AUTOMAKE([
    -Wall -Werror
    1.10
    dist-bzip2 dist-lzma dist-zip
    foreign
    filename-length-max=99
    subdir-objects
])

# Checks for programs.
AC_PROG_CC

dnl We potentially need libtool to link against an uninstalled version
dnl of libusb-1.0. None of the code is in C++ or Fortran, though.
CXX=false
F77=false
AC_PROG_LIBTOOL

AM_PROG_CC_C_O
AC_ERLANG_NEED_ERLC
AC_ERLANG_NEED_ERL
AC_ERLANG_SUBST_ROOT_DIR
AC_ERLANG_SUBST_LIB_DIR
m4_pattern_forbid([^PKG_PROG_PKG_CONFIG])dnl
PKG_PROG_PKG_CONFIG()

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

PKG_CHECK_MODULES([LIBUSB], [libusb],
                  [have_libusb=yes],
                  [have_libusb=no])
AM_CONDITIONAL([HAVE_LIBUSB], [test "x$have_libusb" = "xyes"])

PKG_CHECK_MODULES([LIBUSB_10], [libusb-1.0],
                  [have_libusb_10=yes],
                  [have_libusb_10=no])
AM_CONDITIONAL([HAVE_LIBUSB_10], [test "x$have_libusb_10" = "xyes"])

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

# Make sure the versions are always accurate
AC_CONFIG_FILES([GNUmakefile])

# Initialize the test suite
AC_CONFIG_TESTDIR([tests])
AC_CONFIG_FILES([tests/Makefile])
AC_CONFIG_FILES([tests/atlocal])

# Subpackages

# Output files
AC_CONFIG_FILES([Makefile])
AC_OUTPUT

dnl Be nice to user.
echo "erlusb compile option summary:
   libusb-0.1: $have_libusb
   libusb-1.0: $have_libusb_10
Please verify that this meets your expectations."
