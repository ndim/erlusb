AC_PREREQ(2.61) dnl -*- Autoconf -*-
AC_INIT([Erlang USB interface],
        [m4_esyscmd([./build-helpers/package-version . version-stamp])],
        [hun@n-dimensional.de],
        [erlusb])
AC_CONFIG_SRCDIR([src/erlusb-main.c])
AC_CONFIG_HEADER([config.h])
AC_CONFIG_AUX_DIR([auxdir])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([
    -Wall -Werror
    1.11
    no-dist-gzip dist-xz
    foreign
    filename-length-max=99
    subdir-objects
])

dnl Simplified version number
dnl Change something like 0.4.4-gd77d781-feature_otp-architecture to 0.4.4
AC_SUBST([EU_PACKAGE_VERSION], [`echo "$PACKAGE_VERSION" | sed 's/-.*//'`])

AC_MSG_NOTICE([looking for compilers and tools])

# Checks for programs.
AC_PROG_CC

dnl We potentially need libtool to link against an uninstalled version
dnl of libusb-1.0.
LT_INIT

AM_PROG_CC_C_O
AC_ERLANG_NEED_ERLC
AC_ERLANG_NEED_ERL
AC_ERLANG_SUBST_ROOT_DIR
AC_ERLANG_SUBST_LIB_DIR
m4_pattern_forbid([^PKG_PROG_PKG_CONFIG])dnl
PKG_PROG_PKG_CONFIG()

# Checks for libraries.
AC_MSG_NOTICE([checking for Erlang library locations])
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

AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR([erlusb], [${EU_PACKAGE_VERSION}])
AC_SUBST([erlpkgrootdir],    ["\$(ERLANG_INSTALL_LIB_DIR_erlusb)"])
dnl
AC_SUBST([erlpkgcsrcdir],    ["\$(erlpkgrootdir)/c_src"])
AC_SUBST([erlpkgebindir],    ["\$(erlpkgrootdir)/ebin"])
AC_SUBST([erlpkgincludedir], ["\$(erlpkgrootdir)/include"])
AC_SUBST([erlpkgprivdir],    ["\$(erlpkgrootdir)/priv"])
AC_SUBST([erlpkgsrcdir],     ["\$(erlpkgrootdir)/src"])

dnl Missing: -Werror
AC_SUBST([AM_CFLAGS], ["-Wall -Wextra -Wno-long-long -Wno-pointer-sign -pedantic"])
AC_SUBST([AM_CPPFLAGS], ["-I\$(top_srcdir)/src"])
dnl Missing: -hybrid
AC_SUBST([AM_ERLCFLAGS], ["-v +debuginfo"])

AC_MSG_NOTICE([checking for C libs])

PKG_CHECK_MODULES([LIBUSB_10], [libusb-1.0],
                  [have_libusb_10=yes],
                  [have_libusb_10=no])
AM_CONDITIONAL([HAVE_LIBUSB_10], [test "x$have_libusb_10" = "xyes"])

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.
dnl FIXME: check that sizeof(long) >= sizeof(int)
dnl FIXME: check that sizeof(long) >= sizeof(size_t)

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
cat<<EOF
$PACKAGE_NAME compile option summary:
   libusb-1.0: $have_libusb_10
Please verify that this meets your expectations.
EOF
