# virt-similarity
# Copyright (C) 2013 Red Hat Inc.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

AC_INIT([virt-similarity],[1.0])
AM_INIT_AUTOMAKE([foreign])

dnl Only used temporarily while the spec file is contained in the
dnl tarball.  We will remove this later.
AC_SUBST([RPM_RELEASE],[1])

AC_CONFIG_MACRO_DIR([m4])

dnl Allow all GNU/Linux functions.
dnl autoconf complains unless this is very early in the file.
AC_USE_SYSTEM_EXTENSIONS

AC_PROG_LIBTOOL

dnl Check for basic C environment.
AC_PROG_CC_STDC
AC_PROG_INSTALL
AC_PROG_CPP

AC_C_PROTOTYPES
test "x$U" != "x" && AC_MSG_ERROR([Compiler not ANSI compliant])

AM_PROG_CC_C_O

AC_ARG_ENABLE([gcc-warnings],
    [AS_HELP_STRING([--enable-gcc-warnings],
                    [turn on lots of GCC warnings (for developers)])],
    [case $enableval in
       yes|no) ;;
       *)      AC_MSG_ERROR([bad value $enableval for gcc-warnings option]) ;;
     esac
     gcc_warnings=$enableval],
    [gcc_warnings=no]
)

if test "$gcc_warnings" = yes; then
    # XXX With gnulib we can improve this in future.
    WARN_CFLAGS="-Wall"
    AC_SUBST([WARN_CFLAGS])
    WERROR_CFLAGS="-Werror"
    AC_SUBST([WERROR_CFLAGS])
fi

dnl Check support for 64 bit file offsets.
AC_SYS_LARGEFILE

dnl OCaml compiler.
AC_PROG_OCAML
if test "$OCAMLC" = "no"; then
    AC_MSG_ERROR([You must install the OCaml compiler])
fi

AM_CONDITIONAL([HAVE_OCAMLOPT], [test "x$OCAMLOPT" != "xno"])

dnl OCaml findlib ("ocamlfind") is required.
AC_PROG_FINDLIB
if test "x$OCAMLFIND" = "xno"; then
    AC_MSG_ERROR([You must install OCaml findlib (the ocamlfind command)])
fi

dnl OCaml libguestfs bindings are required.
AC_CHECK_OCAML_PKG(guestfs)
if test "x$OCAML_PKG_guestfs" = "xno"; then
    AC_MSG_ERROR([You must install OCaml libguestfs bindings >= 1.14])
fi

dnl Check for POD (for manual pages).
AC_CHECK_PROG(PERLDOC,perldoc,perldoc)
if test "x$PERLDOC" = "x"; then
    AC_MSG_ERROR([You must install the perldoc program])
fi

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([Makefile
                 config.ml
                 virt-similarity.spec])
AC_OUTPUT
