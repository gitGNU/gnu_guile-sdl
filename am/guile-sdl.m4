## guile-sdl.m4 --- some -*-autoconf-*- macros for Guile-SDL

# Copyright (C) 2007 Thien-Thi Nguyen
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 3 of
# the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public
# License along with this program; if not, write to the Free
# Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
# Boston, MA  02110-1301  USA

##----------------------------------------------------------------------------

# GUILE_SDL_OPTLIB --- Handle --disable-FOO for optional libraries
#
# $1 is a component, one of: mixer, ttf.
# $2 is a C-language function name.
#
# Arrange for --disable-$1 support.
# If not disabled, check libSDL_$1 for function $2.
# If found, do two things w/ var HAVE_$1 (all upcased):
# - create an AM_CONDITIONAL;
# - set its value to 1 and AC_SUBST it.
#
AC_DEFUN([GUILE_SDL_OPTLIB],[

AC_ARG_ENABLE([$1],[AC_HELP_STRING([--disable-$1],
[Omit bindings for SDL_$1 (default=enabled)])],:,[enable_$1=yes])
if test x"$enable_$1" = xyes ; then
  dnl Use ":" to avoid prepending to $LIBS.
  AC_CHECK_LIB([SDL_$1], $2, :, enable_$1=no)
fi

AM_CONDITIONAL(m4_toupper([HAVE_$1]), test x$enable_$1 = xyes)

if test x$enable_$1 = xyes ; then
  m4_toupper([HAVE_$1])=1
  AC_SUBST(m4_toupper([HAVE_$1]))
fi

])dnl GUILE_SDL_OPTLIB

##----------------------------------------------------------------------------

# Override libtool's attempt to support C++, Fortran, etc.
AC_DEFUN([_LT_AC_TAGCONFIG],[:])

##----------------------------------------------------------------------------
## guile-sdl.m4 ends here
