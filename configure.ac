dnl configure.ac

dnl Copyright (C) 2003, 2004, 2005, 2007, 2008, 2009,
dnl               2011, 2012, 2013 Thien-Thi Nguyen
dnl
dnl This program is free software; you can redistribute it and/or
dnl modify it under the terms of the GNU General Public License as
dnl published by the Free Software Foundation; either version 3 of
dnl the License, or (at your option) any later version.
dnl
dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public
dnl License along with this program; if not, write to the Free
dnl Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
dnl Boston, MA  02110-1301  USA

AC_INIT([guile-sdl],[0.5.1],[bug-guile-sdl@gnu.org])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([build-aux])
AC_CONFIG_HEADERS([src/config.h])
AM_INIT_AUTOMAKE([1.12.2 gnu serial-tests no-dist-gzip dist-lzip dist-xz])
AM_MAINTAINER_MODE

AC_ARG_VAR([IMFI_CFLAGS],[
  additional flags for compiling the SDL_gfx imagefilter functions,
  appended to the normal CFLAGS (see README)
])

SNUGGLE_SET_SOFIXFLAGS

##----------------------------------------------------------------------------
##: compiler

LT_PREREQ([2.4.2])
LT_INIT([dlopen disable-static]) dnl module only

AC_PROG_CC_C99
AS_IF([test x"$ac_cv_prog_cc_c99" = xno],
[AC_MSG_ERROR([*** C99 compiler not found!])])

AS_IF([test x"$GCC" = xyes],[keepinside='--visibility=hidden'])
AC_SUBST([keepinside])

##----------------------------------------------------------------------------
##: GUILE

SNUGGLE_PROGS
SNUGGLE_FLAGS

SNUGGLE_GUILE_TOOLS_EXISTSP([gsdl_cv_mmc],[make-module-catalog])
AM_CONDITIONAL([LAME],[test no = $gsdl_cv_mmc])
AM_COND_IF([LAME],[AC_DEFINE([LAME], 1,
[Define to 1 if Guile needs a Scheme wrapper to load .so files.])])

CPPFLAGS="$GUILE_CFLAGS"

SNUGGLE_CHECK_CLASSIC_HEADERS

AC_CHECK_DECLS([scm_values],,,[[
#include <libguile.h>
]])

SNUGGLE_GUILE_LIBSITE_DIR([gsdl])

##----------------------------------------------------------------------------
##: SDL

AM_PATH_SDL([1.2.0], :,
            AC_MSG_ERROR([*** SDL version $SDL_VERSION not found!]))

# <http://wiki.libsdl.org/FAQDevelopment> sez:
#
#   The most portable way to include SDL headers is
#   to use quotes around the header name:
#     #include "SDL.h"
#
# so we don't need to do this:
#
#- SDL_CFLAGS=`echo $SDL_CFLAGS | sed 's|-I */usr/include/SDL/* *||'`
#
# This is also undesirable when SDL is installed in a non-standard
# location, or when ‘--with-sdl{,-exec}-prefix=DIR’ is used.

dnl Extract ‘-I DIR’ for the sake of k2c.
SDLMINUSI=`echo $SDL_CFLAGS | sed 's|\(-I *[[^ ]]*\).*|\1|'`
AC_SUBST([SDLMINUSI])

LIBS="$LIBS $SDL_LIBS"

dnl Checks for libraries.
dnl MUST have SDL_image
AC_CHECK_LIB(SDL_image, IMG_Load,
 	     LIBS="$LIBS -lSDL_image",
 	     AC_MSG_ERROR([*** SDL_image library not found!]))

dnl Bindings for optional libs.
GUILE_SDL_OPTLIB([mixer],[Mix_OpenAudio])
GUILE_SDL_OPTLIB([ttf],[TTF_Init])

dnl Arch-specific checks.

AC_CHECK_SIZEOF([long])

AC_DEFUN([CHECK_MMX_USABILITY],[

AX_GCC_X86_CPUID([0x00000001])
edx=`echo $ax_cv_gcc_x86_cpuid_0x00000001 | sed 's/^unknown$//;s/.*://'`

AC_CACHE_CHECK([whether mmx is supported],[ax_cv_have_mmx_ext],[
  # look at nybble representing edx[23:20]
  case `echo $edx | sed 's/.*\(.\).....$/\1/;y/ABCDEF/abcdef/'` in
     # specifically, test if bit 23 is set
     8|9|a|b|c|d|e|f) ax_cv_have_mmx_ext=yes ;;
     *)               ax_cv_have_mmx_ext=no ;;
  esac
])
AS_IF([test $ax_cv_have_mmx_ext = yes],
[AC_DEFINE([USE_MMX],1,[Define if the arch supports MMX instructions.])])

])dnl CHECK_MMX_USABILITY

dnl The asm in src/SDL_gfx/*.c uses ‘pusha’, which is not supported in
dnl "64-bit mode".  We assume ‘4 == sizeof (long)’ means "32-bit mode".
AS_IF([test 4 = $ac_cv_sizeof_long],[CHECK_MMX_USABILITY])

##----------------------------------------------------------------------------
##: transfer ‘CFLAGS’ to ‘NORMAL_CFLAGS’

NORMAL_CFLAGS="$CFLAGS"
AC_SUBST([NORMAL_CFLAGS])

CFLAGS=

##----------------------------------------------------------------------------
##: write it out

AC_CONFIG_FILES([
  Makefile
  src/Makefile
  src/SDL_gfx/Makefile
  src/k/Makefile
  doc/Makefile
  test/Makefile
  test/zow
  ugh/Makefile
])

AC_OUTPUT

dnl configure.ac ends here
