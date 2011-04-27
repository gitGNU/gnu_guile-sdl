dnl configure.ac

dnl Copyright (C) 2003, 2004, 2005, 2007, 2008, 2009, 2011 Thien-Thi Nguyen
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

AC_INIT([guile-sdl],[0.3.10],[ttn@gnuvola.org])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_HEADERS([src/config.h])
AM_INIT_AUTOMAKE([gnu 1.10.1])
AM_MAINTAINER_MODE

##----------------------------------------------------------------------------
##: compiler

AC_PROG_CC

## If using GCC, ask for warnings and possibly a bit of quickness.
if test x"$GCC" = xyes ; then
  CFLAGS="$CFLAGS -Wall -Wno-long-long -pipe"
  keepinside='--visibility=hidden'
fi

AC_SUBST([keepinside])

##----------------------------------------------------------------------------
##: GUILE

GUILE_PROGS
GUILE_FLAGS

CPPFLAGS="$GUILE_CFLAGS"

GUILE_C2X_METHOD([c2x])

AC_MSG_CHECKING([whether twerp2texi can take option --scheme-implied])
if guile-tools twerp2texi --help | grep -q scheme-implied ; then
  TWERP2TEXIFLAGS=--scheme-implied
  AC_MSG_RESULT([yes])
else
  TWERP2TEXIFLAGS=
  AC_MSG_RESULT([no])
fi
AC_SUBST(TWERP2TEXIFLAGS)

AC_CHECK_HEADERS([guile/gh.h guile/modsup.h],,,[[
#include <libguile.h>
]])

GUILE_LIBSITE_DIR

##----------------------------------------------------------------------------
##: SDL

AM_PATH_SDL([1.2.0], :,
            AC_MSG_ERROR([*** SDL version $SDL_VERSION not found!]))

CFLAGS="$CFLAGS $SDL_CFLAGS"
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
if test $ax_cv_have_mmx_ext = yes ; then
  AC_DEFINE([USE_MMX],1,[Define if the arch supports MMX instructions.])
fi

##----------------------------------------------------------------------------
##: misc

AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_RANLIB
AC_DISABLE_STATIC	dnl module only
AC_PROG_LIBTOOL

##----------------------------------------------------------------------------

# This is basically ‘GUILE_MODULE_CATALOG_PREP’, q.v.,
# but hacked to operate in src/ instead of top-level.
# TODO: Make GUILE_MODULE_CATALOG_PREP take 2nd arg.
#
AC_DEFUN([MODULE_CATALOG_PREP],[
  AC_CONFIG_COMMANDS([module-catalog-prep],[
    test x"$DEPDIR" = x && DEPDIR=".deps"
    test -d "src/$DEPDIR" || mkdir "src/$DEPDIR"
    prereq="$DEPDIR/m4_case([$1],[],[.Pmodule-catalog],[$1])"
    test -f "src/$prereq" || echo '# dummy' > "src/$prereq"
    grep -q "include $prereq" src/Makefile || echo "include $prereq" >> src/Makefile
  ])
  AC_CONFIG_COMMANDS([module-catalog-clean-local],[
    prereq="src/$DEPDIR/m4_case([$1],[],[.Pmodule-catalog],[$1])"
    test -f "$prereq" && echo '# dummy' > "$prereq"
  ])
])

MODULE_CATALOG_PREP

##----------------------------------------------------------------------------
##: write it out

AC_CONFIG_FILES([
  Makefile
  src/Makefile
  src/SDL_gfx/Makefile
  doc/Makefile
  test/Makefile
])

AC_OUTPUT

dnl configure.ac ends here
