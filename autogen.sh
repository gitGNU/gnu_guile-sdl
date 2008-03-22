#!/bin/sh
# Usage: sh -x ./autogen.sh [--libtoolize]
#
# Building Guile-SDL has been tested with the following toolset:
#  GNU Libtool 1.5.24
#  GNU Autoconf 2.61
#  GNU Automake 1.9.6
#  Guile 1.4.1.109
#  GNU gperf 3.0.2
#  GNU texinfo 4.8

[ -f sdl/misc-utils.scm ] || {
  echo "autogen.sh: run this command only in the guile-sdl directory."
  exit 1
}

set -e

( cd am ; ln -sf ../../.common/sofix )
test -f am/sofix || echo WARNING: am/sofix points nowhere

######################################################################
# Libtool setup.

if [ x"$1" = x--libtoolize ] || [ ! -f ltmain.sh ] ; then
    libtoolize --force --automake
fi

######################################################################
# Invoke the auto* tools.

loc="-I `guile-config info datadir`/aclocal"
test "$loc" = "-I `aclocal --print-ac-dir`" && loc=
aclocal $loc -I am
autoheader
autoconf
ln -sf ../.common/GPLv3 COPYING
automake --add-missing

######################################################################
# modsup.h

cd include
ln -sf `guile-config info includedir`/guile/modsup.h

######################################################################
# Done.

: Now run configure and make.
: You must pass the "--enable-maintainer-mode" option to configure.

# autogen.sh ends here
