#!/bin/sh
# Usage: sh -x ./autogen.sh [--libtoolize]
#
# This has been tested with the following toolset:
#  libtool 1.5
#  autoconf 2.57
#  automake 1.7.6
#  guile 1.4.1.97

[ -f sdl/sdl.scm ] || {
  echo "autogen.sh: run this command only in the guile-sdl directory."
  exit 1
}

set -e

if [ x"$1" = x--libtoolize ]
    then libtoolize_p=true ; shift
    else libtoolize_p=false
fi

######################################################################
# Libtool setup.

if $libtoolize_p || [ ! -f ltmain.sh ] ; then
    libtoolize --force --automake
fi

######################################################################
# Invoke the auto* tools.

aclocal -I `guile-config info datadir`/aclocal
autoheader
autoconf
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
