#!/bin/sh
# Usage: sh -x ./autogen.sh [--libtoolize]
#
# Building guile-sdl has been tested with the following toolset:
#  libtool 1.5.6
#  autoconf 2.59
#  automake 1.7.6
#  guile 1.4.1.98
#  gperf 3.0.1
#  texinfo 4.6

[ -f sdl/misc-utils.scm ] || {
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
( echo ; cat aclocal-suffix ) >> aclocal.m4
autoheader
autoconf
automake --add-missing

######################################################################
# modsup.h

cd include
ln -sf `guile-config info includedir`/guile/modsup.h

######################################################################
# Self knowledge.

cd ..
if [ -d CVS ] ; then
    # release tags all look like v-X-Y-Z
    cvs log -h autogen.sh \
        | sed -n '/^[^a-zA-Z]v-/{s/^.//;s/:.*//;p;q;}' \
        > .last-release
fi

######################################################################
# Done.

: Now run configure and make.
: You must pass the "--enable-maintainer-mode" option to configure.

# autogen.sh ends here
