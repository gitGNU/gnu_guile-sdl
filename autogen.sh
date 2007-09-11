#!/bin/sh
# Usage: sh -x ./autogen.sh [--libtoolize]
#
# Building Guile-SDL has been tested with the following toolset:
#  GNU Libtool 1.5.24
#  GNU Autoconf 2.61
#  GNU Automake 1.9.6
#  guile 1.4.1.107.12
#  GNU gperf 3.0.2
#  GNU texinfo 4.8

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

aclocal -I `guile-config info datadir`/aclocal --output=- \
   | sed '$raclocal-suffix' > aclocal.m4
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
