#!/bin/sh
# Usage: sh -x ./autogen.sh [--libtoolize]
#
# Building Guile-SDL has been tested with the following toolset:
#  GNU Libtool 2.4.2
#  GNU Autoconf 2.69
#  GNU Automake 1.12.4
#  Unofficial Guile 1.4.1.127
#  GNU Guile 1.8.7
#  GNU Guile 2.0.6
#  GNU texinfo 4.13
#  Guile-BAUX 20121026.1208.c21074a
#  SNUGGLE 0.1

[ -f src/misc-utils.scm ] || {
  echo "autogen.sh: run this command only in the guile-sdl directory."
  exit 1
}

set -e

######################################################################
# SNUGGLE

snuggle m4 build-aux

######################################################################
# Guile-BAUX

guile-baux-tool import \
    re-prefixed-site-dirs \
    c2x \
    gen-scheme-wrapper \
    forms-from \
    tsar \
    c-tsar \
    tsin \
    gbaux-do

######################################################################
# Invoke the auto* tools.

autoreconf -B `guile-config info datadir`/aclocal -v -i -f

######################################################################
# Done.

: Now run configure and make.
: You must pass the "--enable-maintainer-mode" option to configure.

# autogen.sh ends here
