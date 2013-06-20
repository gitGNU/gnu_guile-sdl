#!/bin/sh
# Usage: sh -x ./autogen.sh [--libtoolize]

[ -f src/misc-utils.scm ] || {
  echo "autogen.sh: run this command only in the guile-sdl directory."
  exit 1
}

set -e

######################################################################
# SNUGGLE

snuggle m4 build-aux
snuggle h src/snuggle

######################################################################
# Guile-BAUX

guile-baux-tool import \
    pascal-pool \
    text-db-table \
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
