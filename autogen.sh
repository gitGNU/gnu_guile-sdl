#!/bin/sh

[ -f sdl/sdl.scm ] || {
  echo "autogen.sh: run this command only in the guile-sdl directory."
  exit 1
}

libtoolize --copy --automake
autoconf
automake --add-missing
