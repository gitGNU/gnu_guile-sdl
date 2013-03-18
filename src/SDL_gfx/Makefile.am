## Makefile.am
##
## Copyright (C) 2010, 2011, 2013 Thien-Thi Nguyen
##
## This program is free software; you can redistribute it and/or
## modify it under the terms of the GNU General Public License as
## published by the Free Software Foundation; either version 3 of
## the License, or (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public
## License along with this program; if not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
## Boston, MA  02110-1301  USA

AM_CPPFLAGS = $(SDL_CFLAGS)
AM_CFLAGS = $(keepinside) $(NORMAL_CFLAGS)

noinst_LTLIBRARIES = libSDL_gfx.la

libSDL_gfx_la_SOURCES = \
  SDL_framerate.c \
  SDL_framerate.h \
  SDL_gfxBlitFunc.c \
  SDL_gfxBlitFunc.h \
  SDL_gfxPrimitives.c \
  SDL_gfxPrimitives.h \
  SDL_gfxPrimitives_font.h \
  SDL_rotozoom.c \
  SDL_rotozoom.h

noinst_LTLIBRARIES += libimfi.la

libimfi_la_SOURCES = \
  SDL_imageFilter.c \
  SDL_imageFilter.h

libimfi_la_CFLAGS = $(AM_CFLAGS) $(IMFI_CFLAGS)

EXTRA_DIST = LICENSE

## Makefile.am ends here
