## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = gnu

PACKAGE = @PACKAGE@
VERSION = @VERSION@

# User-definable stuff:
CC=@CC@
CFLAGS  = @CFLAGS@ -Wall
LIBS    = @LIBS@
INCLUDES = -I../include/ -ISDL_gfxPrimitives/

SUBDIRS = SDL_gfxPrimitives

EXTRA_DIST = sdlgfx.h

WRAPPER_SOURCES = sdlgfx.c
BUILT_SOURCES   = $(WRAPPER_SOURCES:.c=.x)

lib_LTLIBRARIES = libguileSDL_gfx.la

libguileSDL_gfx_la_SOURCES = $(BUILT_SOURCES) $(WRAPPER_SOURCES)

libguileSDL_gfx_la_CFLAGS  = $(CFLAGS)

libguileSDL_gfx_la_LIBADD  =  SDL_gfxPrimitives/libSDL_gfxPrimitives.la

libguileSDL_gfx_la_LDFLAGS =             \
        -release $(LT_RELEASE)  \
        -version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE)

# snarfing
GUILE_SNARF = guile-snarf

SUFFIXES = .x
.c.x:
	$(GUILE_SNARF) $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $< > $@ \
	|| { rm $@; false; }

DISTCLEANFILES = *.x

