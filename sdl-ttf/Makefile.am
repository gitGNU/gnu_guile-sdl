## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = gnu

PACKAGE = @PACKAGE@
VERSION = @VERSION@

# User-definable stuff:
CC=@CC@
CFLAGS  = @CFLAGS@ -Wall
LIBS    = @LIBS@
INCLUDES = -I../include/

EXTRA_DIST = sdlttf.h

lib_LTLIBRARIES = libguileSDL_ttf.la

libguileSDL_ttf_la_SOURCES = sdlttf.c

libguileSDL_ttf_la_CFLAGS  = $(CFLAGS)

libguileSDL_ttf_la_LDFLAGS =             \
        -release $(LT_RELEASE)  \
        -version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE)

