## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = gnu

PACKAGE = @PACKAGE@
VERSION = @VERSION@

# User-definable stuff:
CC=@CC@
CFLAGS   = @CFLAGS@ -Wall
LIBS     = @LIBS@
INCLUDES = -I../include/

EXTRA_DIST = sdlmixer.h

lib_LTLIBRARIES = libguileSDL_mixer.la

libguileSDL_mixer_la_SOURCES = sdlmixer.c

libguileSDL_mixer_la_CFLAGS  = $(CFLAGS)

libguileSDL_mixer_la_LDFLAGS =             \
        -release $(LT_RELEASE)  \
        -version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE)


