## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = gnu

PACKAGE = @PACKAGE@
VERSION = @VERSION@

# User-definable stuff:
CC=@CC@
CFLAGS  = @CFLAGS@ -Wall
LIBS    = @LIBS@

SUBDIRS = SDL_rotozoom #SDL_gfxPrimitives 

lib_LTLIBRARIES = libguileSDL.la

INCLUDES = -I../include/ -ISDL_gfxPrimitives/ -ISDL_rotozoom/

libguileSDL_la_SOURCES = sdl.c sdlvideo.c sdlsurface.c sdlcdrom.c    \
                       sdlrect.c sdlcolor.c sdljoystick.c sdlevent.c \
                       sdlenums.c sdlroto.c #sdlgfx.c

libguileSDL_la_CFLAGS  = $(CFLAGS)

libguileSDL_la_LIBADD  =  SDL_rotozoom/librotozoom.la #SDL_gfxPrimitives/libgfx.la

libguileSDL_la_LDFLAGS =             \
        -release $(LT_RELEASE)  \
        -version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE)


