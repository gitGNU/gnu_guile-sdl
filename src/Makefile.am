## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = gnu

PACKAGE = @PACKAGE@
VERSION = @VERSION@

# User-definable stuff:
CC=@CC@
CFLAGS  = @CFLAGS@ -Wall
LIBS    = @LIBS@

#SUBDIRS = SDL_gfxPrimitives \
#	  SDL_rotozoom

#sdlmoduledir = $(datadir)/guile/sdl

# The scheme file for bringing in the module
#sdlmodule_DATA = sdl.scm dynlink.scm


EXTRA_DIST = sdl.h \
             sdlevent.h \
             sdlgfx.h \
             sdlvideo.h \
             sdlenums.h \
             #sdljoystick.h \
             #sdlrect.h \
             #sdltimer.h \
             #sdlcdrom.h \
             #sdlkeyboard.h \
             #sdlroto.h \
             #sdlimage.h \
             #sdlmouse.h \
	     #sdlpalette.h \
             #sdlsurface.h


lib_LTLIBRARIES = libguileSDL.la

INCLUDES = -I../include/ #-ISDL_gfxPrimitives/ -ISDL_rotozoom/

# libguileSDL_la_SOURCES = sdl.c sdlrect.c sdlpalette.c sdlsurface.c \
# 		      sdlvideo.c \
#                       sdlevents.c sdlkeyboard.c  sdlmouse.c \
# 	              sdlcdrom.c  sdljoystick.c  sdltimer.c \
# 		      sdlenums.c  sdlimage.c \
#                       sdlgfx.c sdlroto.c \
# 		      sdl-main.c

libguileSDL_la_SOURCES = sdl.c sdlvideo.c \
                      sdlevent.c sdlenums.c sdlgfx.c

libguileSDL_la_CFLAGS  = $(CFLAGS)

#libguileSDL_la_LIBADD  =  SDL_gfxPrimitives/libgfx.la
#	  	          SDL_rotozoom/librotozoom.la	

libguileSDL_la_LDFLAGS =             \
        -release $(LT_RELEASE)  \
        -version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE)


