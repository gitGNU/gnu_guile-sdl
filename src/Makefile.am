## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = gnu

include $(top_srcdir)/am/common

AM_CPPFLAGS += \
  -I$(top_srcdir)/sdl-gfx/SDL_gfxPrimitives \
  -I$(srcdir)/SDL_rotozoom

SUBDIRS = SDL_rotozoom

cmod_LTLIBRARIES = sdl-sup.la

WRAPPER_SOURCES = sdl.c sdlvideo.c sdlsurface.c sdlcdrom.c       \
                  sdlrect.c sdlcolor.c sdljoystick.c sdlevent.c  \
                  sdlenums.c sdlroto.c sdlgfx.c
FLAGSTASH       = initf.c kmodf.c videof.c palettef.c overlayf.c
BUILT_SOURCES   = $(WRAPPER_SOURCES:.c=.x) $(FLAGSTASH)

sdl_sup_la_SOURCES = $(BUILT_SOURCES) $(WRAPPER_SOURCES)
sdl_sup_la_LDFLAGS = -version-info 0:0:0 -export-dynamic -module \
	-LSDL_rotozoom -lrotozoom

include $(top_srcdir)/am/fspec
include $(top_srcdir)/am/snarfing

# Makefile.am ends here
