## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = gnu

include $(top_srcdir)/am/common

AM_CPPFLAGS += -I$(srcdir)/SDL_gfx

SUBDIRS = SDL_gfx

WRAPPER_SOURCES = sdlgfx.c
BUILT_SOURCES   = $(WRAPPER_SOURCES:.c=.x)

cmod_LTLIBRARIES = gfx.la
gfx_la_SOURCES = $(WRAPPER_SOURCES)
gfx_la_LDFLAGS = -version-info 0:0:0 -export-dynamic -module \
   -LSDL_gfx -lSDL_gfx

include $(top_srcdir)/am/snarfing

install-data-hook: do-sofix

## Makefile.am ends here
