## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = gnu

include $(top_srcdir)/am/common

AM_CPPFLAGS += -I$(srcdir)/SDL_gfxPrimitives

SUBDIRS = SDL_gfxPrimitives

WRAPPER_SOURCES = sdlgfx.c
BUILT_SOURCES   = $(WRAPPER_SOURCES:.c=.x)

cmod_LTLIBRARIES = gfx.la
gfx_la_SOURCES = $(WRAPPER_SOURCES)
gfx_la_LDFLAGS = -version-info 0:0:0 -export-dynamic -module \
   -LSDL_gfxPrimitives -lSDL_gfxPrimitives

include $(top_srcdir)/am/snarfing

install-data-hook:
	cd $(cmoddir) ; for f in $(cmod_LTLIBRARIES) ; do \
	   test -f lib$$f && rm -f lib$$f ; $(LN_S) $$f lib$$f ; done

## Makefile.am ends here
