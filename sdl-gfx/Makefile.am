## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = gnu

include $(top_srcdir)/am/common

# We include $(SDL_CFLAGS) because SDL_gfx headers
# do #include <SDL.h>, instead of #include <SDL/SDL.h>;
# $(SDL_CFLAGS) typically has `-I/usr/include/SDL'.
AM_CPPFLAGS += $(SDL_CFLAGS) -I$(srcdir)/SDL_gfx

SUBDIRS = SDL_gfx

WRAPPER_SOURCES = sdlgfx.c
BUILT_SOURCES   = $(WRAPPER_SOURCES:.c=.x)

cmod_LTLIBRARIES = gfx.la
gfx_la_SOURCES = $(WRAPPER_SOURCES)
gfx_la_LDFLAGS = -version-info 0:0:0 -export-dynamic -module \
   -LSDL_gfx -lSDL_gfx

include $(top_srcdir)/am/snarfing

install-data-hook:
	cd $(cmoddir) ; for f in $(cmod_LTLIBRARIES) ; do \
	   test -f lib$$f && rm -f lib$$f ; $(LN_S) $$f lib$$f ; done

## Makefile.am ends here
