AM_CPPFLAGS = -I. -I$(srcdir)

EXTRA_DIST = SDL_gfxPrimitives.h \
	     SDL_gfxPrimitives_font.h \
	     SDL_rotozoom.h \
	     SDL_framerate.h \
	     SDL_imageFilter.h

noinst_LTLIBRARIES = libSDL_gfx.la

libSDL_gfx_la_SOURCES = SDL_gfxPrimitives.c \
			SDL_rotozoom.c \
			SDL_framerate.c \
			SDL_imageFilter.c
libSDL_gfx_la_CFLAGS  = $(CFLAGS)
libSDL_gfx_la_LIBADD  = $(LIBS)
