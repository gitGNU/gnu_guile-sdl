## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = gnu

info_TEXINFOS = guile-sdl.texi

lib_LTLIBRARIES = libguileSDL.la

BUILT_SOURCES = sdl.x
#libguileSDL_la_SOURCES = $(BUILT_SOURCES) guile-sdl.c guile-sdl.h video.c video.h
libguileSDL_la_SOURCES = guile-sdl.c guile-sdl.h video.c video.h #image.c image.h
libguileSDL_la_LDFLAGS = -lSDL -lSDL_image -lpthread -version-info 0:0 -export-dynamic

sdldir = $(datadir)/guile/sdl
sdl_DATA = sdl.scm

EXTRA_DIST = autogen.sh convert.sh $(sdl_DATA)

GUILE_SNARF = guile-snarf

SUFFIXES = .x
.c.x:
	$(GUILE_SNARF) $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $< > $@ \
	|| { rm $@; false; }

DISTCLEANFILES = *.x
