## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = gnu

info_TEXINFOS = guile-sdl.texi

lib_LTLIBRARIES = libguileSDL.la

BUILT_SOURCES = sdl.x
libguileSDL_la_SOURCES = guile-sdl.c guile-sdl.h scm.c scm.h \
			 video.c video.h image.c image.h gfx.c gfx.h         \
			 event.c event.h mixer.c mixer.h
libguileSDL_la_LDFLAGS = -lSDL -lSDL_image -lSDL_mixer -lpthread -version-info 0:0 -export-dynamic

sdldir = $(datadir)/guile
sdldatadir = $(datadir)/guile/sdl
sdl_DATA = sdl/sdl.scm

EXTRA_DIST = autogen.sh convert.sh $(sdl_DATA)

GUILE_SNARF = guile-snarf

SUFFIXES = .x
.c.x:
	$(GUILE_SNARF) $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $< > $@ \
	|| { rm $@; false; }

DISTCLEANFILES = *.x

# test suite
TESTS = test/rect.scm test/image.scm test/event.scm test/mixer.scm
TESTS_ENVIRONMENT = LTDL_LIBRARY_PATH=$(srcdir)/.libs \
		    GUILE_LOAD_PATH=$(srcdir)
