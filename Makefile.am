## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = gnu

info_TEXINFOS = guile-sdl.texi

#lib_LTLIBRARIES = libguileSDL.la

#libguileSDL_la_SOURCES = guile-sdl.c guile-sdl.h scm.c scm.h wm.c wm.h  \
#			 video.c video.h image.c image.h gfx.c gfx.h    \
#			 event.c event.h mixer.c mixer.h ttf.c ttf.h
#libguileSDL_la_LDFLAGS = -lSDL -lSDL_image -lSDL_mixer -lpthread -version-info 0:0 -export-dynamic

#sdldir = $(datadir)/guile/sdl
#sdl_DATA = sdl.scm

#EXTRA_DIST = autogen.sh convert.sh $(sdl_DATA)

SUBDIRS = src sdl #sdl-mixer #sdl-ttf

DIST_SUBDIRS = $(SUBDIRS) examples

# test suite
TESTS = test/rect.scm test/image.scm test/event.scm test/mixer.scm \
        test/ttf.scm  test/cursor.scm
TESTS_ENVIRONMENT = LTDL_LIBRARY_PATH=$(srcdir)/src/.libs \
		    GUILE_LOAD_PATH=$(srcdir)
