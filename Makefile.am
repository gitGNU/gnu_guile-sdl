## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = gnu

info_TEXINFOS = guile-sdl.texi

SUBDIRS = src sdl sdl-mixer sdl-ttf

DIST_SUBDIRS = $(SUBDIRS) examples

# test suite
TESTS = test/rect.scm test/image.scm test/event.scm test/mixer.scm \
        test/ttf.scm  test/cursor.scm test/roto.scm
TESTS_ENVIRONMENT = GUILE_LOAD_PATH=$(srcdir) \
	LTDL_LIBRARY_PATH=$(srcdir)/src/.libs:$(srcdir)/sdl-mixer/.libs:$(srcdir)/sdl-ttf/.libs
