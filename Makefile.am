## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = gnu

SUBDIRS = src sdl sdl-mixer sdl-ttf sdl-gfx doc

DIST_SUBDIRS = $(SUBDIRS) examples

# test suite
TESTS = test/rect.scm test/image.scm test/event.scm test/mixer.scm \
        test/ttf.scm  test/cursor.scm test/roto.scm test/gfx.scm
TESTS_ENVIRONMENT = GUILE_LOAD_PATH=$(srcdir) \
	LTDL_LIBRARY_PATH=$(srcdir)/src/.libs:$(srcdir)/sdl-mixer/.libs:$(srcdir)/sdl-ttf/.libs:$(srcdir)/sdl-gfx/.libs

# snarfing
GUILE_SNARF = guile-snarf

SUFFIXES = .x
.c.x:
	$(GUILE_SNARF) $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $< > $@ \
	|| { rm $@; false; }

DISTCLEANFILES = *.x
