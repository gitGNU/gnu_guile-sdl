## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = gnu

info_TEXINFOS = guile-sdl.texi
guile_sdl_TEXINFOS = sdl.texi \
		     sdlevent.texi sdlroto.texi \
		     sdlsurface.texi sdlvideo.texi 

SUBDIRS = src sdl sdl-mixer sdl-ttf

DIST_SUBDIRS = $(SUBDIRS) examples

# doc snarfing
GUILE_SNARF_DOCS = guile-snarf-docs
GUILE_SNARF_DOCS_TEXI = guile-snarf-docs-texi

SUFFIXES = .texi .snarf

%.snarf: src/%.c
	$(GUILE_SNARF_DOCS) $(DEFS) $(INCLUDES) -Iinclude -Isrc/SDL_rotozoom/ $(CPPFLAGS) $(CFLAGS) $< > $@ \
	|| { rm $@; false; }

.snarf.texi:
	$(GUILE_SNARF_DOCS_TEXI) $< > $@ \
	|| { rm $@; false; }

# test suite
TESTS = test/rect.scm test/image.scm test/event.scm test/mixer.scm \
        test/ttf.scm  test/cursor.scm test/roto.scm
TESTS_ENVIRONMENT = GUILE_LOAD_PATH=$(srcdir) \
	LTDL_LIBRARY_PATH=$(srcdir)/src/.libs:$(srcdir)/sdl-mixer/.libs:$(srcdir)/sdl-ttf/.libs

