## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = gnu

AM_CPPFLAGS = -I.. -I. -I$(srcdir) -I$(top_srcdir)/include \
  -I$(srcdir)/SDL_gfxPrimitives

SUBDIRS = SDL_gfxPrimitives

EXTRA_DIST = sdlgfx.h

cmoddir = $(guilelibsitedir)/sdl

WRAPPER_SOURCES = sdlgfx.c
BUILT_SOURCES   = $(WRAPPER_SOURCES:.c=.x)
BUILT_DOCS      = $(WRAPPER_SOURCES:.c=.texi)

cmod_LTLIBRARIES = gfx.la
gfx_la_SOURCES = $(BUILT_SOURCES) $(WRAPPER_SOURCES)
gfx_la_LDFLAGS = -version-info 0:0:0 -export-dynamic -module \
   -LSDL_gfxPrimitives -lSDL_gfxPrimitives

# snarfing
SUFFIXES = .x .texi .doc
.c.x:
	$(c2x) -o $@ $< $(DEFS) $(AM_CPPFLAGS) $(CPPFLAGS) $(CFLAGS)

DISTCLEANFILES = *.x

# doc snarfing
GUILE_SNARF_DOCS = guile-snarf-docs
GUILE_SNARF_DOCS_TEXI = guile-snarf-docs-texi

%.doc: %.c
	$(GUILE_SNARF_DOCS) $(DEFS) $(AM_CPPFLAGS) -I../include \
	$(CPPFLAGS) $(CFLAGS) $< > $@ \
	|| { rm $@; false; }

.doc.texi:
	touch $@
#	$(GUILE_SNARF_DOCS_TEXI) $< > $@ \
#	|| { rm $@; false; }

# Makefile.am ends here
