## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = gnu

PACKAGE = @PACKAGE@
VERSION = @VERSION@

# User-definable stuff:
CC=@CC@
CFLAGS  = @CFLAGS@ -Wall
LIBS    = @LIBS@
INCLUDES = -I../include/ -ISDL_gfxPrimitives/

SUBDIRS = SDL_gfxPrimitives

EXTRA_DIST = sdlgfx.h

WRAPPER_SOURCES = sdlgfx.c
BUILT_SOURCES   = $(WRAPPER_SOURCES:.c=.x)

lib_LTLIBRARIES = libguileSDL_gfx.la

libguileSDL_gfx_la_SOURCES = $(BUILT_SOURCES) $(WRAPPER_SOURCES)

libguileSDL_gfx_la_CFLAGS  = $(CFLAGS)

libguileSDL_gfx_la_LIBADD  =  SDL_gfxPrimitives/libSDL_gfxPrimitives.la

libguileSDL_gfx_la_LDFLAGS =             \
        -release $(LT_RELEASE)  \
        -version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE)

# snarfing
GUILE_SNARF = guile-snarf

SUFFIXES = .x
%.x: %.c
	$(GUILE_SNARF) $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $< > $@ \
	|| { rm $@; false; }

DISTCLEANFILES = *.x

# docs
TEXINFO_TEX = ../doc/texinfo.tex
info_TEXINFOS = dummy.texi
dummy_TEXINFOS = sdlgfx.texi

# doc snarfing
GUILE_SNARF_DOCS = guile-snarf-docs
GUILE_SNARF_DOCS_TEXI = guile-snarf-docs-texi

SUFFIXES = .texi .doc

%.doc: %.c
	$(GUILE_SNARF_DOCS) $(DEFS) $(INCLUDES) -I../include \
	$(CPPFLAGS) $(CFLAGS) $< > $@ \
	|| { rm $@; false; }

.doc.texi:
	$(GUILE_SNARF_DOCS_TEXI) $< > $@ \
	|| { rm $@; false; }

