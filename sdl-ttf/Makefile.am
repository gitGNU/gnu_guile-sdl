## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = gnu

AM_CPPFLAGS = -I.. -I. -I$(srcdir) -I$(top_srcdir)/include

EXTRA_DIST = sdlttf.h

cmoddir = $(guilelibsitedir)/sdl

WRAPPER_SOURCES = sdlttf.c
FLAGSTASH       = ttff.c
BUILT_SOURCES   = $(WRAPPER_SOURCES:.c=.x) $(FLAGSTASH)
BUILT_DOCS	= $(WRAPPER_SOURCES:.c=.texi)

if HAVE_TTF
cmod_LTLIBRARIES = ttf.la
ttf_la_SOURCES = $(BUILT_SOURCES) $(WRAPPER_SOURCES)
ttf_la_LDFLAGS = -version-info 0:0:0 -export-dynamic -module -lSDL_ttf
else
cmod_LTLIBRARIES =
ttf_la_SOURCES =
ttf_la_LDFLAGS =
endif

# snarfing
SUFFIXES = .x .texi .doc .fspec
.c.x:
	$(c2x) -o $@ $< $(DEFS) $(AM_CPPFLAGS) $(CPPFLAGS) $(CFLAGS)

fspec2c = guile-tools fspec2c

.fspec.c:
	$(fspec2c) -o $@ $<

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
