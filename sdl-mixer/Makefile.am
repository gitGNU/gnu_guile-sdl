## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = gnu

PACKAGE = @PACKAGE@
VERSION = @VERSION@

# User-definable stuff:
CC=@CC@
CFLAGS   = @CFLAGS@ -Wall
LIBS     = @LIBS@
INCLUDES = -I../include/

EXTRA_DIST = sdlmixer.h

WRAPPER_SOURCES = sdlmixer.c
BUILT_SOURCES   = $(WRAPPER_SOURCES:.c=.x)
BUILT_DOCS      = $(WRAPPER_SOURCES:.c=.texi)

if HAVE_MIXER
lib_LTLIBRARIES = libguileSDL_mixer.la
libguileSDL_mixer_la_SOURCES = $(BUILT_SOURCES) $(WRAPPER_SOURCES)
libguileSDL_mixer_la_CFLAGS  = $(CFLAGS)
else
lib_LTLIBRARIES =
libguileSDL_mixer_la_SOURCES =
libguileSDL_mixer_la_CFLAGS  =
endif

libguileSDL_mixer_la_DEPENDENCIES = $(BUILT_DOCS)
libguileSDL_mixer_la_LDFLAGS =             \
        -release $(LT_RELEASE)  \
        -version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE)

# snarfing
GUILE_SNARF = guile-snarf

SUFFIXES = .x .texi .doc
%.x: %.c
	$(GUILE_SNARF) $< $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) > $@ \
	|| { rm $@; false; }

DISTCLEANFILES = *.x

# doc snarfing
GUILE_SNARF_DOCS = guile-snarf-docs
GUILE_SNARF_DOCS_TEXI = guile-snarf-docs-texi

%.doc: %.c
	$(GUILE_SNARF_DOCS) $(DEFS) $(INCLUDES) -I../include \
	$(CPPFLAGS) $(CFLAGS) $< > $@ \
	|| { rm $@; false; }

.doc.texi:
	touch $@
#	$(GUILE_SNARF_DOCS_TEXI) $< > $@ \
#	|| { rm $@; false; }

