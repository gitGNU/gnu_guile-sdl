## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = gnu

PACKAGE = @PACKAGE@
VERSION = @VERSION@

# User-definable stuff:
INCLUDES = -I../include/ -ISDL_gfxPrimitives/ -ISDL_rotozoom/
CC=@CC@
CFLAGS  = @CFLAGS@ -Wall
LIBS    = @LIBS@

SUBDIRS = SDL_rotozoom

lib_LTLIBRARIES = libguileSDL.la

WRAPPER_SOURCES = sdl.c sdlvideo.c sdlsurface.c sdlcdrom.c       \
                  sdlrect.c sdlcolor.c sdljoystick.c sdlevent.c  \
                  sdlenums.c sdlroto.c
BUILT_SOURCES   = $(WRAPPER_SOURCES:.c=.x)
BUILT_DOCS      = $(WRAPPER_SOURCES:.c=.texi)

libguileSDL_la_SOURCES = $(BUILT_SOURCES) $(WRAPPER_SOURCES)

libguileSDL_la_CFLAGS  = $(CFLAGS)

libguileSDL_la_LIBADD  =  SDL_rotozoom/librotozoom.la

libguileSDL_la_LDFLAGS =             \
        -release $(LT_RELEASE)  \
        -version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE)

libguileSDL_la_DEPENDENCIES = sdl.texi sdlevent.texi sdlroto.texi \
			      sdlsurface.texi sdlvideo.texi sdlcdrom.texi \
		  	      sdljoystick.texi

# snarfing
GUILE_SNARF = guile-snarf

SUFFIXES = .x .texi .doc
%.x: %.c
	$(GUILE_SNARF) $< $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) > $@ \
	|| { rm $@; false; }

DISTCLEANFILES = *.x

# doc snarfing
GUILE_SNARF_DOCS = guile-snarf-docs
GUILE_SNARF_DOCS_TEXI = snarf-check-and-output-texi

%.doc: %.c
	$(GUILE_SNARF_DOCS) $< $(DEFS) $(INCLUDES) -I../include \
	$(CPPFLAGS) $(CFLAGS) > $@ \
	|| { rm $@; false; }

.doc.texi:
	touch $@
#	cat $< | $(GUILE_SNARF_DOCS_TEXI) > $@ \
#	|| { rm $@; false; }
