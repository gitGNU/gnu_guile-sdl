## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = gnu

AM_CPPFLAGS = -I.. -I. -I$(srcdir) -I$(top_srcdir)/include \
  -I$(top_srcdir)/sdl-gfx/SDL_gfxPrimitives \
  -I$(srcdir)/SDL_rotozoom

SUBDIRS = SDL_rotozoom

cmoddir = $(guilelibsitedir)/sdl
cmod_LTLIBRARIES = sdl-sup.la

WRAPPER_SOURCES = sdl.c sdlvideo.c sdlsurface.c sdlcdrom.c       \
                  sdlrect.c sdlcolor.c sdljoystick.c sdlevent.c  \
                  sdlenums.c sdlroto.c sdlgfx.c
FLAGSTASH       = initf.c kmodf.c videof.c palettef.c overlayf.c
BUILT_SOURCES   = $(WRAPPER_SOURCES:.c=.x) $(FLAGSTASH)
BUILT_DOCS      = $(WRAPPER_SOURCES:.c=.texi)

sdl_sup_la_SOURCES = $(BUILT_SOURCES) $(WRAPPER_SOURCES)
sdl_sup_la_LDFLAGS = -version-info 0:0:0 -export-dynamic -module \
	-LSDL_rotozoom -lrotozoom

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
GUILE_SNARF_DOCS_TEXI = snarf-check-and-output-texi

%.doc: %.c
	$(GUILE_SNARF_DOCS) $< $(DEFS) $(AM_CPPFLAGS) -I../include \
	$(CPPFLAGS) $(CFLAGS) > $@ \
	|| { rm $@; false; }

.doc.texi:
	touch $@
#	cat $< | $(GUILE_SNARF_DOCS_TEXI) > $@ \
#	|| { rm $@; false; }

# Makefile.am ends here
