## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = gnu

SUBDIRS = SDL_gfx

include $(top_srcdir)/am/common

cmod_LTLIBRARIES = sdl.la gfx.la

WRAPPER_SOURCES = sdl.c sdlvideo.c sdlsurface.c sdlcdrom.c       \
                  sdlrect.c sdlcolor.c sdljoystick.c sdlevent.c  \
                  sdlenums.c misc.c
FLAGSTASH       = initf.c kmodf.c videof.c palettef.c overlayf.c evmaskf.c
BUILT_SOURCES   = $(WRAPPER_SOURCES:.c=.x) $(FLAGSTASH) sdlgfx.x

EXTRA_DIST = $(FLAGSTASH:.c=.fspec)		\
 argcheck.h					\
 bool.h						\
 gperfsup.h					\
 modsup.h					\
 retval.h					\
 sdlenums.h					\
 sdlsmobs.h					\
 sdlvideo.h					\
 sym.h						\
 wholefns.h

sdl_la_SOURCES = $(FLAGSTASH) $(WRAPPER_SOURCES)
sdl_la_LDFLAGS = -version-info 0:0:0 -export-dynamic -module

gfx_la_SOURCES  = sdlgfx.c
gfx_la_LDFLAGS  = -version-info 0:0:0 -export-dynamic -module \
   -LSDL_gfx -lSDL_gfx

SUFFIXES += .c .fspec .x .doc

snarfcppopts	= $(DEFS) -I. -I.. $(AM_CPPFLAGS) $(CPPFLAGS)
c2doc		= guile-tools c2doc
mkdocindex	= guile-tools make-twerp2texi-index
dotdocfiles	= $(WRAPPER_SOURCES:.c=.doc) sdlgfx.doc

.c.x:
	$(c2x) -o $@ $< $(snarfcppopts)

.c.doc:
	$(c2doc) -o $@ $< -- $(snarfcppopts)

.doc-index: $(dotdocfiles)
	$(mkdocindex) -o $@ $(dotdocfiles)

DISTCLEANFILES	+= *.x
if MAINTAINER_MODE

fspec2c = guile-tools fspec2c

.fspec.c:
	$(fspec2c) -o $@ $<

noinst_DATA	+= .doc-index
MAINTAINERCLEANFILES = .doc-index *.doc
endif ## MAINTAINER_MODE

SOFIXFLAGS = no-la,no-symlinks

install-data-hook:
	$(top_srcdir)/am/sofix $(SOFIXFLAGS) \
	  $(DESTDIR)$(cmoddir) $(cmod_LTLIBRARIES)
	rm -f $(cmoddir)/sdl-sup.*

############################################################ begin optional

if HAVE_MIXER

BUILT_SOURCES    += sdlmixer.x
dotdocfiles      += sdlmixer.doc
cmod_LTLIBRARIES += mixer.la

mixer_la_SOURCES = sdlmixer.c
mixer_la_LDFLAGS = -version-info 0:0:0 -export-dynamic -module -lSDL_mixer

endif # HAVE_MIXER

if HAVE_TTF

EXTRA_DIST       += ttff.c
BUILT_SOURCES    += sdlttf.x ttff.c
dotdocfiles      += sdlttf.doc
cmod_LTLIBRARIES += ttf.la

ttf_la_SOURCES = sdlttf.c ttff.c
ttf_la_LDFLAGS = -version-info 0:0:0 -export-dynamic -module -lSDL_ttf

endif # HAVE_TTF

## Makefile.am ends here
