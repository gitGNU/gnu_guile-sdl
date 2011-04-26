## Process this file with Automake to create Makefile.in

## Copyright (C) 2003, 2004, 2005, 2007, 2008, 2009 Thien-Thi Nguyen
##
## This program is free software; you can redistribute it and/or
## modify it under the terms of the GNU General Public License as
## published by the Free Software Foundation; either version 3 of
## the License, or (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public
## License along with this program; if not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
## Boston, MA  02110-1301  USA

SUBDIRS = SDL_gfx

cmoddir = $(GUILE_LIBSITE)/sdl

cmod_LTLIBRARIES = sdl.la gfx.la

WRAPPER_SOURCES = sdl.c sdlvideo.c sdlsurface.c sdlcdrom.c       \
                  sdlrect.c sdlcolor.c sdljoystick.c sdlevent.c  \
                  sdlenums.c misc.c
FLAGSTASH       = initf.c kmodf.c videof.c palettef.c overlayf.c evmaskf.c
headers         = guile-sdl.h gperfsup.h modsup.h
scheme          = misc-utils.scm simple.scm

modflags        = -avoid-version -export-dynamic -module

BUILT_SOURCES   = $(WRAPPER_SOURCES:.c=.x) $(FLAGSTASH) sdlgfx.x

cmod_DATA = $(scheme)

EXTRA_DIST = $(FLAGSTASH:.c=.fspec) $(scheme)

sdl_la_SOURCES = $(FLAGSTASH) $(WRAPPER_SOURCES) $(headers)
sdl_la_LDFLAGS = $(modflags)

gfx_la_SOURCES  = sdlgfx.c $(headers)
gfx_la_LDFLAGS  = $(modflags)
gfx_la_LIBADD   = SDL_gfx/libSDL_gfx.la

SUFFIXES = .c .fspec .x .doc

snarfcppopts	= $(DEFS) -I. $(AM_CPPFLAGS) $(CPPFLAGS)
c2doc		= guile-tools c2doc
scm2doc		= guile-tools doc-snarf -D
mkdocindex	= guile-tools make-twerp2texi-index
dotdocfiles	= $(WRAPPER_SOURCES:.c=.doc) sdlgfx.doc $(scheme:.scm=.doc)

.c.x:
	$(c2x) -o $@ $< $(snarfcppopts)

.c.doc:
	$(c2doc) -o $@ $< -- $(snarfcppopts)

.scm.doc:
	$(scm2doc) -o $@ $<

.doc-index: $(dotdocfiles)
	$(mkdocindex) -o $@ $(dotdocfiles)

DISTCLEANFILES = *.x
if MAINTAINER_MODE

fspec2c = guile-tools fspec2c

.fspec.c:
	$(fspec2c) -o $@ $<

noinst_DATA	= .doc-index
MAINTAINERCLEANFILES = .doc-index *.doc
endif ## MAINTAINER_MODE

SOFIXFLAGS = no-la,no-symlinks

install-data-hook:
	cd "$(DESTDIR)$(cmoddir)" ; rm -f sdl-sup.* *.0.0.0
	$(top_srcdir)/am/sofix $(SOFIXFLAGS) \
	  $(DESTDIR)$(cmoddir) $(cmod_LTLIBRARIES)

uninstall-hook:
	rm -rf $(DESTDIR)$(cmoddir)

############################################################ begin optional

if HAVE_MIXER

BUILT_SOURCES    += sdlmixer.x
dotdocfiles      += sdlmixer.doc
cmod_LTLIBRARIES += mixer.la

mixer_la_SOURCES = sdlmixer.c
mixer_la_LDFLAGS = $(modflags) -lSDL_mixer

endif # HAVE_MIXER

if HAVE_TTF

EXTRA_DIST       += ttff.c
BUILT_SOURCES    += sdlttf.x ttff.c
dotdocfiles      += sdlttf.doc
cmod_LTLIBRARIES += ttf.la

ttf_la_SOURCES = sdlttf.c ttff.c
ttf_la_LDFLAGS = $(modflags) -lSDL_ttf

endif # HAVE_TTF

## Makefile.am ends here
