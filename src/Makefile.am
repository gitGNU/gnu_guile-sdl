## Process this file with Automake to create Makefile.in

## Copyright (C) 2003, 2004, 2005, 2007, 2008, 2009, 2011 Thien-Thi Nguyen
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
                  sdlenums.c misc.c convert.c
FLAGSTASH       = initf.c kmodf.c videof.c palettef.c overlayf.c evmaskf.c
headers         = guile-sdl.h gperfsup.h modsup.h
scheme          = misc-utils.scm simple.scm

modflags        = -avoid-version -export-dynamic -module

BUILT_SOURCES   = $(WRAPPER_SOURCES:.c=.x) $(FLAGSTASH) sdlgfx.x

cmod_DATA = $(scheme)
noinst_DATA =

EXTRA_DIST = $(FLAGSTASH:.c=.fspec) $(scheme)

sdl_la_SOURCES = $(FLAGSTASH) $(WRAPPER_SOURCES) $(headers)
sdl_la_LDFLAGS = $(modflags)

gfx_la_SOURCES  = sdlgfx.c $(headers)
gfx_la_LDFLAGS  = $(modflags)
gfx_la_LIBADD   = SDL_gfx/libSDL_gfx.la

SUFFIXES = .c .fspec .x

gx		= $(top_srcdir)/build-aux/guile-baux/gbaux-do
snarfcppopts	= $(DEFS) $(DEFAULT_INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS)

.c.x:
	$(c2x) -o $@ $< $(snarfcppopts)

if MAINTAINER_MODE
coding = -c utf-8
c_docscan = $(gx) c-tsar $(coding) -f $@ -m
c_usual_args = update $^ -- $(snarfcppopts)

noinst_DATA += sdl.tsar
sdl.tsar: $(WRAPPER_SOURCES)
	$(c_docscan) '(sdl sdl)' $(c_usual_args)

noinst_DATA += gfx.tsar
gfx.tsar: $(gfx_la_SOURCES)
	$(c_docscan) '(sdl gfx)' $(c_usual_args)

noinst_DATA += scheme.tsar
scheme.tsar: $(scheme)
	$(gx) tsar $(coding) -f $@ update $^
endif # MAINTAINER_MODE

DISTCLEANFILES = *.x
if MAINTAINER_MODE

fspec2c = guile-tools fspec2c

.fspec.c:
	$(fspec2c) -o $@ $<

endif ## MAINTAINER_MODE

SOFIXFLAGS = no-la,no-symlinks

updatemodcat = $(MAKE) $(AM_MAKEFLAGS) update-libsite-module-catalog

install-data-hook:
	cd "$(DESTDIR)$(cmoddir)" ; rm -f sdl-sup.* *.0.0.0
	$(top_srcdir)/am/sofix $(SOFIXFLAGS) \
	  $(DESTDIR)$(cmoddir) $(cmod_LTLIBRARIES)
if HAVE_MKPIMMC
	$(updatemodcat)
endif

uninstall-hook:
	rm -rf $(DESTDIR)$(cmoddir)
if HAVE_MKPIMMC
	$(updatemodcat)
endif

############################################################ begin optional

if HAVE_MIXER

BUILT_SOURCES    += sdlmixer.x
cmod_LTLIBRARIES += mixer.la

mixer_la_SOURCES = sdlmixer.c
mixer_la_LDFLAGS = $(modflags) -lSDL_mixer

if MAINTAINER_MODE
noinst_DATA += mixer.tsar
mixer.tsar: $(mixer_la_SOURCES)
	$(c_docscan) '(sdl mixer)' $(c_usual_args)
endif
endif # HAVE_MIXER

if HAVE_TTF

EXTRA_DIST       += ttff.c
BUILT_SOURCES    += sdlttf.x ttff.c
cmod_LTLIBRARIES += ttf.la

ttf_la_SOURCES = sdlttf.c ttff.c
ttf_la_LDFLAGS = $(modflags) -lSDL_ttf

if MAINTAINER_MODE
noinst_DATA += ttf.tsar
ttf.tsar: $(ttf_la_SOURCES)
	$(c_docscan) '(sdl ttf)' $(c_usual_args)
endif
endif # HAVE_TTF

###################################################### begin module catalog

if HAVE_MKPIMMC
noinst_DATA += .module-catalog

.module-catalog:
	guile-tools mkpimmc -d $(DEPDIR) $@ "$(srcdir)"	\
	  -- -d in,lo,o,git -X '~$$'

clean-local:
	rm -f .module-catalog
	../config.status module-catalog-clean-local

update-libsite-module-catalog:
	guile-tools make-module-catalog -t "$(DESTDIR)"	\
	  "$(DESTDIR)$(GUILE_LIBSITE)"
endif # HAVE_MKPIMMC

## Makefile.am ends here
