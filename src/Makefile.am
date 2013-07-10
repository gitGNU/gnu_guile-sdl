## Process this file with Automake to create Makefile.in

## Copyright (C) 2003, 2004, 2005, 2007, 2008, 2009,
##               2011, 2012, 2013 Thien-Thi Nguyen
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

SUBDIRS = SDL_gfx k

AM_CPPFLAGS = $(SDL_CFLAGS)
AM_CFLAGS = $(NORMAL_CFLAGS)

cmoddir = $(GUILE_LIBSITE)/sdl

WRAPPER_SOURCES = sdl.c sdlvideo.c sdlsurface.c sdlcdrom.c       \
                  sdlrect.c sdlcolor.c sdljoystick.c sdlevent.c  \
                  sdlenums.c misc.c
headers         = guile-sdl.h b-uv.h b-values.h
scheme          = misc-utils.scm simple.scm

modflags        = -avoid-version -export-dynamic -module

BUILT_SOURCES   = k-stamp k-count.h $(WRAPPER_SOURCES:.c=.x) sdlgfx.x

k-stamp: k
	cd k && $(MAKE)
	cat k/*.list > $@

k-count.h: k-stamp
	printf '#define KOTK_N  %d\n' \
	  `ls -l k/*.c | wc -l` \
	> $@

cmod_DATA = $(scheme)
noinst_DATA =

if LAME
genwrap = $(gx) gen-scheme-wrapper --dlname \
	  -o $@ $< -g sdl -t scm_init_~A_module \
	  -i '$(pkglibdir)'

basic = sdl gfx
CLEANFILES = $(basic) $(basic:=.exports)
cmod_DATA += $(basic)
pkglib_LTLIBRARIES = $(basic:=.la)

sdl.exports: sdl.tsar $(WRAPPER_SOURCES)
	{ $(SED) '/^.[0-9]* [0-9]* [.] /!d;s///;s/.$$//' $^ ;	\
	  $(SED) '/.*DEFINE_ENUM ."/!d;s///;s/".*//' $^ ;	\
	} > $@

gfx.exports: sdlgfx.c
	> $@ \
	$(SED) '/PRIMPROC/,/^(/!d;/^PRIMPROC/d;s/^.*"\(.*\)".*/\1/' $^

sdl: sdl.la sdl.exports
	$(genwrap)

gfx: gfx.la gfx.exports
	$(genwrap)

else
cmod_LTLIBRARIES = sdl.la gfx.la
endif

EXTRA_DIST = $(scheme)
EXTRA_DIST += snuggle

sdlimpl = $(WRAPPER_SOURCES)
sdl_la_SOURCES = $(sdlimpl) $(headers)
sdl_la_LDFLAGS = $(modflags)

gfx_la_SOURCES  = sdlgfx.c $(headers)
gfx_la_LDFLAGS  = $(modflags)
gfx_la_LIBADD   = SDL_gfx/libSDL_gfx.la \
                  SDL_gfx/libimfi.la

SUFFIXES = .c

gx		= $(top_srcdir)/build-aux/guile-baux/gbaux-do
snarfcppopts	= $(DEFS) $(DEFAULT_INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS)

.c.x:
	$(gx) c2x -o $@ $< -- $(snarfcppopts)

if MAINTAINER_MODE
coding = -c utf-8
c_docscan = $(gx) c-tsar $(coding) -f $@ -m
c_usual_args = update $^ -- $(snarfcppopts)

noinst_DATA += sdl.tsar
sdl.tsar: $(sdlimpl)
	$(c_docscan) '(sdl sdl)' $(c_usual_args)

noinst_DATA += gfx.tsar
gfx.tsar: $(gfx_la_SOURCES)
	$(c_docscan) '(sdl gfx)' $(c_usual_args)

noinst_DATA += scheme.tsar
scheme.tsar: $(scheme)
	$(gx) tsar $(coding) -f $@ update $^

# Blech, we need to distribute this to generate the exports file.
EXTRA_DIST += sdl.tsar
endif # MAINTAINER_MODE

DISTCLEANFILES = *.x k-count.h

bx = $(top_srcdir)/build-aux
if LAME
soargs = $(DESTDIR)$(pkglibdir) $(pkglib_LTLIBRARIES)
else !LAME
soargs = $(DESTDIR)$(cmoddir) $(cmod_LTLIBRARIES)
endif !LAME

SOFIXFLAGS = no-la,no-symlinks

updatemodcat = $(MAKE) $(AM_MAKEFLAGS) update-libsite-module-catalog

install-data-hook:
	$(bx)/sofix $(SOFIXFLAGS) $(soargs)
if LAME
	@if [ 1 = '$(FOR_MAKE_CHECK)' ] ; then		\
	  for la in $(pkglib_LTLIBRARIES) ; do 		\
	    b=`basename $$la .la` ;			\
	    mv -f $$b $$b.STASH ;			\
	    $(MAKE) $(AM_MAKEFLAGS) $$b ;		\
	    $(INSTALL_DATA) $$b '$(cmoddir)' ;		\
	    mv -f $$b.STASH $$b ;			\
	  done ;					\
	fi
else !LAME
	-rmdir '$(DESTDIR)$(pkglibdir)'
	$(updatemodcat)
endif !LAME

uninstall-hook:
	$(bx)/uninstall-sofixed $(soargs)
if !LAME
	$(updatemodcat)
endif !LAME
	-rmdir '$(DESTDIR)$(cmoddir)'
	-rmdir '$(DESTDIR)$(pkglibdir)'

############################################################ begin optional

if HAVE_MIXER

BUILT_SOURCES    += sdlmixer.x
if LAME
cmod_DATA += mixer
pkglib_LTLIBRARIES += mixer.la
CLEANFILES += mixer mixer.exports

mixer.exports: sdlmixer.c
	> $@ \
	$(SED) '/PRIMPROC/,/^(/!d;/^PRIMPROC/d;s/^.*"\(.*\)".*/\1/' $^

mixer: mixer.la mixer.exports
	$(genwrap)

else
cmod_LTLIBRARIES += mixer.la
endif

mixer_la_SOURCES = sdlmixer.c
mixer_la_LDFLAGS = $(modflags) -lSDL_mixer

if MAINTAINER_MODE
noinst_DATA += mixer.tsar
mixer.tsar: $(mixer_la_SOURCES)
	$(c_docscan) '(sdl mixer)' $(c_usual_args)
endif
endif # HAVE_MIXER

if HAVE_TTF

BUILT_SOURCES    += sdlttf.x
if LAME
cmod_DATA += ttf
pkglib_LTLIBRARIES += ttf.la
CLEANFILES += ttf ttf.exports

ttf.exports: sdlttf.c
	> $@ \
	$(SED) '/PRIMPROC/,/^(/!d;/^PRIMPROC/d;s/^.*"\(.*\)".*/\1/' $^

ttf: ttf.la ttf.exports
	$(genwrap)

else
cmod_LTLIBRARIES += ttf.la
endif

ttf_la_SOURCES = sdlttf.c
ttf_la_LDFLAGS = $(modflags) -lSDL_ttf

if MAINTAINER_MODE
noinst_DATA += ttf.tsar
ttf.tsar: $(ttf_la_SOURCES)
	$(c_docscan) '(sdl ttf)' $(c_usual_args)
endif
endif # HAVE_TTF

group-stamp: $(cmod_LTLIBRARIES) $(pkglib_LTLIBRARIES)
	@echo $^ > $@
noinst_DATA += group-stamp

###################################################### begin module catalog

update-libsite-module-catalog:
	$(GUILE_TOOLS) make-module-catalog -t "$(DESTDIR)"	\
	  "$(DESTDIR)$(GUILE_LIBSITE)"

####################################################################### etc

if MAINTAINER_MODE
CLEANFILES = *.tsar
endif # MAINTAINER_MODE

## Makefile.am ends here
