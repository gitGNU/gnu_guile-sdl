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

ACLOCAL_AMFLAGS = -I build-aux

SUBDIRS = src doc

noinst_DATA = .module-catalog

.module-catalog:
	if guile-tools | grep -q mkpimmc ; then			\
	  guile-tools mkpimmc -d $(DEPDIR) $@ "$(srcdir)"	\
	    -- -d in,lo,o,git					\
	       -X '~$$' -X 'doc/' -X '/test/' ;			\
	else echo '()' > $@ ; fi

CLEANFILES = .module-catalog
clean-local:
	./config.status module-catalog-clean-local

install-data-hook: update-libsite-module-catalog
uninstall-hook:    update-libsite-module-catalog

update-libsite-module-catalog:
	if guile-tools | grep -q make-module-catalog ; then	\
	   guile-tools make-module-catalog -t "$(DESTDIR)"	\
	     "$(DESTDIR)$(GUILE_LIBSITE)" ;			\
	fi

# test suite
TESTS = test/alive.test				\
	test/leak.scm				\
	test/rect.scm				\
	test/image.scm				\
	test/event.scm				\
	test/mixer.scm				\
	test/ttf.scm				\
	test/cursor.scm				\
	test/roto.scm				\
	test/gfx.scm				\
	test/fading.scm

TESTS_ENVIRONMENT = \
	HAVE_TTF=$(HAVE_TTF) \
	HAVE_MIXER=$(HAVE_MIXER) \
	$(GUILE) -L "@abs_top_builddir@" -l $(srcdir)/test/common -s

# distribution
as_is_dirs = test

EXTRA_DIST = HISTORY \
	build-aux/guile-sdl.m4 \
	build-aux/sofix \
	$(as_is_dirs)

## Makefile.am ends here
