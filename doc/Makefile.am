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

BUILT_SOURCES = ref.texi ef-ref.texi excuse.texi TODO-excerpt

info_TEXINFOS = guile-sdl.texi
guile_sdl_TEXINFOS = $(BUILT_SOURCES)

SUFFIXES = .texi .twerp

TODO-excerpt: $(top_srcdir)/TODO
	sed '1,/^_/!d;s/__*//' $< > $@

# This causes (one hopes) Automake 1.8 and later to generate rules to
# place the info file in the build tree instead of the source tree.
DISTCLEANFILES = guile-sdl.info

ref.texi: $(top_srcdir)/src/*.c $(top_srcdir)/src/*.scm
ef-ref.texi: $(top_srcdir)/*/*.c

.twerp.texi:
	guile-tools twerp2texi \
	  -I ../src/.doc-index \
	  -o $@ $(TWERP2TEXIFLAGS) $<

excuse.texi: $(srcdir)/notes.tdt
	$(srcdir)/mkexcuse $< > $@

EXTRA_DIST = ref.twerp ef-ref.twerp \
 fdl.texi \
 ChangeLog-cov scan notes.tdt \
 cov mkexcuse $(BUILT_SOURCES)

if MAINTAINER_MODE
TESTS_ENVIRONMENT = srcdir=$(srcdir)
TESTS = cov
endif ## MAINTAINER_MODE

coverage.tdt: api.tdt
	$(srcdir)/scan -o $@ --coverage $<

INCDIR = /usr/include

api.tdt: $(INCDIR)/SDL/*.h
	$(srcdir)/scan -o $@ --api $^

CLEANFILES = api.tdt coverage.tdt

## Makefile.am ends here
