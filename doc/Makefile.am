## Process this file with Automake to create Makefile.in

## Copyright (C) 2003, 2004, 2005, 2007, 2008,
##   2009, 2011, 2013 Thien-Thi Nguyen
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

BUILT_SOURCES = ref.texi ef-ref.texi excuse.texi

info_TEXINFOS = guile-sdl.texi
guile_sdl_TEXINFOS = $(BUILT_SOURCES) k-ref.texi

SUFFIXES = .texi .texh

excuse.texi: $(srcdir)/notes.tdt
	$(gx) $(srcdir)/mkexcuse $< > $@

EXTRA_DIST = ref.texh ef-ref.texh \
 fdl.texi \
 ChangeLog-cov scan notes.tdt \
 cov mkexcuse $(BUILT_SOURCES)

if MAINTAINER_MODE
TESTS = cov
endif ## MAINTAINER_MODE

coverage.tdt: api.tdt
	$(gx) $(srcdir)/scan -o $@ --coverage $<

INCDIR = /usr/include

api.tdt: $(INCDIR)/SDL/*.h
	$(gx) $(srcdir)/scan -o $@ --api $^

CLEANFILES = api.tdt coverage.tdt

if MAINTAINER_MODE
gx = $(top_srcdir)/build-aux/guile-baux/gbaux-do
tsopts = -c utf-8 -f snippets

snippets: ../src/*.tsar
	$(gx) tsar $(tsopts) concat $^

.texh.texi:
	$(gx) tsin $(tsopts) $<

ref.texi: snippets
ef-ref.texi: snippets

CLEANFILES += snippets

# k

k-ref.texi: ../src/k-stamp k
	$(gx) $(srcdir)/kls -o $@ `echo $(srcdir)/k/*`

EXTRA_DIST += k kls

endif ## MAINTAINER_MODE

## Makefile.am ends here
