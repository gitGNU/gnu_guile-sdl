## Copyright (C) 2011 Thien-Thi Nguyen
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

XFAIL_TESTS =
TESTS = alive.test				\
	leak.scm				\
	rect.scm				\
	image.scm				\
	event.scm				\
	mixer.scm				\
	ttf.scm					\
	cursor.scm				\
	roto.scm				\
	gfx.scm					\
	fading.scm

TESTS_ENVIRONMENT = \
	HAVE_TTF=$(HAVE_TTF) \
	HAVE_MIXER=$(HAVE_MIXER) \
	sh zow

noises = background.ogg fx.ogg fx.wav
images = gnu-goatee.jpg
etcetc = FreeSansBold.ttf FreeSerifBoldItalic.ttf

EXTRA_DIST = common $(TESTS) \
 $(noises) \
 $(images) \
 $(etcetc)

simu = .simu
check_DATA = $(simu)

$(simu): ../src/group-stamp
	top=`pwd`/$@ ;				\
	rm -rf $$top ;				\
	cd ../src ;				\
	$(MAKE) $(AM_MAKEFLAGS) install		\
	  FOR_MAKE_CHECK=1			\
	  prefix="$$top"

if MAINTAINER_MODE
CLEANFILES = t99.list
check_DATA += t99.list
XFAIL_TESTS += t99-cov
TESTS += t99-cov

t99.list: ../doc/snippets
	> $@ sed -f $(top_srcdir)/build-aux/tsar-names.sed $<
endif MAINTAINER_MODE

clean-local:
	-rm -rf $(simu)

installcheck-local:
	$(MAKE) $(AM_MAKEFLAGS) check GUILE_LIBSITE='$(GUILE_LIBSITE)'

## Makefile.am ends here
