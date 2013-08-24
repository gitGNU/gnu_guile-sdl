## Process this file with Automake to create Makefile.in

# Copyright (C) 2013 Thien-Thi Nguyen
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 3 of
# the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public
# License along with this program; if not, write to the Free
# Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
# Boston, MA  02110-1301  USA

stdf =						\
appstate.kf					\
evmask.kf					\
init.kf						\
jhpos.kf					\
kmod.kf						\
mb.kf						\
overlay.kf					\
palette.kf					\
video.kf

stdp =						\
active.kp					\
alphalim.kp					\
alphalimold.kp					\
cdstate.kp					\
cdtracktype.kp					\
evaction.kp					\
evstate.kp					\
evtype.kp					\
frot.kp						\
grabmode.kp					\
keysym.kp					\
mbut.kp						\
updn.kp

BUILT_SOURCES = $(stdf:.kf=.c) $(stdp:.kp=.c)

EXTRA_DIST = $(stdf) $(stdp)

if HAVE_TTF
stdf += ttf.kf
EXTRA_DIST += ttf.kf
endif

if HAVE_MIXER
stdp += fading.kp
EXTRA_DIST += fading.kp
endif

SUFFIXES = .c .kf .kp

gx = $(top_srcdir)/build-aux/guile-baux/gbaux-do

.kf.c:
	$(gx) k2c -o $@ $< $(SDLMINUSI)

.kp.c:
	$(gx) k2c -o $@ $< $(SDLMINUSI)

CLEANFILES = $(BUILT_SOURCES)
CLEANFILES += *.list

## Makefile.am ends here
