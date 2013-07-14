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

ugh5dir = $(pkgdatadir)/0.5

dist_ugh5_DATA = README
dist_ugh5_DATA += overlay.sed
dist_ugh5_DATA += ttf.sed
dist_ugh5_DATA += palette.sed
dist_ugh5_DATA += kmod.sed
dist_ugh5_DATA += init.sed
dist_ugh5_DATA += evmask.sed
dist_ugh5_DATA += evtype.sed
dist_ugh5_DATA += cdstate.sed
dist_ugh5_DATA += video.sed
dist_ugh5_DATA += keysym.sed

uninstall-hook:
	-rmdir $(ugh5dir)
	-rmdir $(pkgdatadir)

## Makefile.am ends here
