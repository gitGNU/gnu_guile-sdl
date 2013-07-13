# ttf.sed                                 -*- shell-script -*-

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

s/TTF_STYLE_NORMAL/normal/g
s/TTF_STYLE_BOLD/bold/g
s/TTF_STYLE_ITALIC/italic/g
s/TTF_STYLE_UNDERLINE/underline/g

# ttf.sed ends here
