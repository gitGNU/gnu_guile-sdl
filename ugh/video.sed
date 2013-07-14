# video.sed                                 -*- shell-script -*-

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

s/SDL_SWSURFACE/sw-surface/g
s/SDL_HWSURFACE/hw-surface/g
s/SDL_OPENGL/opengl/g
s/SDL_ASYNCBLIT/async-blit/g
s/SDL_OPENGLBLIT/opengl-blit/g
s/SDL_RESIZABLE/resizable/g
s/SDL_NOFRAME/no-frame/g
s/SDL_HWACCEL/hw-accel/g
s/SDL_SRCCOLORKEY/src-colorkey/g
s/SDL_RLEACCELOK/rle-accel-ok/g
s/SDL_RLEACCEL/rle-accel/g
s/SDL_SRCALPHA/src-alpha/g
s/SDL_PREALLOC/prealloc/g
s/SDL_ANYFORMAT/any-format/g
s/SDL_HWPALETTE/hw-palette/g
s/SDL_DOUBLEBUF/doublebuf/g
s/SDL_FULLSCREEN/fullscreen/g

# video.sed ends here
