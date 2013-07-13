# evtype.sed                                 -*- shell-script -*-

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

s/SDL_ACTIVEEVENT/active/g
s/SDL_KEYDOWN/key-down/g
s/SDL_KEYUP/key-up/g
s/SDL_MOUSEMOTION/mouse-motion/g
s/SDL_MOUSEBUTTONDOWN/mouse-button-down/g
s/SDL_MOUSEBUTTONUP/mouse-button-up/g
s/SDL_JOYAXISMOTION/joy-axis-motion/g
s/SDL_JOYBALLMOTION/joy-ball-motion/g
s/SDL_JOYHATMOTION/joy-hat-motion/g
s/SDL_JOYBUTTONDOWN/joy-button-down/g
s/SDL_JOYBUTTONUP/joy-button-up/g
s/SDL_QUIT/quit/g
s/SDL_SYSWMEVENT/sys-wm/g
s/SDL_VIDEORESIZE/video-resize/g
s/SDL_VIDEOEXPOSE/video-expose/g

# evtype.sed
