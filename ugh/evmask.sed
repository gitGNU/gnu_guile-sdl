# evmask.sed                                 -*- shell-script -*-

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

s/SDL_ACTIVEEVENTMASK/active/g
s/SDL_KEYDOWNMASK/key-down/g
s/SDL_KEYUPMASK/key-up/g
s/SDL_KEYEVENTMASK/key/g
s/SDL_MOUSEMOTIONMASK/mouse-motion/g
s/SDL_MOUSEBUTTONDOWNMASK/mouse-button-down/g
s/SDL_MOUSEBUTTONUPMASK/mouse-button-up/g
s/SDL_MOUSEEVENTMASK/mouse/g
s/SDL_JOYAXISMOTIONMASK/joy-axis-motion/g
s/SDL_JOYBALLMOTIONMASK/joy-ball-motion/g
s/SDL_JOYHATMOTIONMASK/joy-hat-motion/g
s/SDL_JOYBUTTONDOWNMASK/joy-button-down/g
s/SDL_JOYBUTTONUPMASK/joy-button-up/g
s/SDL_JOYEVENTMASK/joy/g
s/SDL_QUITMASK/quit/g
s/SDL_SYSWMEVENTMASK/sys-wm/g
s/SDL_VIDEORESIZEMASK/video-resize/g
s/SDL_VIDEOEXPOSEMASK/video-expose/g

# evmask.sed ends here
