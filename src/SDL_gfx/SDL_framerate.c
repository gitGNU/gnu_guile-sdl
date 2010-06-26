/* SDL_framerate: framerate manager

   Copyright (C) 2008 Andreas Schiffler

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*/

#include "SDL_framerate.h"

/*
   Initialize the framerate manager
*/

void SDL_initFramerate(FPSmanager * manager)
{
    /*
     * Store some sane values
     */
    manager->framecount = 0;
    manager->rate = FPS_DEFAULT;
    manager->rateticks = (1000.0 / (float) FPS_DEFAULT);
    manager->lastticks = SDL_GetTicks();
}

/*
   Set the framerate in Hz
*/

int SDL_setFramerate(FPSmanager * manager, int rate)
{
    if ((rate >= FPS_LOWER_LIMIT) && (rate <= FPS_UPPER_LIMIT)) {
	manager->framecount = 0;
	manager->rate = rate;
	manager->rateticks = (1000.0 / (float) rate);
	return (0);
    } else {
	return (-1);
    }
}

/*
  Return the current target framerate in Hz
*/

int SDL_getFramerate(FPSmanager * manager)
{
    if (manager == NULL) {
	return (-1);
    } else {
	return (manager->rate);
    }
}

/*
  Delay execution to maintain a constant framerate. Calculate fps.
*/

void SDL_framerateDelay(FPSmanager * manager)
{
    Uint32 current_ticks;
    Uint32 target_ticks;
    Uint32 the_delay;

    /*
     * Next frame
     */
    manager->framecount++;

    /*
     * Get/calc ticks
     */
    current_ticks = SDL_GetTicks();
    target_ticks = manager->lastticks + (Uint32) ((float) manager->framecount * manager->rateticks);

    if (current_ticks <= target_ticks) {
	the_delay = target_ticks - current_ticks;
	SDL_Delay(the_delay);
    } else {
	manager->framecount = 0;
	manager->lastticks = SDL_GetTicks();
    }
}
