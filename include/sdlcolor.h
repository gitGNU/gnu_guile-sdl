/*******************************************************************
 *  sdlvideo.h -- SDL Video functions for Guile                    *
 *                                                                 *
 *  Copyright (C) 2001 Alex Shinn                                  *
 *                                                                 *
 *  This program is free software; you can redistribute it and/or  *
 * modify it under the terms of the GNU General Public License as  *
 * published by the Free Software Foundation; either version 2 of  *
 * the License, or (at your option) any later version.             *
 *                                                                 *
 * This program is distributed in the hope that it will be useful, *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of  *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the   *
 * GNU General Public License for more details.                    *
 *                                                                 *
 * You should have received a copy of the GNU General Public       *
 * License along with this program; if not, write to the Free      *
 * Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,  *
 * MA 02111-1307 USA                                               *
 ******************************************************************/

#ifndef _GUILE_SDL_COLOR_H
#define _GUILE_SDL_COLOR_H

/* guile headers */
#include <libguile.h>
/* sdl headers */
#include <SDL/SDL.h>

/* tags for SDL smobs */
extern long color_tag;

/* smob functions */

scm_sizet free_color (SCM s_color);
int print_color (SCM s_color, SCM port, scm_print_state *pstate);

/* functions */
SCM make_color (SCM s_r, SCM s_g, SCM s_b);
SCM color_r (SCM s_color);
SCM color_g (SCM s_color);
SCM color_b (SCM s_color);
SCM color_set_r (SCM s_color, SCM s_r);
SCM color_set_g (SCM s_color, SCM s_g);
SCM color_set_b (SCM s_color, SCM s_b);

/* called to initialize functions and smobs */
void sdl_init_color (void);

#endif /* ! _GUILE_SDL_COLOR_H */
