/*******************************************************************
 *  sdlrect.h -- SDL Rect functions for Guile                      *
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

#ifndef _GUILE_SDL_RECT_H
#define _GUILE_SDL_RECT_H

/* guile headers */
#include <libguile.h>
/* sdl headers */
#include <SDL/SDL.h>

/* tags for SDL smobs */
extern long rect_tag;

size_t free_rect (SCM rect);
int print_rect (SCM rect_smob, SCM port, scm_print_state *pstate);

/* functions */
SCM make_rect (SCM s_x, SCM s_y, SCM s_w, SCM s_h);
SCM rect_x (SCM s_rect);
SCM rect_y (SCM s_rect);
SCM rect_w (SCM s_rect);
SCM rect_h (SCM s_rect);
SCM rect_set_x (SCM s_rect, SCM s_x);
SCM rect_set_y (SCM s_rect, SCM s_y);
SCM rect_set_w (SCM s_rect, SCM s_w);
SCM rect_set_h (SCM s_rect, SCM s_h);

/* called to initialize functions and smobs */
void sdl_init_rect (void);

#endif /* ! _GUILE_SDL_RECT_H */
