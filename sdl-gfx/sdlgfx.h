/*******************************************************************
 *  gfx.h -- Additional Graphics functions for Guile SDL           *
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

#ifndef _GUILE_SDL_GFX_H
#define _GUILE_SDL_GFX_H

/* guile headers */
#include <libguile.h>
/* sdl headers */
#include <SDL/SDL.h>
#include "sdlvideo.h"

/*--------------------Scheme functions-------------------------*/ 

SCM sdl_draw_point (SCM surface_smob, SCM s_x, SCM s_y, 
                    SCM s_color);


SCM sdl_draw_line (SCM surface_smob, 
                   SCM s_x1, SCM s_y1, 
                   SCM s_x2, SCM s_y2, 
                   SCM s_color);

SCM sdl_draw_aa_line (SCM surface_smob, 
                      SCM s_x1, SCM s_y1, 
                      SCM s_x2, SCM s_y2, 
                      SCM s_color);

SCM sdl_draw_rectangle (SCM s_fill, 
                        SCM surface_smob, 
                        SCM s_x1, SCM s_y1, 
                        SCM s_x2, SCM s_y2, 
                        SCM s_color);

SCM sdl_draw_circle (SCM s_fill, 
                     SCM surface_smob, 
                     SCM s_x, SCM s_y, 
                     SCM s_r,
                     SCM s_color);

SCM sdl_draw_ellipse (SCM s_fill, 
                      SCM surface_smob, 
                      SCM s_x, SCM s_y, 
                      SCM s_rx, SCM s_ry,
                      SCM s_color);

SCM sdl_draw_polygon (SCM s_fill, 
                      SCM surface_smob, 
                      SCM s_vx, SCM s_vy, /* Vectors */ 		       
                      SCM s_color);

/* SCM sdl_draw_character (SCM surface_smob,  */
/*                         SCM s_x, SCM s_y, */
/*                         SCM s_char, */
/*                         SCM s_color); */

/* SCM sdl_draw_string (SCM surface_smob,  */
/*                      SCM s_x, SCM s_y, */
/*                      SCM s_string, */
/*                      SCM s_color); */

/*-------------------------------------------------------------*/

void sdl_gfx_init (void);

#endif /* ! _GUILE_SDL_GFX_H */
