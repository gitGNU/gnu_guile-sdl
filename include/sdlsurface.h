/*******************************************************************
 *  sdlsurface.h -- SDL Surface functions                          *
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

#ifndef _GUILE_SDL_SURFACE_H
#define _GUILE_SDL_SURFACE_H

/* guile headers */
#include <libguile.h>
/* sdl headers */
#include <SDL/SDL.h>
#include <SDL/SDL_image.h>

/* tags for SDL smobs */
extern long surface_tag;

#define SMOB_SURFACEP(x) (SCM_NIMP (x)\
			 && (long) SCM_CAR (x) == surface_tag)

size_t free_surface (SCM surface);
int print_surface (SCM surface_smob, SCM port, scm_print_state *pstate);

/* constructors */
SCM sdl_make_surface (SCM s_width, SCM s_height, SCM s_flags);
SCM sdl_create_rgb_surface (SCM s_flags, SCM s_width, SCM s_height,
                            SCM s_depth, SCM s_rmask, SCM s_gmask,
                            SCM s_bmask, SCM s_amask);
/* SCM create_rgb_surface_from (SCM s_pixels, SCM s_width, SCM s_height, */
/*                              SCM s_depth, SCM s_pitch, SCM s_rmask, */
/*                              SCM s_gmask, SCM s_bmask, SCM s_amask); */

/* accessors */

SCM surface_get_w (SCM surface);
SCM surface_get_h (SCM surface);
SCM surface_get_flags (SCM surface);
SCM surface_get_depth (SCM surface);
SCM surface_get_format (SCM surface);

/* utilities */
SCM sdl_surface_p (SCM s_surface);
SCM sdl_lock_surface (SCM s_surface);
SCM sdl_unlock_surface (SCM s_surface);
SCM sdl_load_image (SCM s_file);
SCM sdl_load_bmp (SCM s_file);
SCM sdl_save_bmp (SCM s_surface, SCM s_file);
SCM sdl_set_color_key (SCM s_surface, SCM s_flag, SCM s_key);
SCM sdl_set_alpha (SCM s_surface, SCM s_flag, SCM s_alpha);
SCM sdl_set_clip_rect (SCM s_surface, SCM s_rect);
SCM sdl_get_clip_rect (SCM s_surface);
SCM sdl_convert_surface (SCM s_src, SCM s_pixel_fmt, SCM s_flags);
SCM sdl_blit_surface (SCM s_src, SCM s_srcrect, SCM s_dst, SCM s_dstrect);

/* called to initialize functions and smobs */
void sdl_init_surface (void);

#endif /* ! _GUILE_SDL_SURFACE_H */
