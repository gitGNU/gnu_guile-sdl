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

#ifndef _GUILE_SDL_VIDEO_H
#define _GUILE_SDL_VIDEO_H

/* guile headers */
#include <libguile.h>
/* sdl headers */
#include <SDL/SDL.h>
#include <SDL/SDL_image.h>
#include "sdlrect.h"
#include "sdlcolor.h"
#include "sdlsurface.h"

/* tags for SDL smobs */
extern long cursor_tag;
extern long pixel_format_tag;
extern long overlay_tag;

extern SCM sdl_video_flags;
extern SCM sdl_palette_flags;
extern SCM sdl_alpha_enums;
extern SCM sdl_gl_enums;

scm_sizet free_cursor (SCM s_cursor);
scm_sizet free_yuv_overlay (SCM s_overlay);
scm_sizet free_pixel_format (SCM s_pixel_format);

/* constructors */
SCM create_cursor (SCM s_data, SCM s_mask, SCM s_w, SCM s_h,
                   SCM s_hot_x, SCM s_hot_y);
SCM create_yuv_overlay (SCM s_width, SCM s_height,
                        SCM s_format, SCM s_display);

/* function prototypes */
SCM get_video_surface (void);
SCM get_video_info (void);
SCM video_driver_name (void);
SCM list_modes (SCM s_pixel_format, SCM s_flags);
SCM video_mode_ok (SCM s_width, SCM s_height, SCM s_bpp, SCM s_flags);
SCM update_rect (SCM s_surface, SCM s_x, SCM s_y, SCM s_w, SCM s_h);
SCM update_rects (SCM s_surface, SCM s_rects);
SCM sdl_flip (SCM s_surface);
SCM set_colors (SCM s_surface, SCM s_colors);
SCM set_palette (SCM s_surface, SCM s_flags, SCM s_colors);
SCM set_gamma (SCM s_redgamma, SCM s_greengamma, SCM s_bluegamma);
SCM get_gamma_ramp (void);
SCM set_gamma_ramp (SCM s_redtable, SCM s_greentable, SCM s_bluetable);
SCM map_rgb (SCM s_pixel_fmt, SCM s_r, SCM s_g, SCM s_b);
SCM map_rgba (SCM s_pixel_fmt, SCM s_r, SCM s_g, SCM s_b, SCM s_a);
SCM get_rgb (SCM s_pixel, SCM s_pixel_fmt);
SCM get_rgba (SCM s_pixel, SCM s_pixel_fmt);
SCM display_format (SCM s_surface);
SCM display_format_alpha (SCM s_surface);
SCM warp_mouse (SCM s_x, SCM s_y);
SCM set_cursor (SCM s_cursor);
SCM get_cursor (void);
SCM show_cursor (SCM s_toggle);
SCM gl_load_library (SCM s_path);
SCM gl_get_proc_address (SCM s_proc);
SCM gl_get_attribute (SCM s_attr);
SCM gl_set_attribute (SCM s_attr, SCM s_value);
SCM gl_swap_buffers (void);
SCM lock_yuv_overlay (SCM s_overlay);
SCM unlock_yuv_overlay (SCM s_overlay);
SCM display_yuv_overlay (SCM s_overlay, SCM s_dstrect);

SCM wm_set_caption (SCM title, SCM icon);
SCM wm_get_caption (void);
SCM wm_set_icon (SCM icon);
SCM wm_iconify_window (void);
SCM wm_toggle_full_screen (SCM surface);
SCM wm_grab_input (SCM mode);

/* called to initialize functions and smobs */
void sdl_init_video (void);

#endif /* ! _GUILE_SDL_VIDEO_H */
