/*******************************************************************
 *  video.h -- SDL Video functions for Guile                       *
 *                                                                 *
 *  Created:    <2001-04-24 23:40:20 foof>                         *
 *  Time-stamp: <2001-06-30 01:19:13 foof>                         *
 *  Author:     Alex Shinn <foof@debian.org>                       *
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

/* tags for SDL smobs */
extern long surface_tag;
extern long cursor_tag;
extern long rect_tag;
extern long color_tag;
extern long palette_tag;
extern long pixel_format_tag;
extern long overlay_tag;
extern long video_info_tag;

#define SMOB_SURFACEP(x) (SCM_NIMP (x)\
			 && (long) SCM_CAR (x) == surface_tag)

scm_sizet free_surface (SCM surface);
scm_sizet free_rect (SCM rect);
scm_sizet free_color (SCM color);
scm_sizet free_cursor (SCM s_cursor);
scm_sizet free_yuv_overlay (SCM s_overlay);

/* constructors */
SCM img_load (SCM file);
SCM make_surface (SCM s_width, SCM s_height);
SCM create_rgb_surface (SCM s_flags, SCM s_width, SCM s_height,
                        SCM s_depth, SCM s_rmask, SCM s_gmask,
                        SCM s_bmask, SCM s_amask);
/* SCM create_rgb_surface_from (SCM s_pixels, SCM s_width, SCM s_height, */
/*                              SCM s_depth, SCM s_pitch, SCM s_rmask, */
/*                              SCM s_gmask, SCM s_bmask, SCM s_amask); */
SCM create_cursor (SCM s_data, SCM s_mask, SCM s_w, SCM s_h,
                   SCM s_hot_x, SCM s_hot_y);
SCM create_yuv_overlay (SCM s_width, SCM s_height,
                        SCM s_format, SCM s_display);
SCM make_rect (SCM s_x, SCM s_y, SCM s_w, SCM s_h);
SCM rect_x (SCM s_rect);
SCM rect_y (SCM s_rect);
SCM rect_w (SCM s_rect);
SCM rect_h (SCM s_rect);
SCM rect_set_x (SCM s_rect, SCM s_x);
SCM rect_set_y (SCM s_rect, SCM s_y);
SCM rect_set_w (SCM s_rect, SCM s_w);
SCM rect_set_h (SCM s_rect, SCM s_h);
SCM make_color (SCM s_r, SCM s_g, SCM s_b);
SCM color_r (SCM s_color);
SCM color_g (SCM s_color);
SCM color_b (SCM s_color);
SCM color_set_r (SCM s_color, SCM s_r);
SCM color_set_g (SCM s_color, SCM s_g);
SCM color_set_b (SCM s_color, SCM s_b);
SCM make_palette (SCM s_colors);
SCM make_pixel_format (void);

/* function prototypes */
SCM get_video_surface (void);
SCM get_video_info (void);
SCM video_driver_name (void);
SCM list_modes (SCM s_pixel_format, SCM s_flags);
SCM video_mode_ok (SCM s_width, SCM s_height, SCM s_bpp, SCM s_flags);
SCM update_rect (SCM s_screen, SCM s_x, SCM s_y, SCM s_w, SCM s_h);
SCM flip (SCM s_screen);
SCM set_colors (SCM s_surface, SCM s_colors);
SCM set_palette (SCM s_surface, SCM s_flags, SCM s_colors);
SCM set_gamma (SCM s_redgamma, SCM s_greengamma, SCM s_bluegamma);
SCM get_gamma_ramp (void);
SCM set_gamma_ramp (SCM s_redtable, SCM s_greentable, SCM s_bluetable);
SCM map_rgb (SCM s_pixel_fmt, SCM s_r, SCM s_g, SCM s_b);
SCM map_rgba (SCM s_pixel_fmt, SCM s_r, SCM s_g, SCM s_b, SCM s_a);
SCM get_rgb (SCM s_pixel, SCM s_pixel_fmt);
SCM get_rgba (SCM s_pixel, SCM s_pixel_fmt);
SCM lock_surface (SCM s_surface);
SCM unlock_surface (SCM s_surface);
SCM load_bmp (SCM s_file);
SCM save_bmp (SCM s_surface, SCM s_file);
SCM set_color_key (SCM s_surface, SCM s_flag, SCM s_key);
SCM set_alpha (SCM s_surface, SCM s_flag, SCM s_alpha);
SCM set_clip_rect (SCM s_surface, SCM s_rect);
SCM get_clip_rect (SCM s_surface);
SCM convert_surface (SCM s_src, SCM s_pixel_fmt, SCM s_flags);
SCM blit_surface (SCM s_src, SCM s_srcrect, SCM s_dst, SCM s_dstrect);
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
