/*******************************************************************
 *  video.c -- SDL Video functions for Guile                       *
 *                                                                 *
 *  Created:    <2001-04-24 23:40:20 foof>                         *
 *  Time-stamp: <2001-05-29 21:19:25 foof>                         *
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

/* guile headers */
#include <libguile.h>
/* sdl headers */
#include <SDL/SDL.h>
#include <SDL/SDL_image.h>

#define MAX_DRIVER_LEN 100

/* tags for SDL smobs */
static long surface_tag;
static long cursor_tag;
static long rect_tag;
static long color_tag;
static long palette_tag;
static long pixel_format_tag;
static long overlay_tag;
static long video_info_tag;

/* constants */

/* constructors */

SCM
create_rgb_surface (SCM s_flags, SCM s_width, SCM s_height,
                    SCM s_depth, SCM s_rmask, SCM s_gmask,
                    SCM s_bmask, SCM s_amask)
{
   return SCM_UNSPECIFIED;
}

SCM
create_rgb_surface_from (SCM s_pixels, SCM s_width, SCM s_height,
                         SCM s_depth, SCM s_pitch, SCM s_rmask,
                         SCM s_gmask, SCM s_bmask, SCM s_amask)
{
   return SCM_UNSPECIFIED;
}

SCM  
img_load (SCM file)  
{  
   SDL_Surface *image;  

   SCM_ASSERT ((SCM_NIMP (file) && SCM_STRINGP (file)),  
               file, SCM_ARG1, "sdl-load-image");  

   image = IMG_Load (SCM_CHARS (file));  
   SCM_RETURN_NEWSMOB (surface_tag, image);  
}  

SCM
create_cursor (SCM s_data, SCM s_mask, SCM s_w, SCM s_h,
               SCM s_hot_x, SCM s_hot_y)
{
   return SCM_UNSPECIFIED;
}

SCM
create_yuv_overlay (SCM s_width, SCM s_height, SCM s_format, SCM s_display)
{
   return SCM_UNSPECIFIED;
}

SCM
make_rect (SCM s_x, SCM s_y, SCM s_w, SCM s_h)
{
   SDL_Rect *rect;

   SCM_ASSERT (SCM_INUMP (s_x),  s_x,  SCM_ARG1, "make-rect");
   SCM_ASSERT (SCM_INUMP (s_y),  s_y,  SCM_ARG2, "make-rect");
   SCM_ASSERT (SCM_INUMP (s_w),  s_w,  SCM_ARG3, "make-rect");
   SCM_ASSERT (SCM_INUMP (s_h),  s_h,  SCM_ARG4, "make-rect");

   rect = (SDL_Rect *) scm_must_malloc (sizeof (SDL_Rect), "rect");
   rect->x = SCM_INUM (s_x);
   rect->y = SCM_INUM (s_y);
   rect->w = SCM_INUM (s_w);
   rect->h = SCM_INUM (s_h);

   SCM_RETURN_NEWSMOB (rect_tag, rect);
}

SCM
make_color (SCM s_r, SCM s_g, SCM s_b)
{
   SDL_Color *color;

   SCM_ASSERT (SCM_INUMP (s_r),  s_r,  SCM_ARG1, "make-rect");
   SCM_ASSERT (SCM_INUMP (s_g),  s_g,  SCM_ARG2, "make-rect");
   SCM_ASSERT (SCM_INUMP (s_b),  s_b,  SCM_ARG3, "make-rect");

   color = (SDL_Color *) scm_must_malloc (sizeof (SDL_Color), "color");
   color->r = SCM_INUM (s_r);
   color->g = SCM_INUM (s_g);
   color->b = SCM_INUM (s_b);

   SCM_RETURN_NEWSMOB (color_tag, color);
}

SCM
make_palette (SCM s_colors)
{
   return SCM_UNSPECIFIED;
}

SCM
make_pixel_format (void)
{
   return SCM_UNSPECIFIED;
}

/* function prototypes */
SCM
get_video_surface (void)
{
   SDL_Surface *surface = SDL_GetVideoSurface();
   SCM_RETURN_NEWSMOB (surface_tag, surface);
}

SCM
get_video_info (void)
{
   SDL_VideoInfo *info = SDL_GetVideoInfo();
   SCM_RETURN_NEWSMOB (video_info_tag, info);
}

SCM
video_driver_name (void)
{
   char name[MAX_DRIVER_LEN];
   SDL_VideoDriverName (name, MAX_DRIVER_LEN);
   return scm_makfrom0str (name);
}


SCM
list_modes (SCM s_pixel_format, SCM s_flags)
{
   SDL_PixelFormat *format;
   Uint32 flags;
   SDL_Rect **modes;
   SCM result = SCM_EOL;
   SCM current = result;
   SCM rect_smob;
   int i;

   SCM_ASSERT ((SCM_NIMP (s_pixel_format)
                && SCM_CAR (s_pixel_format) == pixel_format_tag),
               s_pixel_format, SCM_ARG1, "list-modes");
   SCM_ASSERT (SCM_INUMP (s_flags), s_flags, SCM_ARG2, "list-modes");

   format = (SDL_PixelFormat *) SCM_CDR (s_pixel_format);
   flags  = (Uint32) SCM_INUM (s_flags);

   modes = SDL_ListModes (format, flags);

   if (modes == (SDL_Rect**)0) {
      /* return #f to signify no resolutions are available */
      result = SCM_BOOL_F;
   }
   else if (modes == (SDL_Rect**)-1) {
      /* return #t to signify all resolutions are available */
      result = SCM_BOOL_T;
   } else {
      /* otherwise return a list of the available resolutions */
      for (i=0; modes[i]; i++) {
         /* create the rect smob */
         SCM_NEWCELL (rect_smob);
         SCM_SETCDR (rect_smob, modes[i]);
         SCM_SETCAR (rect_smob, rect_tag);
         /* cons it onto the list */
         result = scm_cons (rect_smob, result);
      }
   }
}

SCM
video_mode_ok (SCM s_width, SCM s_height, SCM s_bpp, SCM s_flags)
{
   int width, height, bpp;
   Uint32 flags;
   int result;

   SCM_ASSERT (SCM_INUMP (s_width),  s_width,  SCM_ARG1, "video-mode-ok");
   SCM_ASSERT (SCM_INUMP (s_height), s_height, SCM_ARG2, "video-mode-ok");
   SCM_ASSERT (SCM_INUMP (s_bpp),    s_bpp,    SCM_ARG3, "video-mode-ok");
   SCM_ASSERT (SCM_INUMP (s_flags),  s_flags,  SCM_ARG4, "video-mode-ok");

   width  = SCM_INUM (s_width);
   height = SCM_INUM (s_height);
   bpp    = SCM_INUM (s_bpp);
   flags  = (Uint32) SCM_INUM (s_flags);

   result = SDL_VideoModeOK (width, height, bpp, flags);
   if (result) {
      return SCM_MAKINUM (result);
   } else {
      return SCM_BOOL_F;
   }
}

SCM
set_video_mode (SCM s_width, SCM s_height, SCM s_bpp, SCM s_flags)
{
   SDL_Surface *surface;
   int width, height, bpp;
   Uint32 flags;

   SCM_ASSERT (SCM_INUMP (s_width),  s_width,  SCM_ARG1, "set-video-mode");
   SCM_ASSERT (SCM_INUMP (s_height), s_height, SCM_ARG2, "set-video-mode");
   SCM_ASSERT (SCM_INUMP (s_bpp),    s_bpp,    SCM_ARG3, "set-video-mode");
   SCM_ASSERT (SCM_INUMP (s_flags),  s_flags,  SCM_ARG4, "set-video-mode");

   width  = SCM_INUM (s_width);
   height = SCM_INUM (s_height);
   bpp    = SCM_INUM (s_bpp);
   flags  = (Uint32) SCM_INUM (s_flags);

   surface = SDL_SetVideoMode (width, height, bpp, flags);
   SCM_RETURN_NEWSMOB (surface_tag, surface);
}

SCM
update_rect (SCM s_screen, SCM s_x, SCM s_y, SCM s_w, SCM s_h)
{
   SDL_Surface *screen;
   Sint32 x, y, w, h;

   SCM_ASSERT ((SCM_NIMP (s_screen)
                && SCM_CAR (s_screen) == surface_tag),
               s_screen, SCM_ARG1, "update-rect");
   SCM_ASSERT (SCM_INUMP (s_x), s_x, SCM_ARG2, "update-rect");
   SCM_ASSERT (SCM_INUMP (s_y), s_y, SCM_ARG3, "update-rect");
   SCM_ASSERT (SCM_INUMP (s_w), s_w, SCM_ARG4, "update-rect");
   SCM_ASSERT (SCM_INUMP (s_h), s_h, SCM_ARG5, "update-rect");

   screen = (SDL_Surface *) SCM_CDR (s_screen);
   x = (Sint32) SCM_INUM (s_x);
   y = (Sint32) SCM_INUM (s_y);
   w = (Sint32) SCM_INUM (s_w);
   h = (Sint32) SCM_INUM (s_h);

   SDL_UpdateRect (screen, x, y, w, h);

   return SCM_UNSPECIFIED;
}

SCM
update_rects (SCM s_screen, SCM s_rects)
{
}

SCM
flip (SCM s_screen)
{
   SCM_ASSERT ((SCM_NIMP (s_screen)
                && SCM_CAR (s_screen) == surface_tag),
               s_screen, SCM_ARG1, "flip");

   SDL_Flip ((SDL_Surface *) SCM_CDR (s_screen));

   return SCM_UNSPECIFIED;
}

SCM
set_colors (SCM s_surface, SCM s_colors)
{
   return SCM_UNSPECIFIED;
}

SCM
set_palette (SCM s_surface, SCM s_flags, SCM s_colors)
{
   return SCM_UNSPECIFIED;
}

SCM
set_gamma (SCM s_redgamma, SCM s_greengamma, SCM s_bluegamma)
{
   return SCM_UNSPECIFIED;
}

SCM
get_gamma_ramp (void)
{
   return SCM_UNSPECIFIED;
}

SCM
set_gamma_ramp (SCM s_redtable, SCM s_greentable, SCM s_bluetable)
{
   return SCM_UNSPECIFIED;
}

SCM
map_rgb (SCM s_pixel_fmt, SCM s_r, SCM s_g, SCM s_b)
{
   return SCM_UNSPECIFIED;
}

SCM
map_rgba (SCM s_pixel_fmt, SCM s_r, SCM s_g, SCM s_b, SCM s_a)
{
   return SCM_UNSPECIFIED;
}

SCM
get_rgb (SCM s_pixel, SCM s_pixel_fmt)
{
   return SCM_UNSPECIFIED;
}

SCM
get_rgba (SCM s_pixel, SCM s_pixel_fmt)
{
   return SCM_UNSPECIFIED;
}

SCM
free_surface (SCM s_surface)
{
   return SCM_UNSPECIFIED;
}

SCM
lock_surface (SCM s_surface)
{
   return SCM_UNSPECIFIED;
}

SCM
unlock_surface (SCM s_surface)
{
   return SCM_UNSPECIFIED;
}

SCM
load_bmp (SCM s_file)
{
   return SCM_UNSPECIFIED;
}

SCM
save_bmp (SCM s_surface, SCM s_file)
{
   return SCM_UNSPECIFIED;
}

SCM
set_color_key (SCM s_surface, SCM s_flag, SCM s_key)
{
   return SCM_UNSPECIFIED;
}

SCM
set_alpha (SCM s_surface, SCM s_flag, SCM s_alpha)
{
   return SCM_UNSPECIFIED;
}

SCM
set_clip_rect (SCM s_surface, SCM s_rect)
{
   return SCM_UNSPECIFIED;
}

SCM
get_clip_rect (SCM s_surface)
{
   return SCM_UNSPECIFIED;
}

SCM
convert_surface (SCM s_src, SCM s_pixel_fmt, SCM s_flags)
{
   return SCM_UNSPECIFIED;
}

SCM
blit_surface (SCM s_src, SCM s_srcrect, SCM s_dst, SCM s_dstrect)
{
   SDL_Surface *src;
   SDL_Surface *dst;
   SDL_Rect *srcrect;
   SDL_Rect *dstrect;

   SCM_ASSERT ((SCM_NIMP (s_src)
                && SCM_CAR (s_src) == surface_tag),
               s_src, SCM_ARG1, "blit-surface");
   SCM_ASSERT ((SCM_NIMP (s_srcrect)
                && SCM_CAR (s_srcrect) == rect_tag),
               s_srcrect, SCM_ARG2, "blit-surface");
   SCM_ASSERT ((SCM_NIMP (s_dst)
                && SCM_CAR (s_dst) == surface_tag),
               s_dst, SCM_ARG3, "blit-surface");
   SCM_ASSERT ((SCM_NIMP (s_dstrect)
                && SCM_CAR (s_dstrect) == rect_tag),
               s_dstrect, SCM_ARG4, "blit-surface");

   src = (SDL_Surface *) SCM_CDR (s_src);
   srcrect = (SDL_Rect *) SCM_CDR (s_srcrect);
   dst = (SDL_Surface *) SCM_CDR (s_dst);
   dstrect = (SDL_Rect *) SCM_CDR (s_dstrect);

   return SCM_MAKINUM (SDL_BlitSurface (src, srcrect, dst, dstrect));
}

SCM
fill_rect (SCM s_dst, SCM s_dstrect, SCM s_color)
{
   SDL_Surface *dst;
   SDL_Rect *dstrect;
   Uint32 color;

   SCM_ASSERT ((SCM_NIMP (s_dst)
                && SCM_CAR (s_dst) == surface_tag),
               s_dst, SCM_ARG1, "fill-rect");
   SCM_ASSERT ((SCM_NIMP (s_dstrect)
                && SCM_CAR (s_dstrect) == rect_tag),
               s_dstrect, SCM_ARG2, "fill-rect");
   SCM_ASSERT (SCM_INUMP (s_color), s_color, SCM_ARG3, "fill-rect");

   dst = (SDL_Surface *) SCM_CDR (s_dst);
   dstrect = (SDL_Rect *) SCM_CDR (s_dstrect);
   color = (Uint32) SCM_INUM (s_color);

   return SCM_MAKINUM (SDL_FillRect (dst, dstrect, color));
}

SCM
display_format (SCM s_surface)
{
   return SCM_UNSPECIFIED;
}

SCM
display_format_alpha (SCM s_surface)
{
   return SCM_UNSPECIFIED;
}

SCM
warp_mouse (SCM s_x, SCM s_y)
{
   return SCM_UNSPECIFIED;
}

SCM
free_cursor (SCM s_cursor)
{
   return SCM_UNSPECIFIED;
}

SCM
set_cursor (SCM s_cursor)
{
   return SCM_UNSPECIFIED;
}

SCM
get_cursor (void)
{
   return SCM_UNSPECIFIED;
}

SCM
show_cursor (SCM s_toggle)
{
   return SCM_UNSPECIFIED;
}

SCM
gl_load_library (SCM s_path)
{
   return SCM_UNSPECIFIED;
}

SCM
gl_get_proc_address (SCM s_proc)
{
   return SCM_UNSPECIFIED;
}

SCM
gl_get_attribute (SCM s_attr)
{
   return SCM_UNSPECIFIED;
}

SCM
gl_set_attribute (SCM s_attr, SCM s_value)
{
   return SCM_UNSPECIFIED;
}

SCM
gl_swap_buffers (void)
{
   return SCM_UNSPECIFIED;
}

SCM
lock_yuv_overlay (SCM s_overlay)
{
   return SCM_UNSPECIFIED;
}

SCM
unlock_yuv_overlay (SCM s_overlay)
{
   return SCM_UNSPECIFIED;
}

SCM
display_yuv_overlay (SCM s_overlay, SCM s_dstrect)
{
   return SCM_UNSPECIFIED;
}

SCM
free_yuv_overlay (SCM s_overlay)
{
   return SCM_UNSPECIFIED;
}

void
sdl_video_init (void)
{
   /* smobs */
   surface_tag   = scm_make_smob_type ("surface", sizeof (SDL_Surface));
   rect_tag      = scm_make_smob_type ("rect", sizeof (SDL_Rect));
   color_tag     = scm_make_smob_type ("color", sizeof (SDL_Color));
   cursor_tag    = scm_make_smob_type ("cursor", sizeof (SDL_Cursor));
   palette_tag   = scm_make_smob_type ("palette", sizeof (SDL_Palette));
   pixel_format_tag = scm_make_smob_type ("pixel-format", sizeof (SDL_PixelFormat));
   overlay_tag   = scm_make_smob_type ("overlay", sizeof (SDL_Overlay));
   video_info_tag = scm_make_smob_type ("video-info", sizeof (SDL_VideoInfo));

   /* video functions */
   scm_make_gsubr ("make-rect",          4, 0, 0, make_rect);
   scm_make_gsubr ("make-color",         3, 0, 0, make_color);
   scm_make_gsubr ("get-video-surface",  0, 0, 0, get_video_surface);
   scm_make_gsubr ("video-mode-ok",      4, 0, 0, video_mode_ok);
   scm_make_gsubr ("set-video-mode",     4, 0, 0, set_video_mode);
   scm_make_gsubr ("update-rect",        5, 0, 0, update_rect);
   scm_make_gsubr ("flip",               1, 0, 0, flip);
   scm_make_gsubr ("blit-surface",       4, 0, 0, blit_surface);
   scm_make_gsubr ("fill-rect",          3, 0, 0, fill_rect);
   scm_make_gsubr ("list-modes",         2, 0, 0, list_modes);
   scm_make_gsubr ("video-driver-name",  0, 0, 0, video_driver_name);
   scm_make_gsubr ("get-video-info",     0, 0, 0, get_video_info);

   /* image functions */
   scm_make_gsubr ("load-image",         1, 0, 0, img_load);

   /* exported symbols */
   scm_c_export ("make-rect", "make-color", "get-video-surface",
                 "video-mode-ok", "set-video-mode", "update-rect",
                 "flip", "blit-surface", "fill-rect", "list-modes",
                 "video-driver-name", "get-video-info",
                 /* image functions */
                 "load-image",
                 NULL);
}

