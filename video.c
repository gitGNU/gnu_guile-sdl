/*******************************************************************
 *  video.c -- SDL Video functions for Guile                       *
 *                                                                 *
 *  Created:    <2001-04-24 23:40:20 foof>                         *
 *  Time-stamp: <2001-06-13 00:17:24 foof>                         *
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
/* scm util headers */
#include "scm.h"

#define MAX_DRIVER_LEN 100

/* tags for SDL smobs */
long surface_tag;
long cursor_tag;
long rect_tag;
long color_tag;
long palette_tag;
long pixel_format_tag;
long overlay_tag;
long video_info_tag;


/* surfaces */

scm_sizet
free_surface (SCM surface)
{
   /* printf ("free_surface(%p)\n", (SDL_Surface*) SCM_SMOB_DATA (surface)); */
   /* it's safe to call this on the primary display */
   SDL_FreeSurface ((SDL_Surface*) SCM_SMOB_DATA (surface));
   return sizeof (SDL_Surface);
}

SCM
make_surface (SCM s_width, SCM s_height, SCM s_flags)
{
   /* surface to make */
   SDL_Surface *surface;
   /* params */
   int width, height;
   Uint32 flags;
   /* video info and pixel format */
   const SDL_VideoInfo *info = SDL_GetVideoInfo();
   const SDL_PixelFormat *fmt = info->vfmt;
   /* current screen info */
   Uint8 depth;
   Uint32 rmask, gmask, bmask, amask;

   /* validate params */
   SCM_ASSERT (SCM_INUMP (s_width),  s_width,  SCM_ARG1, "make-surface");
   SCM_ASSERT (SCM_INUMP (s_height), s_height, SCM_ARG2, "make-surface");

   width  = SCM_INUM (s_width);
   height = SCM_INUM (s_height);

   /* flags are optional, defaulting to SDL_HWSURFACE */
   if (s_flags == SCM_UNDEFINED) {
      flags = SDL_HWSURFACE;
   } else {
      SCM_ASSERT (SCM_INUMP (s_flags),  s_flags,  SCM_ARG3, "make-surface");
      flags = (Uint32) SCM_INUM (s_flags);
   }

   /* get defaults from the current video info */
   depth = (Uint8)  fmt->BitsPerPixel;
   rmask = (Uint32) fmt->Rmask;
   gmask = (Uint32) fmt->Gmask;
   bmask = (Uint32) fmt->Bmask;
   amask = (Uint32) fmt->Amask;

   /* create the surface */
   surface = SDL_CreateRGBSurface (SDL_HWSURFACE, width, height, depth,
                                   rmask, gmask, bmask, amask);

   /* return a newly allocated smob */
   SCM_RETURN_NEWSMOB (surface_tag, surface);
}

SCM
create_rgb_surface (SCM s_flags, SCM s_width, SCM s_height,
                    SCM s_depth, SCM s_rmask, SCM s_gmask,
                    SCM s_bmask, SCM s_amask)
{
   /* surface to make */
   SDL_Surface *surface;
   /* params */
   int width, height;
   Uint32 flags;
   Uint8 depth;
   Uint32 rmask, gmask, bmask, amask;

   /* validate params */
   SCM_ASSERT (SCM_INUMP (s_flags),  s_flags,  SCM_ARG1, "create-rgb-surface");
   SCM_ASSERT (SCM_INUMP (s_width),  s_width,  SCM_ARG2, "create-rgb-surface");
   SCM_ASSERT (SCM_INUMP (s_height), s_height, SCM_ARG3, "create-rgb-surface");
   SCM_ASSERT (SCM_INUMP (s_depth),  s_depth,  SCM_ARG4, "create-rgb-surface");
   SCM_ASSERT (SCM_INUMP (s_rmask),  s_rmask,  SCM_ARG5, "create-rgb-surface");
   SCM_ASSERT (SCM_INUMP (s_gmask),  s_gmask,  SCM_ARG6, "create-rgb-surface");
   SCM_ASSERT (SCM_INUMP (s_bmask),  s_bmask,  SCM_ARG7, "create-rgb-surface");
   SCM_ASSERT (SCM_INUMP (s_amask),  s_amask,  SCM_ARGn, "create-rgb-surface");

   flags  = (Uint32) SCM_INUM (s_flags);
   width  = SCM_INUM (s_width);
   height = SCM_INUM (s_height);
   depth  = (Uint8)  SCM_INUM (s_depth);
   rmask  = (Uint32) SCM_INUM (s_rmask);
   gmask  = (Uint32) SCM_INUM (s_gmask);
   bmask  = (Uint32) SCM_INUM (s_bmask);
   amask  = (Uint32) SCM_INUM (s_amask);

   /* create the surface */
   surface = SDL_CreateRGBSurface (SDL_HWSURFACE, width, height, depth,
                                   rmask, gmask, bmask, amask);

   /* return a newly allocated smob */
   SCM_RETURN_NEWSMOB (surface_tag, surface);
}

/* SCM */
/* create_rgb_surface_from (SCM s_pixels, SCM s_width, SCM s_height, */
/*                          SCM s_depth, SCM s_pitch, SCM s_rmask, */
/*                          SCM s_gmask, SCM s_bmask, SCM s_amask) */
/* { */
/*    return SCM_UNSPECIFIED; */
/* } */

SCM
create_cursor (SCM s_data, SCM s_mask, SCM s_w, SCM s_h,
               SCM s_hot_x, SCM s_hot_y)
{
   SDL_Cursor *cursor;
   Uint8 *data, *mask;
   int data_len, mask_len;
   int i, w, h, hot_x, hot_y;

   /* validate args */
   SCM_ASSERT (scm_vector_p (s_data), s_data, SCM_ARG1, "create-cursor");
   SCM_ASSERT (scm_vector_p (s_mask), s_mask, SCM_ARG2, "create-cursor");
   SCM_ASSERT (SCM_INUMP (s_w), s_w, SCM_ARG3, "create-cursor");
   SCM_ASSERT (SCM_INUMP (s_h), s_h, SCM_ARG4, "create-cursor");
   SCM_ASSERT (SCM_INUMP (s_hot_x), s_hot_x, SCM_ARG5, "create-cursor");
   SCM_ASSERT (SCM_INUMP (s_hot_y), s_hot_y, SCM_ARG5, "create-cursor");

   /* build the arrays */
   data_len = SCM_INUM (scm_vector_length (s_data));
   data = scm_must_malloc (data_len, "create-cursor data array");
   for (i=0; i<data_len; i++) {
      data[i] = (Uint8) SCM_INUM (scm_vector_ref (s_data, SCM_MAKINUM (i)));
   }
   mask_len = SCM_INUM (scm_vector_length (s_mask));
   mask = scm_must_malloc (mask_len, "create-cursor mask array");
   for (i=0; i<mask_len; i++) {
      mask[i] = (Uint8) SCM_INUM (scm_vector_ref (s_mask, SCM_MAKINUM (i)));
   }

   /* numbers */
   w = SCM_INUM (s_w);
   h = SCM_INUM (s_h);
   hot_x = SCM_INUM (s_hot_x);
   hot_y = SCM_INUM (s_hot_y);

   /* create the cursor */
   cursor = SDL_CreateCursor (data, mask, w, h, hot_x, hot_y);

   /* free the arrays */
   scm_must_free (data);
   scm_must_free (mask);

   /* return the new smob */
   SCM_RETURN_NEWSMOB (cursor_tag, cursor);
}

SCM
create_yuv_overlay (SCM s_width, SCM s_height, SCM s_format, SCM s_display)
{
   int width, height;
   Uint32 format;
   SDL_Surface *display;
   SDL_Overlay *overlay;

   SCM_ASSERT (SCM_INUMP (s_width), s_width, SCM_ARG1, "create-yuv-overlay");
   SCM_ASSERT (SCM_INUMP (s_height), s_height, SCM_ARG2, "create-yuv-overlay");
   SCM_ASSERT (SCM_INUMP (s_format), s_format, SCM_ARG3, "create-yuv-overlay");

   width = SCM_INUM (s_width);
   height = SCM_INUM (s_height);
   format = (Uint32) SCM_INUM (s_format);

   if (s_display == SCM_UNDEFINED) {
      display = SDL_GetVideoSurface ();
   } else {
      SCM_ASSERT_SMOB (s_display, surface_tag, SCM_ARG4, "create-yuv-overlay");
      display = (SDL_Surface*) SCM_SMOB_DATA (s_display);
   }

   overlay = SDL_CreateYUVOverlay (width, height, format, display);

   SCM_RETURN_NEWSMOB (overlay_tag, overlay);
}


/* rectangles */

scm_sizet
free_rect (SCM rect)
{
   /* printf ("free_rect(%p)\n", rect); */
   free ((SDL_Rect*) SCM_SMOB_DATA (rect));
   return sizeof (SDL_Rect);
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

/* rect getters */
SCM_DEFINE_INUM_GETTER ("rect:x", rect_x, rect_tag, SDL_Rect*, x)
SCM_DEFINE_INUM_GETTER ("rect:y", rect_y, rect_tag, SDL_Rect*, y)
SCM_DEFINE_INUM_GETTER ("rect:w", rect_w, rect_tag, SDL_Rect*, w)
SCM_DEFINE_INUM_GETTER ("rect:h", rect_h, rect_tag, SDL_Rect*, h)

/* rect setters */
SCM_DEFINE_INUM_SETTER ("rect:set-x!", rect_set_x, rect_tag, SDL_Rect*, x)
SCM_DEFINE_INUM_SETTER ("rect:set-y!", rect_set_y, rect_tag, SDL_Rect*, y)
SCM_DEFINE_INUM_SETTER ("rect:set-w!", rect_set_w, rect_tag, SDL_Rect*, w)
SCM_DEFINE_INUM_SETTER ("rect:set-h!", rect_set_h, rect_tag, SDL_Rect*, h)


/* colors */
scm_sizet
free_color (SCM color)
{
   /* printf ("free_color(%p)\n", color); */
   free ((SDL_Color*) SCM_SMOB_DATA (color));
   return sizeof (SDL_Color);
}

SCM
make_color (SCM s_r, SCM s_g, SCM s_b)
{
   SDL_Color *color;

   SCM_ASSERT (SCM_INUMP (s_r),  s_r,  SCM_ARG1, "make-color");
   SCM_ASSERT (SCM_INUMP (s_g),  s_g,  SCM_ARG2, "make-color");
   SCM_ASSERT (SCM_INUMP (s_b),  s_b,  SCM_ARG3, "make-color");

   color = (SDL_Color *) scm_must_malloc (sizeof (SDL_Color), "color");
   color->r = SCM_INUM (s_r);
   color->g = SCM_INUM (s_g);
   color->b = SCM_INUM (s_b);

   SCM_RETURN_NEWSMOB (color_tag, color);
}

/* color getters */
SCM_DEFINE_INUM_GETTER ("color:r", color_r, color_tag, SDL_Color*, r)
SCM_DEFINE_INUM_GETTER ("color:g", color_g, color_tag, SDL_Color*, g)
SCM_DEFINE_INUM_GETTER ("color:b", color_b, color_tag, SDL_Color*, b)

/* color setters */
SCM_DEFINE_INUM_SETTER ("color:set-r!", color_set_r, color_tag, SDL_Color*, r)
SCM_DEFINE_INUM_SETTER ("color:set-g!", color_set_g, color_tag, SDL_Color*, g)
SCM_DEFINE_INUM_SETTER ("color:set-b!", color_set_b, color_tag, SDL_Color*, b)


/* palettes */
SCM
make_palette (SCM s_colors)
{
   return SCM_UNSPECIFIED;
}


/* pixel formats */
SCM
make_pixel_format (void)
{
   return SCM_UNSPECIFIED;
}

/* function prototypes */
SCM
get_video_surface (void)
{
   SCM_RETURN_NEWSMOB (surface_tag, SDL_GetVideoSurface());
}

SCM
get_video_info (void)
{
   const SDL_VideoInfo *info = SDL_GetVideoInfo();
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
   SDL_PixelFormat *format=NULL;
   Uint32 flags=0;
   SDL_Rect **modes;
   SCM result = SCM_EOL;
   SCM rect_smob;
   int i;

   /* if a pixel format is given, verify and unpack it */
   if (s_pixel_format != SCM_UNDEFINED) {
      SCM_ASSERT_SMOB (s_pixel_format, pixel_format_tag, SCM_ARG1, "list-modes");
      format = (SDL_PixelFormat *) SCM_SMOB_DATA (s_pixel_format);
   }

   /* if flags are given, verify and unpack them */
   if (s_flags != SCM_UNDEFINED) {
      SCM_ASSERT (SCM_INUMP (s_flags), s_flags, SCM_ARG2, "list-modes");
      flags  = (Uint32) SCM_INUM (s_flags);
   }

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

   return result;
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

   SCM_RETURN_NEWSMOB (surface_tag, SDL_SetVideoMode (width, height, bpp, flags));
}

SCM
update_rect (SCM s_screen, SCM s_x, SCM s_y, SCM s_w, SCM s_h)
{
   SDL_Surface *screen;
   Sint32 x, y, w, h;

   SCM_ASSERT_SMOB (s_screen, surface_tag, SCM_ARG1, "update-rect");
   SCM_ASSERT (SCM_INUMP (s_x), s_x, SCM_ARG2, "update-rect");
   SCM_ASSERT (SCM_INUMP (s_y), s_y, SCM_ARG3, "update-rect");
   SCM_ASSERT (SCM_INUMP (s_w), s_w, SCM_ARG4, "update-rect");
   SCM_ASSERT (SCM_INUMP (s_h), s_h, SCM_ARG5, "update-rect");

   screen = (SDL_Surface *) SCM_SMOB_DATA (s_screen);
   x = (Sint32) SCM_INUM (s_x);
   y = (Sint32) SCM_INUM (s_y);
   w = (Sint32) SCM_INUM (s_w);
   h = (Sint32) SCM_INUM (s_h);

   SDL_UpdateRect (screen, x, y, w, h);

   return SCM_UNSPECIFIED;
}

SCM
flip (SCM s_screen)
{
   SDL_Surface *screen;

   if (s_screen != SCM_UNDEFINED) {
      /* verify and unpack a surface */
      SCM_ASSERT_SMOB (s_screen, surface_tag, SCM_ARG1, "flip");
      screen = (SDL_Surface *) SCM_SMOB_DATA (s_screen);
   } else {
      /* otherwise default to the current display */
      screen = SDL_GetVideoSurface();
   }

   SDL_Flip (screen);
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
   float redgamma, greengamma, bluegamma;

   SCM_ASSERT (scm_number_p (s_redgamma),   s_redgamma,   SCM_ARG1, "set-gamma");
   SCM_ASSERT (scm_number_p (s_greengamma), s_greengamma, SCM_ARG2, "set-gamma");
   SCM_ASSERT (scm_number_p (s_bluegamma),  s_bluegamma,  SCM_ARG3, "set-gamma");

   redgamma   = (float) SCM_REAL_VALUE (s_redgamma);
   greengamma = (float) SCM_REAL_VALUE (s_greengamma);
   bluegamma  = (float) SCM_REAL_VALUE (s_bluegamma);

   SCM_RETURN_TRUE_IF_0 (SDL_SetGamma (redgamma, greengamma, bluegamma));
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
   SDL_PixelFormat *fmt;
   Uint8 r, g, b;

   SCM_ASSERT_SMOB (s_pixel_fmt, pixel_format_tag, SCM_ARG1, "map-rbg");
   SCM_ASSERT (SCM_INUMP (s_r), s_r, SCM_ARG2, "map-rgb");
   SCM_ASSERT (SCM_INUMP (s_g), s_g, SCM_ARG3, "map-rgb");
   SCM_ASSERT (SCM_INUMP (s_b), s_b, SCM_ARG4, "map-rgb");

   fmt = (SDL_PixelFormat*) SCM_SMOB_DATA (s_pixel_fmt);
   r = (Uint8) SCM_INUM (s_r);
   g = (Uint8) SCM_INUM (s_g);
   b = (Uint8) SCM_INUM (s_b);

   return SCM_MAKINUM (SDL_MapRGB (fmt, r, g, b));
}

SCM
map_rgba (SCM s_pixel_fmt, SCM s_r, SCM s_g, SCM s_b, SCM s_a)
{
   SDL_PixelFormat *fmt;
   Uint8 r, g, b, a;

   SCM_ASSERT_SMOB (s_pixel_fmt, pixel_format_tag, SCM_ARG1, "map-rbga");
   SCM_ASSERT (SCM_INUMP (s_r), s_r, SCM_ARG2, "map-rgba");
   SCM_ASSERT (SCM_INUMP (s_g), s_g, SCM_ARG3, "map-rgba");
   SCM_ASSERT (SCM_INUMP (s_b), s_b, SCM_ARG4, "map-rgba");
   SCM_ASSERT (SCM_INUMP (s_a), s_a, SCM_ARG5, "map-rgba");

   fmt = (SDL_PixelFormat*) SCM_SMOB_DATA (s_pixel_fmt);
   r = (Uint8) SCM_INUM (s_r);
   g = (Uint8) SCM_INUM (s_g);
   b = (Uint8) SCM_INUM (s_b);
   a = (Uint8) SCM_INUM (s_a);

   return SCM_MAKINUM (SDL_MapRGBA (fmt, r, g, b, a));
}

SCM
get_rgb (SCM s_pixel, SCM s_pixel_fmt)
{
   SDL_PixelFormat *fmt;
   Uint32 pixel;
   Uint8 r, g, b;

   SCM_ASSERT (SCM_INUMP (s_pixel), s_pixel, SCM_ARG1, "get-rgb");
   SCM_ASSERT_SMOB (s_pixel_fmt, pixel_format_tag, SCM_ARG2, "get-rbg");

   fmt = (SDL_PixelFormat*) SCM_SMOB_DATA (s_pixel_fmt);
   pixel = (Uint32) SCM_INUM (s_pixel);

   SDL_GetRGB (pixel, fmt, &r, &g, &b);

   return SCM_LIST3 (SCM_MAKINUM (r), SCM_MAKINUM (g), SCM_MAKINUM (b));
}

SCM
get_rgba (SCM s_pixel, SCM s_pixel_fmt)
{
   SDL_PixelFormat *fmt;
   Uint32 pixel;
   Uint8 r, g, b, a;

   SCM_ASSERT (SCM_INUMP (s_pixel), s_pixel, SCM_ARG1, "get-rgba");
   SCM_ASSERT_SMOB (s_pixel_fmt, pixel_format_tag, SCM_ARG2, "get-rbga");

   fmt = (SDL_PixelFormat*) SCM_SMOB_DATA (s_pixel_fmt);
   pixel = (Uint32) SCM_INUM (s_pixel);

   SDL_GetRGBA (pixel, fmt, &r, &g, &b, &a);

   return SCM_LIST4 (SCM_MAKINUM (r), SCM_MAKINUM (g),
                     SCM_MAKINUM (b), SCM_MAKINUM (a));
}

/* SCM */
/* free_surface (SCM s_surface) */
/* { */
/*    return SCM_UNSPECIFIED; */
/* } */

SCM
lock_surface (SCM s_surface)
{
   SDL_Surface *surface;

   SCM_ASSERT_SMOB (s_surface, surface_tag, SCM_ARG1, "lock-surface");
   surface = (SDL_Surface*) SCM_SMOB_DATA (s_surface);

   SCM_RETURN_TRUE_IF_0 (SDL_LockSurface (surface));
}

SCM
unlock_surface (SCM s_surface)
{
   SDL_Surface *surface;

   SCM_ASSERT_SMOB (s_surface, surface_tag, SCM_ARG1, "unlock-surface");
   surface = (SDL_Surface*) SCM_SMOB_DATA (s_surface);

   SDL_UnlockSurface (surface);

   return SCM_UNSPECIFIED;
}

SCM
load_bmp (SCM s_file)
{
   SDL_Surface *surface;
   const char *file;

   SCM_ASSERT (SCM_STRINGP (s_file), s_file, SCM_ARG1, "load-bmp");
   file = SCM_STRING_CHARS (s_file);

   surface = SDL_LoadBMP (file);

   SCM_RETURN_NEWSMOB (surface_tag, surface);
}

SCM
save_bmp (SCM s_surface, SCM s_file)
{
   SDL_Surface *surface;
   const char *file;

   SCM_ASSERT_SMOB (s_surface, surface_tag,  SCM_ARG1, "save-bmp");
   SCM_ASSERT (SCM_STRINGP (s_file), s_file, SCM_ARG2, "save-bmp");

   surface = (SDL_Surface*) SCM_SMOB_DATA (s_surface);
   file = SCM_STRING_CHARS (s_file);

   SCM_RETURN_TRUE_IF_0 (SDL_SaveBMP (surface, file));
}

SCM
set_color_key (SCM s_surface, SCM s_flag, SCM s_key)
{
   SDL_Surface *surface;
   Uint32 flag, key;

   SCM_ASSERT_SMOB (s_surface, surface_tag, SCM_ARG1, "set-color-key!");
   SCM_ASSERT (SCM_INUMP (s_flag), s_flag,  SCM_ARG2, "set-color-key!");
   SCM_ASSERT (SCM_INUMP (s_key),  s_key,   SCM_ARG3, "set-color-key!");

   surface = (SDL_Surface*) SCM_SMOB_DATA (s_surface);
   flag = (Uint32) SCM_INUM (s_flag);
   key  = (Uint32) SCM_INUM (s_key);

   SCM_RETURN_TRUE_IF_0 (SDL_SetColorKey (surface, flag, key));
}

SCM
set_alpha (SCM s_surface, SCM s_flag, SCM s_alpha)
{
   SDL_Surface *surface;
   Uint32 flag;
   Uint8 alpha;

   SCM_ASSERT_SMOB (s_surface, surface_tag,  SCM_ARG1, "set-alpha!");
   SCM_ASSERT (SCM_INUMP (s_flag), s_flag,   SCM_ARG2, "set-alpha!");
   SCM_ASSERT (SCM_INUMP (s_alpha), s_alpha, SCM_ARG3, "set-alpha!");

   surface = (SDL_Surface*) SCM_SMOB_DATA (s_surface);
   flag  = (Uint32) SCM_INUM (s_flag);
   alpha = (Uint8)  SCM_INUM (s_alpha);

   SCM_RETURN_TRUE_IF_0 (SDL_SetAlpha (surface, flag, alpha));
}

SCM
set_clip_rect (SCM s_surface, SCM s_rect)
{
   SDL_Surface *surface;
   SDL_Rect *rect=NULL;

   SCM_ASSERT_SMOB (s_surface, surface_tag,  SCM_ARG1, "set-clip-rect!");

   surface = (SDL_Surface*) SCM_SMOB_DATA (s_surface);

   if (s_rect != SCM_UNDEFINED) {
      /* rect defaults to NULL (the whole surface) */
      SCM_ASSERT_SMOB (s_rect,    rect_tag,     SCM_ARG2, "set-clip-rect!");
      rect = (SDL_Rect*) SCM_SMOB_DATA (s_rect);
   }

   SDL_SetClipRect (surface, rect);

   return SCM_UNSPECIFIED;
}

SCM
get_clip_rect (SCM s_surface)
{
   SDL_Surface *surface;
   SDL_Rect *rect;

   SCM_ASSERT_SMOB (s_surface, surface_tag,  SCM_ARG1, "get-clip-rect");

   surface = (SDL_Surface*) SCM_SMOB_DATA (s_surface);

   SDL_GetClipRect (surface, rect);

   SCM_RETURN_NEWSMOB (rect_tag, rect);
}

SCM
convert_surface (SCM s_src, SCM s_fmt, SCM s_flags)
{
   SDL_Surface *src, *result;
   SDL_PixelFormat *fmt;
   Uint32 flags;

   SCM_ASSERT_SMOB (s_src, surface_tag,  SCM_ARG1, "convert-surface");
   SCM_ASSERT_SMOB (s_fmt, pixel_format_tag, SCM_ARG2, "convert-surface");
   SCM_ASSERT (SCM_INUMP (s_flags), s_flags, SCM_ARG3, "convert-surface");

   src = (SDL_Surface*) SCM_SMOB_DATA (s_src);
   fmt = (SDL_PixelFormat*) SCM_SMOB_DATA (s_fmt);
   flags = (Uint32) SCM_INUM (s_flags);

   result = SDL_ConvertSurface (src, fmt, flags);

   SCM_RETURN_NEWSMOB (surface_tag, result);
}

SCM
blit_surface (SCM s_src, SCM s_srcrect, SCM s_dst, SCM s_dstrect)
{
   SDL_Surface *src;
   SDL_Surface *dst;
   SDL_Rect *srcrect;
   SDL_Rect *dstrect;

   SCM_ASSERT_SMOB (s_src, surface_tag,  SCM_ARG1, "blit-surface");
   SCM_ASSERT_SMOB (s_srcrect, rect_tag, SCM_ARG2, "blit-surface");
   SCM_ASSERT_SMOB (s_dst, surface_tag,  SCM_ARG3, "blit-surface");
   SCM_ASSERT_SMOB (s_dstrect, rect_tag, SCM_ARG4, "blit-surface");

   src = (SDL_Surface *)  SCM_SMOB_DATA (s_src);
   srcrect = (SDL_Rect *) SCM_SMOB_DATA (s_srcrect);
   dst = (SDL_Surface *)  SCM_SMOB_DATA (s_dst);
   dstrect = (SDL_Rect *) SCM_SMOB_DATA (s_dstrect);

   return SCM_MAKINUM (SDL_BlitSurface (src, srcrect, dst, dstrect));
}

SCM
fill_rect (SCM s_dst, SCM s_dstrect, SCM s_color)
{
   SDL_Surface *dst;
   SDL_Rect *dstrect;
   Uint32 color;

   SCM_ASSERT_SMOB (s_dst, surface_tag,  SCM_ARG1, "fill-rect");
   SCM_ASSERT_SMOB (s_dstrect, rect_tag, SCM_ARG2, "fill-rect");
   SCM_ASSERT (SCM_INUMP (s_color), s_color, SCM_ARG3, "fill-rect");

   dst = (SDL_Surface *) SCM_SMOB_DATA (s_dst);
   dstrect = (SDL_Rect *) SCM_SMOB_DATA (s_dstrect);
   color = (Uint32) SCM_INUM (s_color);

   return SCM_MAKINUM (SDL_FillRect (dst, dstrect, color));
}

SCM
display_format (SCM s_surface)
{
   SDL_Surface *surface;

   SCM_ASSERT_SMOB (s_surface, surface_tag,  SCM_ARG1, "display-format");

   surface = SDL_DisplayFormat ((SDL_Surface*) SCM_SMOB_DATA (s_surface));

   if (! surface) {
      return SCM_BOOL_F;
   }

   SCM_RETURN_NEWSMOB (surface_tag, surface);
}

SCM
display_format_alpha (SCM s_surface)
{
   SDL_Surface *surface;

   SCM_ASSERT_SMOB (s_surface, surface_tag,  SCM_ARG1, "display-format-alpha");

   surface = SDL_DisplayFormatAlpha ((SDL_Surface*) SCM_SMOB_DATA (s_surface));

   if (! surface) {
      return SCM_BOOL_F;
   }

   SCM_RETURN_NEWSMOB (surface_tag, surface);
}

SCM
warp_mouse (SCM s_x, SCM s_y)
{
   Uint16 x, y;

   SCM_ASSERT (SCM_INUMP (s_x), s_x, SCM_ARG1, "warp-mouse");
   SCM_ASSERT (SCM_INUMP (s_y), s_y, SCM_ARG2, "warp-mouse");

   x = (Uint16) SCM_INUM (s_x);
   y = (Uint16) SCM_INUM (s_y);

   SDL_WarpMouse (x, y);

   return SCM_UNSPECIFIED;
}

scm_sizet
free_cursor (SCM s_cursor)
{
   /* printf ("free_cursor(%p)\n", s_cursor); */
   SDL_FreeCursor ((SDL_Cursor*) SCM_SMOB_DATA (s_cursor));
   return sizeof (SDL_Cursor);
}

SCM
set_cursor (SCM s_cursor)
{
   SCM_ASSERT_SMOB (s_cursor, cursor_tag, SCM_ARG1, "set-cursor");
   SDL_SetCursor ((SDL_Cursor*) SCM_SMOB_DATA (s_cursor));
   return SCM_UNSPECIFIED;
}

SCM
get_cursor (void)
{
   SCM_RETURN_NEWSMOB (cursor_tag, SDL_GetCursor());
}

SCM
show_cursor (SCM s_toggle)
{
   SCM_ASSERT (SCM_INUMP (s_toggle), s_toggle, SCM_ARG1, "show-cursor");
   return SCM_MAKINUM (SDL_ShowCursor (SCM_INUM (s_toggle)));
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
   SDL_Overlay *overlay;

   SCM_ASSERT_SMOB (s_overlay, overlay_tag, SCM_ARG1, "lock-yuv-overlay");
   overlay = (SDL_Overlay*) SCM_SMOB_DATA (s_overlay);

   SCM_RETURN_TRUE_IF_0 (SDL_LockYUVOverlay (overlay));
}

SCM
unlock_yuv_overlay (SCM s_overlay)
{
   SDL_Overlay *overlay;

   SCM_ASSERT_SMOB (s_overlay, overlay_tag, SCM_ARG1, "unlock-yuv-overlay");
   overlay = (SDL_Overlay*) SCM_SMOB_DATA (s_overlay);

   SDL_UnlockYUVOverlay (overlay);
   return SCM_UNSPECIFIED;
}

SCM
display_yuv_overlay (SCM s_overlay, SCM s_dstrect)
{
   SDL_Overlay *overlay;
   SDL_Rect *rect;

   SCM_ASSERT_SMOB (s_overlay, overlay_tag, SCM_ARG1, "display-yuv-overlay");
   SCM_ASSERT_SMOB (s_dstrect, rect_tag, SCM_ARG2, "display-yuv-overlay");

   overlay = (SDL_Overlay*) SCM_SMOB_DATA (s_overlay);
   rect = (SDL_Rect*) SCM_SMOB_DATA (s_dstrect);

   SCM_RETURN_TRUE_IF_0 (SDL_DisplayYUVOverlay (overlay, rect));
}

scm_sizet
free_yuv_overlay (SCM s_overlay)
{
   /* printf ("free_yuv_overlay(%p)\n", s_overlay); */
   SDL_FreeYUVOverlay ((SDL_Overlay*) SCM_SMOB_DATA (s_overlay));
   return sizeof (SDL_Overlay);
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

   scm_set_smob_free (surface_tag, free_surface);
   scm_set_smob_free (rect_tag, free_rect);
   scm_set_smob_free (color_tag, free_color);
   scm_set_smob_free (cursor_tag, free_cursor);
   scm_set_smob_free (overlay_tag, free_yuv_overlay);

   /* rect functions */
   scm_c_define_gsubr ("make-rect",          4, 0, 0, make_rect);
   scm_c_define_gsubr ("rect:x",             1, 0, 0, rect_x);
   scm_c_define_gsubr ("rect:y",             1, 0, 0, rect_y);
   scm_c_define_gsubr ("rect:w",             1, 0, 0, rect_w);
   scm_c_define_gsubr ("rect:h",             1, 0, 0, rect_h);
   scm_c_define_gsubr ("rect:set-x!",        2, 0, 0, rect_set_x);
   scm_c_define_gsubr ("rect:set-y!",        2, 0, 0, rect_set_y);
   scm_c_define_gsubr ("rect:set-w!",        2, 0, 0, rect_set_w);
   scm_c_define_gsubr ("rect:set-h!",        2, 0, 0, rect_set_h);
   /* color functions */
   scm_c_define_gsubr ("make-color",         3, 0, 0, make_color);
   scm_c_define_gsubr ("color:r",            1, 0, 0, color_r);
   scm_c_define_gsubr ("color:g",            1, 0, 0, color_g);
   scm_c_define_gsubr ("color:b",            1, 0, 0, color_b);
   scm_c_define_gsubr ("color:set-r!",       2, 0, 0, color_set_r);
   scm_c_define_gsubr ("color:set-g!",       2, 0, 0, color_set_g);
   scm_c_define_gsubr ("color:set-b!",       2, 0, 0, color_set_b);
   /* pixel formats */
   scm_c_define_gsubr ("map-rgb",            4, 0, 0, map_rgb);
   scm_c_define_gsubr ("map-rgba",           5, 0, 0, map_rgba);
   scm_c_define_gsubr ("get-rgb",            2, 0, 0, get_rgb);
   scm_c_define_gsubr ("get-rgba",           2, 0, 0, get_rgba);
   /* surfaces */
   scm_c_define_gsubr ("get-video-surface",  0, 0, 0, get_video_surface);
   scm_c_define_gsubr ("make-surface",       2, 1, 0, make_surface);
   scm_c_define_gsubr ("create-rgb-surface", 8, 0, 0, create_rgb_surface);
   scm_c_define_gsubr ("lock-surface",       1, 0, 0, lock_surface);
   scm_c_define_gsubr ("unlock-surface",     1, 0, 0, unlock_surface);
   scm_c_define_gsubr ("set-clip-rect!",     2, 0, 0, set_clip_rect);
   scm_c_define_gsubr ("get-clip-rect",      1, 0, 0, get_clip_rect);
   scm_c_define_gsubr ("set-color-key!",     3, 0, 0, set_color_key);
   scm_c_define_gsubr ("set-alpha!",         3, 0, 0, set_alpha);
   scm_c_define_gsubr ("set-gamma",          3, 0, 0, set_gamma);
   scm_c_define_gsubr ("display-format",     1, 0, 0, display_format);
   scm_c_define_gsubr ("display-format-alpha", 1, 0, 0, display_format_alpha);
   scm_c_define_gsubr ("convert-surface",    3, 0, 0, convert_surface);
   /* overlays */
   scm_c_define_gsubr ("create-yuv-overlay", 3, 1, 0, create_yuv_overlay);
   scm_c_define_gsubr ("lock-yuv-overlay",   1, 0, 0, lock_yuv_overlay);
   scm_c_define_gsubr ("unlock-yuv-overlay", 1, 0, 0, unlock_yuv_overlay);
   scm_c_define_gsubr ("display-yuv-overlay", 2, 0, 0, display_yuv_overlay);
   /* video */
   scm_c_define_gsubr ("set-video-mode",     4, 0, 0, set_video_mode);
   scm_c_define_gsubr ("update-rect",        5, 0, 0, update_rect);
   scm_c_define_gsubr ("flip",               0, 1, 0, flip);
   scm_c_define_gsubr ("blit-surface",       4, 0, 0, blit_surface);
   scm_c_define_gsubr ("fill-rect",          3, 0, 0, fill_rect);
   /* cursors */
   scm_c_define_gsubr ("create-cursor",      6, 0, 0, create_cursor);
   scm_c_define_gsubr ("set-cursor",         1, 0, 0, set_cursor);
   scm_c_define_gsubr ("get-cursor",         0, 0, 0, get_cursor);
   scm_c_define_gsubr ("show-cursor",        1, 0, 0, show_cursor);
   scm_c_define_gsubr ("warp-mouse",         2, 0, 0, warp_mouse);
   /* info */
   scm_c_define_gsubr ("list-modes",         0, 2, 0, list_modes);
   scm_c_define_gsubr ("video-mode-ok",      4, 0, 0, video_mode_ok);
   scm_c_define_gsubr ("video-driver-name",  0, 0, 0, video_driver_name);
   scm_c_define_gsubr ("get-video-info",     0, 0, 0, get_video_info);

   /* exported symbols */
   scm_c_export (
      /* rect functions */
      "make-rect", "rect:x", "rect:y", "rect:w", "rect:h",
      "rect:set-x!", "rect:set-y!", "rect:set-w!", "rect:set-h!",
      /* color functions */
      "make-color", "color:r", "color:g", "color:b",
      "color:set-r!", "color:set-g!", "color:set-b!",
      /* pixel formats */
      "map-rgb", "map-rgba", "get-rgb", "get-rgba",
      /* surfaces */
      "get-video-surface", "make-surface", "create-rgb-surface",
      "lock-surface", "unlock-surface", "set-clip-rect!",
      "get-clip-rect", "set-color-key!", "set-alpha!",
      "set-gamma", "display-format", "display-format-alpha",
      "convert-surface",
      /* overlays */
      "create-yuv-overlay", "lock-yuv-overlay", "unlock-yuv-overlay",
      "display-yuv-overlay",
      /* video */
      "set-video-mode", "update-rect", "flip", "blit-surface",
      "fill-rect", "load-bmp", "save-bmp",
      /* cursors */
      "create-cursor", "set-cursor", "get-cursor", "show-cursor",
      "warp-mouse",
      /* info */
      "list-modes", "video-mode-ok", "video-driver-name",
      "get-video-info", NULL);
}

