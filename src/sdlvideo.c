/*******************************************************************
 *  video.c -- SDL Video functions for Guile                       *
 *                                                                 *
 *  Created:    <2001-04-24 23:40:20 foof>                         *
 *  Time-stamp: <2001-07-05 17:15:06 foof>                         *
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
#include "sdlenums.h"
#include "sdlsmobs.h"

#define MAX_DRIVER_LEN    100
#define GAMMA_TABLE_SIZE  256

/* tags for SDL smobs */
long surface_tag;
long cursor_tag;
long rect_tag;
long color_tag;
long palette_tag;
long pixel_format_tag;
long overlay_tag;
long video_info_tag;

SCM sdl_video_flags;
SCM sdl_gl_enums;

/* surfaces */

scm_sizet
free_surface (SCM surface)
{
   /* printf ("free_surface(%p)\n", (SDL_Surface*) SCM_SMOB_DATA (surface)); */
   SDL_FreeSurface ((SDL_Surface*) SCM_SMOB_DATA (surface));
   return sizeof (SDL_Surface);
}

/* Load an image in one of many formats */
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
   SCM_ASSERT (scm_exact_p (s_width),  s_width,  SCM_ARG1, "sdl-make-surface");
   SCM_ASSERT (scm_exact_p (s_height), s_height, SCM_ARG2, "sdl-make-surface");

   width  = scm_num2long (s_width, SCM_ARG1, "scm_num2long");
   height = scm_num2long (s_height, SCM_ARG1, "scm_num2long");

   /* flags are optional, defaulting to current screen flags */
   if (s_flags == SCM_UNDEFINED) {
      flags = SDL_GetVideoSurface()->flags;
   } else {
      SCM_ASSERT (scm_exact_p (s_flags),  s_flags,  SCM_ARG3, "sdl-make-surface");
      flags = (Uint32) scm_num2long (s_flags, SCM_ARG1, "scm_num2long");
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
   SCM_ASSERT (scm_exact_p (s_flags),  s_flags,  SCM_ARG1, "sdl-create-rgb-surface");
   SCM_ASSERT (scm_exact_p (s_width),  s_width,  SCM_ARG2, "sdl-create-rgb-surface");
   SCM_ASSERT (scm_exact_p (s_height), s_height, SCM_ARG3, "sdl-create-rgb-surface");
   SCM_ASSERT (scm_exact_p (s_depth),  s_depth,  SCM_ARG4, "sdl-create-rgb-surface");
   SCM_ASSERT (scm_exact_p (s_rmask),  s_rmask,  SCM_ARG5, "sdl-create-rgb-surface");
   SCM_ASSERT (scm_exact_p (s_gmask),  s_gmask,  SCM_ARG6, "sdl-create-rgb-surface");
   SCM_ASSERT (scm_exact_p (s_bmask),  s_bmask,  SCM_ARG7, "sdl-create-rgb-surface");
   SCM_ASSERT (scm_exact_p (s_amask),  s_amask,  SCM_ARGn, "sdl-create-rgb-surface");

   flags  = (Uint32) scm_num2long (s_flags, SCM_ARG1, "scm_num2long");
   width  = scm_num2long (s_width, SCM_ARG1, "scm_num2long");
   height = scm_num2long (s_height, SCM_ARG1, "scm_num2long");
   depth  = (Uint8)  scm_num2long (s_depth, SCM_ARG1, "scm_num2long");
   rmask  = (Uint32) scm_num2long (s_rmask, SCM_ARG1, "scm_num2long");
   gmask  = (Uint32) scm_num2long (s_gmask, SCM_ARG1, "scm_num2long");
   bmask  = (Uint32) scm_num2long (s_bmask, SCM_ARG1, "scm_num2long");
   amask  = (Uint32) scm_num2long (s_amask, SCM_ARG1, "scm_num2long");

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

/* surface getters */
SCM_DEFINE_NUMBER_GETTER ("sdl-surface:w", surface_w, surface_tag, SDL_Surface*, w)
SCM_DEFINE_NUMBER_GETTER ("sdl-surface:h", surface_h, surface_tag, SDL_Surface*, h)
SCM_DEFINE_NUMBER_GETTER ("sdl-surface:flags", surface_flags, surface_tag,
                        SDL_Surface*, flags)
SCM_DEFINE_NUMBER_GETTER ("sdl-surface:depth", surface_depth, surface_tag,
                        SDL_Surface*, format->BitsPerPixel)


SCM
create_cursor (SCM s_data, SCM s_mask, SCM s_w, SCM s_h,
               SCM s_hot_x, SCM s_hot_y)
{
   SDL_Cursor *cursor;
   Uint8 *data, *mask;
   int data_len, mask_len;
   int i, w, h, hot_x, hot_y;

   /* validate args */
   SCM_ASSERT (scm_vector_p (s_data), s_data, SCM_ARG1, "sdl-create-cursor");
   SCM_ASSERT (scm_vector_p (s_mask), s_mask, SCM_ARG2, "sdl-create-cursor");
   SCM_ASSERT (scm_exact_p (s_w), s_w, SCM_ARG3, "sdl-create-cursor");
   SCM_ASSERT (scm_exact_p (s_h), s_h, SCM_ARG4, "sdl-create-cursor");
   SCM_ASSERT (scm_exact_p (s_hot_x), s_hot_x, SCM_ARG5, "sdl-create-cursor");
   SCM_ASSERT (scm_exact_p (s_hot_y), s_hot_y, SCM_ARG5, "sdl-create-cursor");

   /* build the arrays */
   data_len = scm_num2long (scm_vector_length (s_data), SCM_ARG1, "scm_num2long");
   data = scm_must_malloc (data_len, "sdl-create-cursor data array");
   for (i=0; i<data_len; i++) {
      data[i] = (Uint8) scm_num2long (scm_vector_ref (s_data, scm_long2num (i)),
                                      SCM_ARG1, "scm_num2long");
   }
   mask_len = scm_num2long (scm_vector_length (s_mask), SCM_ARG1, "scm_num2long");
   mask = scm_must_malloc (mask_len, "sdl-create-cursor mask array");
   for (i=0; i<mask_len; i++) {
      mask[i] = (Uint8) scm_num2long (scm_vector_ref (s_mask, scm_long2num (i)),
                                      SCM_ARG1, "scm_num2long");
   }

   /* numbers */
   w = scm_num2long (s_w, SCM_ARG1, "scm_num2long");
   h = scm_num2long (s_h, SCM_ARG1, "scm_num2long");
   hot_x = scm_num2long (s_hot_x, SCM_ARG1, "scm_num2long");
   hot_y = scm_num2long (s_hot_y, SCM_ARG1, "scm_num2long");

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

   SCM_ASSERT (scm_exact_p (s_width), s_width, SCM_ARG1, "sdl-create-yuv-overlay");
   SCM_ASSERT (scm_exact_p (s_height), s_height, SCM_ARG2, "sdl-create-yuv-overlay");
   SCM_ASSERT (scm_exact_p (s_format), s_format, SCM_ARG3, "sdl-create-yuv-overlay");

   width = scm_num2long (s_width, SCM_ARG1, "scm_num2long");
   height = scm_num2long (s_height, SCM_ARG1, "scm_num2long");
   format = (Uint32) scm_num2long (s_format, SCM_ARG1, "scm_num2long");

   if (s_display == SCM_UNDEFINED) {
      display = SDL_GetVideoSurface ();
   } else {
      SCM_ASSERT_SMOB (s_display, surface_tag, SCM_ARG4, "sdl-create-yuv-overlay");
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

   SCM_ASSERT (scm_exact_p (s_x),  s_x,  SCM_ARG1, "sdl-make-rect");
   SCM_ASSERT (scm_exact_p (s_y),  s_y,  SCM_ARG2, "sdl-make-rect");
   SCM_ASSERT (scm_exact_p (s_w),  s_w,  SCM_ARG3, "sdl-make-rect");
   SCM_ASSERT (scm_exact_p (s_h),  s_h,  SCM_ARG4, "sdl-make-rect");

   rect = (SDL_Rect *) scm_must_malloc (sizeof (SDL_Rect), "sdl-make-rect");
   rect->x = scm_num2long (s_x, SCM_ARG1, "scm_num2long");
   rect->y = scm_num2long (s_y, SCM_ARG1, "scm_num2long");
   rect->w = scm_num2long (s_w, SCM_ARG1, "scm_num2long");
   rect->h = scm_num2long (s_h, SCM_ARG1, "scm_num2long");

   SCM_RETURN_NEWSMOB (rect_tag, rect);
}

/* rect getters */
SCM_DEFINE_NUMBER_GETTER ("sdl-rect:x", rect_x, rect_tag, SDL_Rect*, x)
SCM_DEFINE_NUMBER_GETTER ("sdl-rect:y", rect_y, rect_tag, SDL_Rect*, y)
SCM_DEFINE_NUMBER_GETTER ("sdl-rect:w", rect_w, rect_tag, SDL_Rect*, w)
SCM_DEFINE_NUMBER_GETTER ("sdl-rect:h", rect_h, rect_tag, SDL_Rect*, h)

/* rect setters */
SCM_DEFINE_NUMBER_SETTER ("sdl-rect:set-x!", rect_set_x, rect_tag, SDL_Rect*, x)
SCM_DEFINE_NUMBER_SETTER ("sdl-rect:set-y!", rect_set_y, rect_tag, SDL_Rect*, y)
SCM_DEFINE_NUMBER_SETTER ("sdl-rect:set-w!", rect_set_w, rect_tag, SDL_Rect*, w)
SCM_DEFINE_NUMBER_SETTER ("sdl-rect:set-h!", rect_set_h, rect_tag, SDL_Rect*, h)


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

   SCM_ASSERT (scm_exact_p (s_r),  s_r,  SCM_ARG1, "sdl-make-color");
   SCM_ASSERT (scm_exact_p (s_g),  s_g,  SCM_ARG2, "sdl-make-color");
   SCM_ASSERT (scm_exact_p (s_b),  s_b,  SCM_ARG3, "sdl-make-color");

   color = (SDL_Color *) scm_must_malloc (sizeof (SDL_Color), "sdl-make-color");
   color->r = scm_num2long (s_r, SCM_ARG1, "scm_num2long");
   color->g = scm_num2long (s_g, SCM_ARG1, "scm_num2long");
   color->b = scm_num2long (s_b, SCM_ARG1, "scm_num2long");

   SCM_RETURN_NEWSMOB (color_tag, color);
}

/* color getters */
SCM_DEFINE_NUMBER_GETTER ("sdl-color:r", color_r, color_tag, SDL_Color*, r)
SCM_DEFINE_NUMBER_GETTER ("sdl-color:g", color_g, color_tag, SDL_Color*, g)
SCM_DEFINE_NUMBER_GETTER ("sdl-color:b", color_b, color_tag, SDL_Color*, b)

/* color setters */
SCM_DEFINE_NUMBER_SETTER ("sdl-color:set-r!", color_set_r, color_tag, SDL_Color*, r)
SCM_DEFINE_NUMBER_SETTER ("sdl-color:set-g!", color_set_g, color_tag, SDL_Color*, g)
SCM_DEFINE_NUMBER_SETTER ("sdl-color:set-b!", color_set_b, color_tag, SDL_Color*, b)


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
      SCM_ASSERT_SMOB (s_pixel_format, pixel_format_tag, SCM_ARG1, "sdl-list-modes");
      format = (SDL_PixelFormat *) SCM_SMOB_DATA (s_pixel_format);
   }

   /* if flags are given, verify and unpack them */
   if (s_flags != SCM_UNDEFINED) {
      /* SCM_ASSERT (scm_exact_p (s_flags), s_flags, SCM_ARG2, "sdl-list-modes"); */
      flags  = (Uint32) scm_flags2ulong (s_flags, sdl_video_flags,
                                         SCM_ARG2, "sdl-list-modes");
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

   SCM_ASSERT (scm_exact_p (s_width),  s_width,  SCM_ARG1, "sdl-video-mode-ok");
   SCM_ASSERT (scm_exact_p (s_height), s_height, SCM_ARG2, "sdl-video-mode-ok");
   SCM_ASSERT (scm_exact_p (s_bpp),    s_bpp,    SCM_ARG3, "sdl-video-mode-ok");
   SCM_ASSERT (scm_exact_p (s_flags),  s_flags,  SCM_ARG4, "sdl-video-mode-ok");

   width  = scm_num2long (s_width, SCM_ARG1, "scm_num2long");
   height = scm_num2long (s_height, SCM_ARG1, "scm_num2long");
   bpp    = scm_num2long (s_bpp, SCM_ARG1, "scm_num2long");
   flags  = (Uint32) scm_num2long (s_flags, SCM_ARG1, "scm_num2long");

   result = SDL_VideoModeOK (width, height, bpp, flags);
   if (result) {
      return scm_long2num (result);
   } else {
      return SCM_BOOL_F;
   }
}

SCM
set_video_mode (SCM s_width, SCM s_height, SCM s_bpp, SCM s_flags)
{
   int width, height, bpp;
   Uint32 flags=0;

   SCM_ASSERT (scm_exact_p (s_width),  s_width,  SCM_ARG1, "sdl-set-video-mode");
   SCM_ASSERT (scm_exact_p (s_height), s_height, SCM_ARG2, "sdl-set-video-mode");
   SCM_ASSERT (scm_exact_p (s_bpp),    s_bpp,    SCM_ARG3, "sdl-set-video-mode");

   width  = scm_num2long (s_width, SCM_ARG1, "scm_num2long");
   height = scm_num2long (s_height, SCM_ARG1, "scm_num2long");
   bpp    = scm_num2long (s_bpp, SCM_ARG1, "scm_num2long");

   if (s_flags != SCM_UNDEFINED) {
     flags = (Uint32) scm_flags2ulong (s_flags, sdl_video_flags,
                                       SCM_ARG4, "sdl-set-video-mode");
   }

   SCM_RETURN_NEWSMOB (surface_tag, SDL_SetVideoMode (width, height, bpp, flags));
}

SCM
update_rect (SCM s_screen, SCM s_x, SCM s_y, SCM s_w, SCM s_h)
{
   SDL_Surface *screen;
   Sint32 x, y, w, h;

   SCM_ASSERT_SMOB (s_screen, surface_tag, SCM_ARG1, "sdl-update-rect");
   SCM_ASSERT (scm_exact_p (s_x), s_x, SCM_ARG2, "sdl-update-rect");
   SCM_ASSERT (scm_exact_p (s_y), s_y, SCM_ARG3, "sdl-update-rect");
   SCM_ASSERT (scm_exact_p (s_w), s_w, SCM_ARG4, "sdl-update-rect");
   SCM_ASSERT (scm_exact_p (s_h), s_h, SCM_ARG5, "sdl-update-rect");

   screen = (SDL_Surface *) SCM_SMOB_DATA (s_screen);
   x = (Sint32) scm_num2long (s_x, SCM_ARG1, "scm_num2long");
   y = (Sint32) scm_num2long (s_y, SCM_ARG1, "scm_num2long");
   w = (Sint32) scm_num2long (s_w, SCM_ARG1, "scm_num2long");
   h = (Sint32) scm_num2long (s_h, SCM_ARG1, "scm_num2long");

   SDL_UpdateRect (screen, x, y, w, h);

   return SCM_UNSPECIFIED;
}

SCM
flip (SCM s_screen)
{
   SDL_Surface *screen;

   if (s_screen != SCM_UNDEFINED) {
      /* verify and unpack a surface */
      SCM_ASSERT_SMOB (s_screen, surface_tag, SCM_ARG1, "sdl-flip");
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
   SDL_Surface *surface;
   SDL_Color *colors;
   SDL_Color *color;
   int i, length, result;

   SCM_ASSERT_SMOB (s_surface, surface_tag, SCM_ARG1, "sdl-set-colors!");
   SCM_ASSERT (SCM_VECTORP (s_colors), s_colors, SCM_ARG2, "sdl-set-colors!");

   surface = (SDL_Surface*) SCM_SMOB_DATA (s_surface);
   length = SCM_VECTOR_LENGTH (s_colors);
   colors = (SDL_Color*) scm_must_malloc (length, "sdl-set-colors!");

   for (i=0; i<length; i++) {
      color = (SDL_Color*) SCM_SMOB_DATA
         (scm_vector_ref (s_colors, scm_long2num (i)));
      colors[i] = *color;
   }

   result = SDL_SetColors (surface, colors, 0, length);
   scm_must_free (colors);

   return result ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM
set_palette (SCM s_surface, SCM s_flags, SCM s_colors)
{
   SDL_Surface *surface;
   SDL_Color *colors;
   SDL_Color *color;
   int flags, i, length, result;

   SCM_ASSERT_SMOB (s_surface, surface_tag, SCM_ARG1, "sdl-set-palette!");
   SCM_ASSERT (scm_exact_p (s_flags), s_flags, SCM_ARG2, "sdl-set-palette!");
   SCM_ASSERT (SCM_VECTORP (s_colors), s_colors, SCM_ARG3, "sdl-set-palette!");

   surface = (SDL_Surface*) SCM_SMOB_DATA (s_surface);
   flags   = scm_num2long (s_flags, SCM_ARG1, "scm_num2long");
   length  = SCM_VECTOR_LENGTH (s_colors);
   colors  = (SDL_Color*) scm_must_malloc (length, "sdl-set-palette!");

   for (i=0; i<length; i++) {
      color = (SDL_Color*) SCM_SMOB_DATA
         (scm_vector_ref (s_colors, scm_long2num (i)));
      colors[i] = *color;
   }

   result = SDL_SetPalette (surface, flags, colors, 0, length);
   scm_must_free (colors);

   return result ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM
set_gamma (SCM s_redgamma, SCM s_greengamma, SCM s_bluegamma)
{
   float redgamma, greengamma, bluegamma;

   SCM_ASSERT (scm_number_p (s_redgamma),   s_redgamma,   SCM_ARG1, "sdl-set-gamma");
   SCM_ASSERT (scm_number_p (s_greengamma), s_greengamma, SCM_ARG2, "sdl-set-gamma");
   SCM_ASSERT (scm_number_p (s_bluegamma),  s_bluegamma,  SCM_ARG3, "sdl-set-gamma");

   redgamma   = (float) SCM_REAL_VALUE (s_redgamma);
   greengamma = (float) SCM_REAL_VALUE (s_greengamma);
   bluegamma  = (float) SCM_REAL_VALUE (s_bluegamma);

   SCM_RETURN_TRUE_IF_0 (SDL_SetGamma (redgamma, greengamma, bluegamma));
}

SCM
get_gamma_ramp (void)
{
   SCM redtable, greentable, bluetable, prot;
   Uint16 rt[GAMMA_TABLE_SIZE], gt[GAMMA_TABLE_SIZE], bt[GAMMA_TABLE_SIZE];
   int i;

   if (SDL_GetGammaRamp (rt, gt, bt) != -1) {
      /* no error, translate the tables */
      prot = scm_long2num (0);
      redtable   = scm_make_uve (GAMMA_TABLE_SIZE, prot);
      greentable = scm_make_uve (GAMMA_TABLE_SIZE, prot);
      bluetable  = scm_make_uve (GAMMA_TABLE_SIZE, prot);
      /* loop through and copy the elements */
      for (i=0; i<GAMMA_TABLE_SIZE; i++) {
         scm_vector_set_x (redtable,   scm_long2num (i), scm_long2num (rt[i]));
         scm_vector_set_x (greentable, scm_long2num (i), scm_long2num (gt[i]));
         scm_vector_set_x (bluetable,  scm_long2num (i), scm_long2num (bt[i]));
      }
      /* return a list of red, green and blue tables */
      return SCM_LIST3 (redtable, greentable, bluetable);
   } else {
      /* error, return false */
      return SCM_BOOL_F;
   }
}

SCM
set_gamma_ramp (SCM s_redtable, SCM s_greentable, SCM s_bluetable)
{
   Uint16 rt[GAMMA_TABLE_SIZE], gt[GAMMA_TABLE_SIZE], bt[GAMMA_TABLE_SIZE];
   int i;

   SCM_ASSERT (SCM_VECTORP (s_redtable),   s_redtable,
               SCM_ARG1, "sdl-set-gamma-ramp");
   SCM_ASSERT (SCM_VECTORP (s_greentable), s_greentable,
               SCM_ARG2, "sdl-set-gamma-ramp");
   SCM_ASSERT (SCM_VECTORP (s_bluetable),  s_bluetable,
               SCM_ARG3, "sdl-set-gamma-ramp");

   /* loop through and copy the elements */
   for (i=0; i<GAMMA_TABLE_SIZE; i++) {
      rt[i] = scm_num2long (scm_vector_ref (s_redtable,   scm_long2num (i)),
                            SCM_ARG1, "scm_num2long");
      gt[i] = scm_num2long (scm_vector_ref (s_greentable, scm_long2num (i)),
                            SCM_ARG1, "scm_num2long");
      bt[i] = scm_num2long (scm_vector_ref (s_bluetable,  scm_long2num (i)),
                            SCM_ARG1, "scm_num2long");
   }

   SCM_RETURN_TRUE_IF_0 (SDL_SetGammaRamp (rt, gt, bt));
}

SCM
map_rgb (SCM s_pixel_fmt, SCM s_r, SCM s_g, SCM s_b)
{
   SDL_PixelFormat *fmt;
   Uint8 r, g, b;

   SCM_ASSERT_SMOB (s_pixel_fmt, pixel_format_tag, SCM_ARG1, "sdl-map-rbg");
   SCM_ASSERT (scm_exact_p (s_r), s_r, SCM_ARG2, "sdl-map-rgb");
   SCM_ASSERT (scm_exact_p (s_g), s_g, SCM_ARG3, "sdl-map-rgb");
   SCM_ASSERT (scm_exact_p (s_b), s_b, SCM_ARG4, "sdl-map-rgb");

   fmt = (SDL_PixelFormat*) SCM_SMOB_DATA (s_pixel_fmt);
   r = (Uint8) scm_num2long (s_r, SCM_ARG1, "scm_num2long");
   g = (Uint8) scm_num2long (s_g, SCM_ARG1, "scm_num2long");
   b = (Uint8) scm_num2long (s_b, SCM_ARG1, "scm_num2long");

   return scm_long2num (SDL_MapRGB (fmt, r, g, b));
}

SCM
map_rgba (SCM s_pixel_fmt, SCM s_r, SCM s_g, SCM s_b, SCM s_a)
{
   SDL_PixelFormat *fmt;
   Uint8 r, g, b, a;

   SCM_ASSERT_SMOB (s_pixel_fmt, pixel_format_tag, SCM_ARG1, "sdl-map-rbga");
   SCM_ASSERT (scm_exact_p (s_r), s_r, SCM_ARG2, "sdl-map-rgba");
   SCM_ASSERT (scm_exact_p (s_g), s_g, SCM_ARG3, "sdl-map-rgba");
   SCM_ASSERT (scm_exact_p (s_b), s_b, SCM_ARG4, "sdl-map-rgba");
   SCM_ASSERT (scm_exact_p (s_a), s_a, SCM_ARG5, "sdl-map-rgba");

   fmt = (SDL_PixelFormat*) SCM_SMOB_DATA (s_pixel_fmt);
   r = (Uint8) scm_num2long (s_r, SCM_ARG1, "scm_num2long");
   g = (Uint8) scm_num2long (s_g, SCM_ARG1, "scm_num2long");
   b = (Uint8) scm_num2long (s_b, SCM_ARG1, "scm_num2long");
   a = (Uint8) scm_num2long (s_a, SCM_ARG1, "scm_num2long");

   return scm_long2num (SDL_MapRGBA (fmt, r, g, b, a));
}

SCM
get_rgb (SCM s_pixel, SCM s_pixel_fmt)
{
   SDL_PixelFormat *fmt;
   Uint32 pixel;
   Uint8 r, g, b;

   SCM_ASSERT (scm_exact_p (s_pixel), s_pixel, SCM_ARG1, "sdl-get-rgb");
   SCM_ASSERT_SMOB (s_pixel_fmt, pixel_format_tag, SCM_ARG2, "sdl-get-rbg");

   fmt = (SDL_PixelFormat*) SCM_SMOB_DATA (s_pixel_fmt);
   pixel = (Uint32) scm_num2long (s_pixel, SCM_ARG1, "scm_num2long");

   SDL_GetRGB (pixel, fmt, &r, &g, &b);

   return SCM_LIST3 (scm_long2num (r), scm_long2num (g), scm_long2num (b));
}

SCM
get_rgba (SCM s_pixel, SCM s_pixel_fmt)
{
   SDL_PixelFormat *fmt;
   Uint32 pixel;
   Uint8 r, g, b, a;

   SCM_ASSERT (scm_exact_p (s_pixel), s_pixel, SCM_ARG1, "sdl-get-rgba");
   SCM_ASSERT_SMOB (s_pixel_fmt, pixel_format_tag, SCM_ARG2, "sdl-get-rbga");

   fmt = (SDL_PixelFormat*) SCM_SMOB_DATA (s_pixel_fmt);
   pixel = (Uint32) scm_num2long (s_pixel, SCM_ARG1, "scm_num2long");

   SDL_GetRGBA (pixel, fmt, &r, &g, &b, &a);

   return SCM_LIST4 (scm_long2num (r), scm_long2num (g),
                     scm_long2num (b), scm_long2num (a));
}

SCM
lock_surface (SCM s_surface)
{
   SDL_Surface *surface;

   SCM_ASSERT_SMOB (s_surface, surface_tag, SCM_ARG1, "sdl-lock-surface");
   surface = (SDL_Surface*) SCM_SMOB_DATA (s_surface);

   SCM_RETURN_TRUE_IF_0 (SDL_LockSurface (surface));
}

SCM
unlock_surface (SCM s_surface)
{
   SDL_Surface *surface;

   SCM_ASSERT_SMOB (s_surface, surface_tag, SCM_ARG1, "sdl-unlock-surface");
   surface = (SDL_Surface*) SCM_SMOB_DATA (s_surface);

   SDL_UnlockSurface (surface);

   return SCM_UNSPECIFIED;
}

SCM
load_bmp (SCM s_file)
{
   SDL_Surface *surface;
   const char *file;

   SCM_ASSERT (SCM_STRINGP (s_file), s_file, SCM_ARG1, "sdl-load-bmp");
   file = SCM_STRING_CHARS (s_file);

   surface = SDL_LoadBMP (file);

   SCM_RETURN_NEWSMOB (surface_tag, surface);
}

SCM
save_bmp (SCM s_surface, SCM s_file)
{
   SDL_Surface *surface;
   const char *file;

   SCM_ASSERT_SMOB (s_surface, surface_tag,  SCM_ARG1, "sdl-save-bmp");
   SCM_ASSERT (SCM_STRINGP (s_file), s_file, SCM_ARG2, "sdl-save-bmp");

   surface = (SDL_Surface*) SCM_SMOB_DATA (s_surface);
   file = SCM_STRING_CHARS (s_file);

   SCM_RETURN_TRUE_IF_0 (SDL_SaveBMP (surface, file));
}

SCM
set_color_key (SCM s_surface, SCM s_flag, SCM s_key)
{
   SDL_Surface *surface;
   Uint32 flag, key;

   SCM_ASSERT_SMOB (s_surface, surface_tag, SCM_ARG1, "sdl-set-color-key!");
   SCM_ASSERT (scm_exact_p (s_flag), s_flag,  SCM_ARG2, "sdl-set-color-key!");
   SCM_ASSERT (scm_exact_p (s_key),  s_key,   SCM_ARG3, "sdl-set-color-key!");

   surface = (SDL_Surface*) SCM_SMOB_DATA (s_surface);
   flag = (Uint32) scm_num2long (s_flag, SCM_ARG1, "scm_num2long");
   key  = (Uint32) scm_num2long (s_key, SCM_ARG1, "scm_num2long");

   SCM_RETURN_TRUE_IF_0 (SDL_SetColorKey (surface, flag, key));
}

SCM
set_alpha (SCM s_surface, SCM s_flag, SCM s_alpha)
{
   SDL_Surface *surface;
   Uint32 flag;
   Uint8 alpha;

   SCM_ASSERT_SMOB (s_surface, surface_tag,  SCM_ARG1, "sdl-set-alpha!");
   SCM_ASSERT (scm_exact_p (s_flag), s_flag,   SCM_ARG2, "sdl-set-alpha!");
   SCM_ASSERT (scm_exact_p (s_alpha), s_alpha, SCM_ARG3, "sdl-set-alpha!");

   surface = (SDL_Surface*) SCM_SMOB_DATA (s_surface);
   flag  = (Uint32) scm_num2long (s_flag, SCM_ARG1, "scm_num2long");
   alpha = (Uint8)  scm_num2long (s_alpha, SCM_ARG1, "scm_num2long");

   SCM_RETURN_TRUE_IF_0 (SDL_SetAlpha (surface, flag, alpha));
}

SCM
set_clip_rect (SCM s_surface, SCM s_rect)
{
   SDL_Surface *surface;
   SDL_Rect *rect=NULL;

   SCM_ASSERT_SMOB (s_surface, surface_tag,  SCM_ARG1, "sdl-set-clip-rect!");

   surface = (SDL_Surface*) SCM_SMOB_DATA (s_surface);

   if (s_rect != SCM_UNDEFINED) {
      /* rect defaults to NULL (the whole surface) */
      SCM_ASSERT_SMOB (s_rect,    rect_tag,     SCM_ARG2, "sdl-set-clip-rect!");
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

   SCM_ASSERT_SMOB (s_surface, surface_tag,  SCM_ARG1, "sdl-get-clip-rect");

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

   SCM_ASSERT_SMOB (s_src, surface_tag,  SCM_ARG1, "sdl-convert-surface");
   SCM_ASSERT_SMOB (s_fmt, pixel_format_tag, SCM_ARG2, "sdl-convert-surface");
   SCM_ASSERT (scm_exact_p (s_flags), s_flags, SCM_ARG3, "sdl-convert-surface");

   src = (SDL_Surface*) SCM_SMOB_DATA (s_src);
   fmt = (SDL_PixelFormat*) SCM_SMOB_DATA (s_fmt);
   flags = (Uint32) scm_num2long (s_flags, SCM_ARG1, "scm_num2long");

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
   SDL_Rect default_rect;

   /* 1st arg, source surface */
   SCM_ASSERT_SMOB (s_src, surface_tag,  SCM_ARG1, "sdl-blit-surface");
   src = (SDL_Surface *)  SCM_SMOB_DATA (s_src);

   /* 2nd arg, source rect, default (0,0) by source dimensions */
   if (s_srcrect != SCM_UNDEFINED) {
      SCM_ASSERT_SMOB (s_srcrect, rect_tag, SCM_ARG2, "sdl-blit-surface");
      srcrect = (SDL_Rect *) SCM_SMOB_DATA (s_srcrect);
   } else {
      default_rect.x = 0;
      default_rect.y = 0;
      default_rect.w = src->w;
      default_rect.h = src->h;
      srcrect = &default_rect;
   }

   /* 3rd arg, dest surface, default video surface */
   if (s_dst != SCM_UNDEFINED) {
      SCM_ASSERT_SMOB (s_dst, surface_tag,  SCM_ARG3, "sdl-blit-surface");
      dst = (SDL_Surface *)  SCM_SMOB_DATA (s_dst);
   } else {
      dst = SDL_GetVideoSurface ();
   }

   /* 4th arg, dest rect, default src rect */
   if (s_dstrect != SCM_UNDEFINED) {
      SCM_ASSERT_SMOB (s_dstrect, rect_tag, SCM_ARG4, "sdl-blit-surface");
      dstrect = (SDL_Rect *) SCM_SMOB_DATA (s_dstrect);
   } else {
      dstrect = &default_rect;
   }

   return scm_long2num (SDL_BlitSurface (src, srcrect, dst, dstrect));
}

SCM
fill_rect (SCM s_dst, SCM s_dstrect, SCM s_color)
{
   SDL_Surface *dst;
   SDL_Rect *dstrect;
   Uint32 color;

   SCM_ASSERT_SMOB (s_dst, surface_tag,  SCM_ARG1, "sdl-fill-rect");
   SCM_ASSERT_SMOB (s_dstrect, rect_tag, SCM_ARG2, "sdl-fill-rect");
   SCM_ASSERT (scm_exact_p (s_color), s_color, SCM_ARG3, "sdl-fill-rect");

   dst = (SDL_Surface *) SCM_SMOB_DATA (s_dst);
   dstrect = (SDL_Rect *) SCM_SMOB_DATA (s_dstrect);
   color = (Uint32) scm_num2long (s_color, SCM_ARG1, "scm_num2long");

   return scm_long2num (SDL_FillRect (dst, dstrect, color));
}

SCM
display_format (SCM s_surface)
{
   SDL_Surface *surface;

   SCM_ASSERT_SMOB (s_surface, surface_tag,  SCM_ARG1, "sdl-display-format");

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

   SCM_ASSERT_SMOB (s_surface, surface_tag,  SCM_ARG1, "sdl-display-format-alpha");

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

   SCM_ASSERT (scm_exact_p (s_x), s_x, SCM_ARG1, "sdl-warp-mouse");
   SCM_ASSERT (scm_exact_p (s_y), s_y, SCM_ARG2, "sdl-warp-mouse");

   x = (Uint16) scm_num2long (s_x, SCM_ARG1, "scm_num2long");
   y = (Uint16) scm_num2long (s_y, SCM_ARG1, "scm_num2long");

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
   SCM_ASSERT_SMOB (s_cursor, cursor_tag, SCM_ARG1, "sdl-set-cursor");
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
   SCM_ASSERT (scm_exact_p (s_toggle), s_toggle, SCM_ARG1, "sdl-show-cursor");
   return scm_long2num (SDL_ShowCursor (scm_num2long (s_toggle, SCM_ARG1, "scm_num2long")));
}

/* SCM */
/* gl_load_library (SCM s_path) */
/* { */
/*    return SCM_UNSPECIFIED; */
/* } */

/* SCM */
/* gl_get_proc_address (SCM s_proc) */
/* { */
/*    return SCM_UNSPECIFIED; */
/* } */

SCM
gl_get_attribute (SCM s_attr)
{
   SDL_GLattr attr;
   int value;

   SCM_ASSERT (scm_exact_p (s_attr), s_attr, SCM_ARG1, "sdl-gl-get-attribute");
   attr = (SDL_GLattr) scm_num2long (s_attr, SCM_ARG1, "scm_num2long");

   SDL_GL_GetAttribute(attr, &value);

   return scm_long2num (value);
}

SCM
gl_set_attribute (SCM s_attr, SCM s_value)
{
   SDL_GLattr attr;
   int value;

   SCM_ASSERT (scm_exact_p (s_attr), s_attr, SCM_ARG1, "sdl-gl-set-attribute");
   SCM_ASSERT (scm_exact_p (s_value), s_value, SCM_ARG2, "sdl-gl-set-attribute");

   attr = (SDL_GLattr) scm_num2long (s_attr, SCM_ARG1, "scm_num2long");
   value = (int) scm_num2long (s_value, SCM_ARG1, "scm_num2long");

   SDL_GL_SetAttribute(attr, value);

   return SCM_UNSPECIFIED;
}

SCM
gl_swap_buffers (void)
{
   SDL_GL_SwapBuffers ();
   return SCM_UNSPECIFIED;
}

SCM
lock_yuv_overlay (SCM s_overlay)
{
   SDL_Overlay *overlay;

   SCM_ASSERT_SMOB (s_overlay, overlay_tag, SCM_ARG1, "sdl-lock-yuv-overlay");
   overlay = (SDL_Overlay*) SCM_SMOB_DATA (s_overlay);

   SCM_RETURN_TRUE_IF_0 (SDL_LockYUVOverlay (overlay));
}

SCM
unlock_yuv_overlay (SCM s_overlay)
{
   SDL_Overlay *overlay;

   SCM_ASSERT_SMOB (s_overlay, overlay_tag, SCM_ARG1, "sdl-unlock-yuv-overlay");
   overlay = (SDL_Overlay*) SCM_SMOB_DATA (s_overlay);

   SDL_UnlockYUVOverlay (overlay);
   return SCM_UNSPECIFIED;
}

SCM
display_yuv_overlay (SCM s_overlay, SCM s_dstrect)
{
   SDL_Overlay *overlay;
   SDL_Rect *rect;

   SCM_ASSERT_SMOB (s_overlay, overlay_tag, SCM_ARG1, "sdl-display-yuv-overlay");
   SCM_ASSERT_SMOB (s_dstrect, rect_tag, SCM_ARG2, "sdl-display-yuv-overlay");

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

/* window manager functions */

SCM
wm_set_caption (SCM title, SCM icon)
{
   SCM_ASSERT ((SCM_NIMP (title) && SCM_STRINGP (title)),
               title, SCM_ARG1, "sdl-set-caption");

   SCM_ASSERT ((SCM_NIMP (icon) && SCM_STRINGP (icon)),
               icon, SCM_ARG1, "sdl-set-caption");

   SDL_WM_SetCaption (SCM_CHARS (title), SCM_CHARS (icon));

   return SCM_UNSPECIFIED;
}

SCM
wm_get_caption (void)
{
   char *title;
   char *icon;

   SDL_WM_GetCaption (&title, &icon);

   return SCM_LIST2 (scm_makfrom0str (title), scm_makfrom0str (icon));
}

SCM
wm_set_icon (SCM icon)
{
   SDL_Surface *surface;

   SCM_ASSERT_SMOB (icon, surface_tag, SCM_ARG1, "sdl-set-icon");
   surface = (SDL_Surface*) SCM_SMOB_DATA (icon);

   /* set w/ a NULL mask for now */
   SDL_WM_SetIcon (surface, NULL);

   return SCM_UNSPECIFIED;
}

SCM
wm_iconify_window (void)
{
   return SDL_WM_IconifyWindow () ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM
wm_toggle_full_screen (SCM s_surface)
{
   SDL_Surface *surface;

   if (s_surface == SCM_UNDEFINED) {
      surface = SDL_GetVideoSurface ();
   } else {
      SCM_ASSERT_SMOB (s_surface, surface_tag, SCM_ARG1, "sdl-toggle-full-screen");
      surface = (SDL_Surface*) SCM_SMOB_DATA (s_surface);
   }

   return SDL_WM_ToggleFullScreen (surface) ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM
wm_grab_input (SCM s_mode)
{
   int mode = SDL_GRAB_QUERY;

   if (s_mode != SCM_UNDEFINED) {
      SCM_ASSERT (scm_exact_p (s_mode), s_mode, SCM_ARG1, "sdl-grab-input");
      mode = scm_num2long (s_mode, SCM_ARG1, "scm_num2long");
   }

   return scm_long2num (SDL_WM_GrabInput (mode));
}

void
sdl_init_video (void)
{
   /* smobs */
   surface_tag   = scm_make_smob_type ("SDL-Surface", sizeof (SDL_Surface));
   rect_tag      = scm_make_smob_type ("SDL-Rect", sizeof (SDL_Rect));
   color_tag     = scm_make_smob_type ("SDL-Color", sizeof (SDL_Color));
   cursor_tag    = scm_make_smob_type ("SDL-Cursor", sizeof (SDL_Cursor));
   palette_tag   = scm_make_smob_type ("SDL-Palette", sizeof (SDL_Palette));
   pixel_format_tag = scm_make_smob_type ("SDL-Pixel-Format", sizeof (SDL_PixelFormat));
   overlay_tag   = scm_make_smob_type ("SDL-Overlay", sizeof (SDL_Overlay));
   video_info_tag = scm_make_smob_type ("SDL-Video-Info", sizeof (SDL_VideoInfo));

   scm_set_smob_free (surface_tag, free_surface);
   scm_set_smob_free (rect_tag, free_rect);
   scm_set_smob_free (color_tag, free_color);
   scm_set_smob_free (cursor_tag, free_cursor);
   scm_set_smob_free (overlay_tag, free_yuv_overlay);

   /* alpha constants */
   SCM_DEFINE_CONST ("sdl-alpha/opaque",      SDL_ALPHA_OPAQUE);
   SCM_DEFINE_CONST ("sdl-alpha/transparent", SDL_ALPHA_TRANSPARENT);

   /* video constants */
   sdl_video_flags = scm_c_define_flag (
    "sdl-video-flags",
    "SDL_SWSURFACE",   SDL_SWSURFACE,   /* Surface is in system memory */
    "SDL_HWSURFACE",   SDL_HWSURFACE,   /* Surface is in video memory */
    "SDL_ASYNCBLIT",   SDL_ASYNCBLIT,   /* Use asynchronous blits if possible */
    /* Available for SDL_SetVideoMode() */
    "SDL_ANYFORMAT",   SDL_ANYFORMAT,   /* Allow any video depth/pixel-format */
    "SDL_HWPALETTE",   SDL_HWPALETTE,   /* Surface has exclusive palette */
    "SDL_DOUBLEBUF",   SDL_DOUBLEBUF,   /* Set up double-buffered video mode */
    "SDL_FULLSCREEN",  SDL_FULLSCREEN,  /* Surface is a full screen display */
    "SDL_OPENGL",      SDL_OPENGL,      /* Create an OpenGL rendering context */
    "SDL_OPENGLBLIT",  SDL_OPENGLBLIT,  /* Create an OpenGL rendering context and use it for blitting */
    "SDL_RESIZABLE",   SDL_RESIZABLE,   /* This video mode may be resized */
    "SDL_NOFRAME",     SDL_NOFRAME,     /* No window caption or edge frame */
    /* Used internally (read-only) */
    "SDL_HWACCEL",     SDL_HWACCEL,     /* Blit uses hardware acceleration */
    "SDL_SRCCOLORKEY", SDL_SRCCOLORKEY, /* Blit uses a source color key */
    "SDL_RLEACCELOK",  SDL_RLEACCELOK,  /* Private flag */
    "SDL_RLEACCEL",    SDL_RLEACCEL,    /* Surface is RLE encoded */
    "SDL_SRCALPHA",    SDL_SRCALPHA,    /* Blit uses source alpha blending */
    "SDL_PREALLOC",    SDL_PREALLOC,    /* Surface uses preallocated memory */
    NULL);

   /* yuv overlay formats (values too large to be made enums) */
   SCM_DEFINE_CONST ("sdl-yv12-overlay", SDL_YV12_OVERLAY);  /* Planar mode: Y + V + U  (3 planes) */
   SCM_DEFINE_CONST ("sdl-iyuv-overlay", SDL_IYUV_OVERLAY);  /* Planar mode: Y + U + V  (3 planes) */
   SCM_DEFINE_CONST ("sdl-yuy2-overlay", SDL_YUY2_OVERLAY);  /* Packed mode: Y0+U0+Y1+V0 (1 plane) */
   SCM_DEFINE_CONST ("sdl-uyvy-overlay", SDL_UYVY_OVERLAY);  /* Packed mode: U0+Y0+V0+Y1 (1 plane) */
   SCM_DEFINE_CONST ("sdl-yvyu-overlay", SDL_YVYU_OVERLAY);  /* Packed mode: Y0+V0+Y1+U0 (1 plane) */

   /* GL constants */
   sdl_gl_enums = scm_c_define_enum (
    "sdl-gl-enums",
    "SDL_GL_RED_SIZE",         SDL_GL_RED_SIZE,
    "SDL_GL_GREEN_SIZE",       SDL_GL_GREEN_SIZE,
    "SDL_GL_BLUE_SIZE",        SDL_GL_BLUE_SIZE,
    "SDL_GL_ALPHA_SIZE",       SDL_GL_ALPHA_SIZE,
    "SDL_GL_BUFFER_SIZE",      SDL_GL_BUFFER_SIZE,
    "SDL_GL_DOUBLEBUFFER",     SDL_GL_DOUBLEBUFFER,
    "SDL_GL_DEPTH_SIZE",       SDL_GL_DEPTH_SIZE,
    "SDL_GL_STENCIL_SIZE",     SDL_GL_STENCIL_SIZE,
    "SDL_GL_ACCUM_RED_SIZE",   SDL_GL_ACCUM_RED_SIZE,
    "SDL_GL_ACCUM_GREEN_SIZE", SDL_GL_ACCUM_GREEN_SIZE,
    "SDL_GL_ACCUM_BLUE_SIZE",  SDL_GL_ACCUM_BLUE_SIZE,
    "SDL_GL_ACCUM_ALPHA_SIZE", SDL_GL_ACCUM_ALPHA_SIZE,
     NULL);

   /* rect functions */
   scm_c_define_gsubr ("sdl-make-rect",          4, 0, 0, make_rect);
   scm_c_define_gsubr ("sdl-rect:x",             1, 0, 0, rect_x);
   scm_c_define_gsubr ("sdl-rect:y",             1, 0, 0, rect_y);
   scm_c_define_gsubr ("sdl-rect:w",             1, 0, 0, rect_w);
   scm_c_define_gsubr ("sdl-rect:h",             1, 0, 0, rect_h);
   scm_c_define_gsubr ("sdl-rect:set-x!",        2, 0, 0, rect_set_x);
   scm_c_define_gsubr ("sdl-rect:set-y!",        2, 0, 0, rect_set_y);
   scm_c_define_gsubr ("sdl-rect:set-w!",        2, 0, 0, rect_set_w);
   scm_c_define_gsubr ("sdl-rect:set-h!",        2, 0, 0, rect_set_h);
   /* color functions */
   scm_c_define_gsubr ("sdl-make-color",         3, 0, 0, make_color);
   scm_c_define_gsubr ("sdl-color:r",            1, 0, 0, color_r);
   scm_c_define_gsubr ("sdl-color:g",            1, 0, 0, color_g);
   scm_c_define_gsubr ("sdl-color:b",            1, 0, 0, color_b);
   scm_c_define_gsubr ("sdl-color:set-r!",       2, 0, 0, color_set_r);
   scm_c_define_gsubr ("sdl-color:set-g!",       2, 0, 0, color_set_g);
   scm_c_define_gsubr ("sdl-color:set-b!",       2, 0, 0, color_set_b);
   /* pixel formats */
   scm_c_define_gsubr ("sdl-map-rgb",            4, 0, 0, map_rgb);
   scm_c_define_gsubr ("sdl-map-rgba",           5, 0, 0, map_rgba);
   scm_c_define_gsubr ("sdl-get-rgb",            2, 0, 0, get_rgb);
   scm_c_define_gsubr ("sdl-get-rgba",           2, 0, 0, get_rgba);
   /* surfaces */
   scm_c_define_gsubr ("sdl-surface:w",          1, 0, 0, surface_w);
   scm_c_define_gsubr ("sdl-surface:h",          1, 0, 0, surface_h);
   scm_c_define_gsubr ("sdl-surface:depth",      1, 0, 0, surface_depth);
   scm_c_define_gsubr ("sdl-surface:flags",      1, 0, 0, surface_flags);
   scm_c_define_gsubr ("sdl-get-video-surface",  0, 0, 0, get_video_surface);
   scm_c_define_gsubr ("sdl-load-image",         1, 0, 0, img_load);
   scm_c_define_gsubr ("sdl-make-surface",       2, 1, 0, make_surface);
   scm_c_define_gsubr ("sdl-create-rgb-surface", 8, 0, 0, create_rgb_surface);
   scm_c_define_gsubr ("sdl-lock-surface",       1, 0, 0, lock_surface);
   scm_c_define_gsubr ("sdl-unlock-surface",     1, 0, 0, unlock_surface);
   scm_c_define_gsubr ("sdl-set-clip-rect!",     2, 0, 0, set_clip_rect);
   scm_c_define_gsubr ("sdl-get-clip-rect",      1, 0, 0, get_clip_rect);
   scm_c_define_gsubr ("sdl-set-color-key!",     3, 0, 0, set_color_key);
   scm_c_define_gsubr ("sdl-set-colors!",        2, 0, 0, set_colors);
   scm_c_define_gsubr ("sdl-set-palette!",       3, 0, 0, set_palette);
   scm_c_define_gsubr ("sdl-set-alpha!",         3, 0, 0, set_alpha);
   scm_c_define_gsubr ("sdl-set-gamma",          3, 0, 0, set_gamma);
   scm_c_define_gsubr ("sdl-set-gamma-ramp",     3, 0, 0, set_gamma_ramp);
   scm_c_define_gsubr ("sdl-get-gamma-ramp",     0, 0, 0, get_gamma_ramp);
   scm_c_define_gsubr ("sdl-display-format",     1, 0, 0, display_format);
   scm_c_define_gsubr ("sdl-display-format-alpha", 1, 0, 0, display_format_alpha);
   scm_c_define_gsubr ("sdl-convert-surface",    3, 0, 0, convert_surface);
   /* overlays */
   scm_c_define_gsubr ("sdl-create-yuv-overlay", 3, 1, 0, create_yuv_overlay);
   scm_c_define_gsubr ("sdl-lock-yuv-overlay",   1, 0, 0, lock_yuv_overlay);
   scm_c_define_gsubr ("sdl-unlock-yuv-overlay", 1, 0, 0, unlock_yuv_overlay);
   scm_c_define_gsubr ("sdl-display-yuv-overlay", 2, 0, 0, display_yuv_overlay);
   /* video */
   scm_c_define_gsubr ("sdl-set-video-mode",     3, 1, 0, set_video_mode);
   scm_c_define_gsubr ("sdl-update-rect",        5, 0, 0, update_rect);
   scm_c_define_gsubr ("sdl-flip",               0, 1, 0, flip);
   scm_c_define_gsubr ("sdl-blit-surface",       1, 3, 0, blit_surface);
   scm_c_define_gsubr ("sdl-fill-rect",          3, 0, 0, fill_rect);
   /* opengl */
   scm_c_define_gsubr ("sdl-gl-swap-buffers",    0, 0, 0, gl_swap_buffers);
   scm_c_define_gsubr ("sdl-gl-get-attribute",   1, 0, 0, gl_get_attribute);
   scm_c_define_gsubr ("sdl-gl-set-attribute",   2, 0, 0, gl_set_attribute);
   /* cursors */
   scm_c_define_gsubr ("sdl-create-cursor",      6, 0, 0, create_cursor);
   scm_c_define_gsubr ("sdl-set-cursor",         1, 0, 0, set_cursor);
   scm_c_define_gsubr ("sdl-get-cursor",         0, 0, 0, get_cursor);
   scm_c_define_gsubr ("sdl-show-cursor",        1, 0, 0, show_cursor);
   scm_c_define_gsubr ("sdl-warp-mouse",         2, 0, 0, warp_mouse);
   /* info */
   scm_c_define_gsubr ("sdl-list-modes",         0, 2, 0, list_modes);
   scm_c_define_gsubr ("sdl-video-mode-ok",      4, 0, 0, video_mode_ok);
   scm_c_define_gsubr ("sdl-video-driver-name",  0, 0, 0, video_driver_name);
   scm_c_define_gsubr ("sdl-get-video-info",     0, 0, 0, get_video_info);

   /* exported symbols */
   scm_c_export (
      /* alpha constants */
      "sdl-alpha/opaque",       "sdl-alpha/transparent",
      /* video constants */
      "sdl-video-flags",
      /* yuv constants */
      "sdl-yv12-overlay",       "sdl-iyuv-overlay",       "sdl-yuy2-overlay",
      "sdl-uyvy-overlay",       "sdl-yvyu-overlay",
      /* GL constants */
      "sdl-gl-enums",
      /* rect functions */
      "sdl-make-rect",   "sdl-rect:x",      "sdl-rect:y",    "sdl-rect:w",
      "sdl-rect:h",      "sdl-rect:set-x!", "sdl-rect:set-y!",
      "sdl-rect:set-w!", "sdl-rect:set-h!",
      /* color functions */
      "sdl-make-color",   "sdl-color:r",  "sdl-color:g",  "sdl-color:b",
      "sdl-color:set-r!", "sdl-color:set-g!", "sdl-color:set-b!",
      /* pixel formats */
      "sdl-map-rgb",  "sdl-map-rgba",  "sdl-get-rgb",  "sdl-get-rgba",
      /* surfaces */
      "sdl-surface:w",          "sdl-surface:h",
      "sdl-surface:depth",      "sdl-surface:flags",
      "sdl-get-video-surface",  "sdl-make-surface",
      "sdl-create-rgb-surface", "sdl-lock-surface",
      "sdl-unlock-surface",     "sdl-set-clip-rect!",
      "sdl-get-clip-rect",      "sdl-set-color-key!",
      "sdl-set-colors!",        "sdl-set-palette!",
      "sdl-set-alpha!",         "sdl-set-gamma",
      "sdl-get-gamma-ramp",     "sdl-set-gamma-ramp",
      "sdl-display-format",     "sdl-display-format-alpha",
      "sdl-convert-surface",    "sdl-load-image",
      /* overlays */
      "sdl-create-yuv-overlay",   "sdl-lock-yuv-overlay",
      "sdl-unlock-yuv-overlay",   "sdl-display-yuv-overlay",
      /* video */
      "sdl-set-video-mode",     "sdl-update-rect",    "sdl-flip",
      "sdl-blit-surface",       "sdl-fill-rect",
      "sdl-load-bmp",           "sdl-save-bmp",
      /* opengl */
      "sdl-get-set-attribute",  "sdl-gl-get-attribute",
      "sdl-gl-swap-buffers",
      /* cursors */
      "sdl-create-cursor",      "sdl-set-cursor",   "sdl-get-cursor",
      "sdl-show-cursor",        "sdl-warp-mouse",
      /* wm functions */
      "sdl-set-caption",        "sdl-get-caption",
      "sdl-set-icon",           "sdl-iconify-window",
      "sdl-toggle-full-screen", "sdl-grab-input",
      /* info */
      "sdl-list-modes",         "sdl-video-mode-ok",
      "sdl-video-driver-name",  "get-video-info",
      NULL);
}

