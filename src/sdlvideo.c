/*******************************************************************
 *  sdlvideo.c -- SDL Video functions for Guile                    *
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
#include "sdlrect.h"
#include "sdlcolor.h"
#include "sdlsurface.h"

#define MAX_DRIVER_LEN    100
#define GAMMA_TABLE_SIZE  256

/* tags for SDL smobs */
long cursor_tag;
long pixel_format_tag;
long overlay_tag;

SCM sdl_video_flags;
SCM sdl_palette_flags;
SCM sdl_alpha_enums;
SCM sdl_gl_enums;

/* smob functions */

size_t
free_cursor (SCM s_cursor)
{
  /* printf ("free_cursor(%p)\n", s_cursor); */
  SDL_FreeCursor ((SDL_Cursor*) SCM_SMOB_DATA (s_cursor));
  /* return sizeof (SDL_Cursor); */
  return 0;
}

size_t
free_yuv_overlay (SCM s_overlay)
{
  /* printf ("free_yuv_overlay(%p)\n", s_overlay); */
  SDL_FreeYUVOverlay ((SDL_Overlay*) SCM_SMOB_DATA (s_overlay));
  /* return sizeof (SDL_Overlay); */
  return 0;
}

size_t
free_pixel_format (SCM s_pixel_format)
{
  /* printf ("free_pixel_format(%p)\n", s_pixel_format); */
  /* always part of a surface, no need to free */
  return 0;
}


/* scheme callable functions */

SCM_DEFINE( create_cursor, "sdl-create-cursor", 6, 0, 0,
            (SCM s_data,
             SCM s_mask,
             SCM s_w,
             SCM s_h,
             SCM s_hot_x,
             SCM s_hot_y),
"Creates a new cursor.")
#define FUNC_NAME s_create_cursor
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
  SCM_ASSERT (scm_exact_p (s_hot_y), s_hot_y, SCM_ARG6, "sdl-create-cursor");

  /* build the arrays */
  data_len = scm_num2long (scm_vector_length (s_data), SCM_ARG1,
                           "sdl-create-cursor");
  data = scm_gc_malloc (data_len, "sdl-create-cursor data array");
  for (i=0; i<data_len; i++) {
    data[i] = (Uint8) scm_num2long (scm_vector_ref (s_data, scm_long2num (i)),
                                    SCM_ARG1, "sdl-create-cursor");
  }
  mask_len = scm_num2long (scm_vector_length (s_mask), SCM_ARG2,
                           "sdl-create-cursor");
  mask = scm_gc_malloc (mask_len, "sdl-create-cursor mask array");
  for (i=0; i<mask_len; i++) {
    mask[i] = (Uint8) scm_num2long (scm_vector_ref (s_mask, scm_long2num (i)),
                                    SCM_ARG2, "sdl-create-cursor");
  }

  /* numbers */
  w = scm_num2long (s_w, SCM_ARG3, "sdl-create-cursor");
  h = scm_num2long (s_h, SCM_ARG4, "sdl-create-cursor");
  hot_x = scm_num2long (s_hot_x, SCM_ARG5, "sdl-create-cursor");
  hot_y = scm_num2long (s_hot_y, SCM_ARG6, "sdl-create-cursor");

  /* create the cursor */
  cursor = SDL_CreateCursor (data, mask, w, h, hot_x, hot_y);

  /* free the arrays */
  scm_must_free (data);
  scm_must_free (mask);

  /* return the new smob */
  SCM_RETURN_NEWSMOB (cursor_tag, cursor);
}
#undef FUNC_NAME


SCM_DEFINE( create_yuv_overlay, "sdl-create-yuv-overlay", 3, 1, 0,
            (SCM s_width,
             SCM s_height,
             SCM s_format,
             SCM s_display),
"Creates a new YUV overlay.")
#define FUNC_NAME s_create_yuv_overlay
{
  int width, height;
  Uint32 format;
  SDL_Surface *display;
  SDL_Overlay *overlay;

  SCM_ASSERT (scm_exact_p (s_width), s_width, SCM_ARG1, "sdl-create-yuv-overlay");
  SCM_ASSERT (scm_exact_p (s_height), s_height, SCM_ARG2, "sdl-create-yuv-overlay");
  SCM_ASSERT (scm_exact_p (s_format), s_format, SCM_ARG3, "sdl-create-yuv-overlay");

  width = scm_num2long (s_width, SCM_ARG1, "sdl-create-yuv-overlay");
  height = scm_num2long (s_height, SCM_ARG2, "sdl-create-yuv-overlay");
  format = (Uint32) scm_num2long (s_format, SCM_ARG3, "sdl-create-yuv-overlay");

  if (s_display == SCM_UNDEFINED) {
    display = SDL_GetVideoSurface ();
  } else {
    SCM_ASSERT_SMOB (s_display, surface_tag, SCM_ARG4, "sdl-create-yuv-overlay");
    display = (SDL_Surface*) SCM_SMOB_DATA (s_display);
  }

  overlay = SDL_CreateYUVOverlay (width, height, format, display);

  SCM_RETURN_NEWSMOB (overlay_tag, overlay);
}
#undef FUNC_NAME


SCM_DEFINE( get_video_surface, "sdl-get-video-surface", 0, 0, 0,
            (void),
"Returns the current display surface.")
#define FUNC_NAME s_get_video_surface
{
  SCM_RETURN_NEWSMOB (surface_tag, SDL_GetVideoSurface());
}
#undef FUNC_NAME


SCM_DEFINE( get_video_info, "sdl-get-video-info", 0, 0, 0,
            (void),
"Returns information about the video hardware as an alist.")
#define FUNC_NAME s_get_video_info
{
  const SDL_VideoInfo *info = SDL_GetVideoInfo();
  SCM format;

  SCM_NEWSMOB (format, pixel_format_tag, info->vfmt);

  /* SCM_RETURN_NEWSMOB (video_info_tag, info); */
  return scm_listify (scm_cons (scm_str2symbol ("hw_available"),
                                SCM_BOOL (info->hw_available)),
                      scm_cons (scm_str2symbol ("ww_available"),
                                SCM_BOOL (info->wm_available)),
                      scm_cons (scm_str2symbol ("blit_hw"),
                                SCM_BOOL (info->blit_hw)),
                      scm_cons (scm_str2symbol ("blit_hw_CC"),
                                SCM_BOOL (info->blit_hw_CC)),
                      scm_cons (scm_str2symbol ("blit_hw_A"),
                                SCM_BOOL (info->blit_hw_A)),
                      scm_cons (scm_str2symbol ("blit_sw"),
                                SCM_BOOL (info->blit_sw)),
                      scm_cons (scm_str2symbol ("blit_sw_CC"),
                                SCM_BOOL (info->blit_sw_CC)),
                      scm_cons (scm_str2symbol ("blit_sw_A"),
                                SCM_BOOL (info->blit_sw_A)),
                      scm_cons (scm_str2symbol ("blit_fill"),
                                scm_ulong2num (info->blit_fill)),
                      scm_cons (scm_str2symbol ("video_mem"),
                                scm_ulong2num (info->video_mem)),
                      scm_cons (scm_str2symbol ("vfmt"), format),
                      SCM_UNDEFINED);
}
#undef FUNC_NAME


SCM_DEFINE( video_driver_name, "sdl-video-driver-name", 0, 0, 0,
            (void),
"Returns the name of the video driver.")
#define FUNC_NAME s_video_driver_name
{
  char name[MAX_DRIVER_LEN];
  SDL_VideoDriverName (name, MAX_DRIVER_LEN);
  return scm_makfrom0str (name);
}
#undef FUNC_NAME


SCM_DEFINE( list_modes, "sdl-list-modes", 0, 2, 0,
            (SCM s_pixel_format,
             SCM s_flags),
"Return a list of available screen dimensions for the given format
and flags.  Format defaults to that for the current screen.  Flags
default to none.")
#define FUNC_NAME s_list_modes
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
#undef FUNC_NAME


SCM_DEFINE( video_mode_ok, "sdl-video-mode-ok", 4, 0, 0,
            (SCM s_width,
             SCM s_height,
             SCM s_bpp,
             SCM s_flags),
"Check to see if a particular video mode is supported.")
#define FUNC_NAME s_video_mode_ok
{
  int width, height, bpp;
  Uint32 flags=0;
  int result;

  SCM_ASSERT (scm_exact_p (s_width),  s_width,  SCM_ARG1, "sdl-video-mode-ok");
  SCM_ASSERT (scm_exact_p (s_height), s_height, SCM_ARG2, "sdl-video-mode-ok");
  SCM_ASSERT (scm_exact_p (s_bpp),    s_bpp,    SCM_ARG3, "sdl-video-mode-ok");

  width  = scm_num2long (s_width, SCM_ARG1, "sdl-video-mode-ok");
  height = scm_num2long (s_height, SCM_ARG2, "sdl-video-mode-ok");
  bpp    = scm_num2long (s_bpp, SCM_ARG3, "sdl-video-mode-ok");

  if (s_flags != SCM_UNDEFINED) {
    flags  = (Uint32) scm_flags2ulong (s_flags, sdl_video_flags,
                                       SCM_ARG1, "sdl-video-mode-ok");
  }

  result = SDL_VideoModeOK (width, height, bpp, flags);
  if (result) {
    return scm_long2num (result);
  } else {
    return SCM_BOOL_F;
  }
}
#undef FUNC_NAME


SCM_DEFINE( set_video_mode, "sdl-set-video-mode", 3, 1, 0,
            (SCM s_width,
             SCM s_height,
             SCM s_bpp,
             SCM s_flags),
"Sets the SDL video mode.")
#define FUNC_NAME s_set_video_mode
{
  int width, height, bpp;
  Uint32 flags=0;

  SCM_ASSERT (scm_exact_p (s_width),  s_width,  SCM_ARG1, "sdl-set-video-mode");
  SCM_ASSERT (scm_exact_p (s_height), s_height, SCM_ARG2, "sdl-set-video-mode");
  SCM_ASSERT (scm_exact_p (s_bpp),    s_bpp,    SCM_ARG3, "sdl-set-video-mode");

  width  = scm_num2long (s_width, SCM_ARG1, "sdl-set-video-mode");
  height = scm_num2long (s_height, SCM_ARG2, "sdl-set-video-mode");
  bpp    = scm_num2long (s_bpp, SCM_ARG3, "sdl-set-video-mode");

  if (s_flags != SCM_UNDEFINED) {
    flags = (Uint32) scm_flags2ulong (s_flags, sdl_video_flags,
                                      SCM_ARG4, "sdl-set-video-mode");
  }

  SCM_RETURN_NEWSMOB (surface_tag, SDL_SetVideoMode (width, height, bpp, flags));
}
#undef FUNC_NAME


SCM_DEFINE( update_rect, "sdl-update-rect", 2, 3, 0,
            (SCM s_surface,
             SCM s_x,
             SCM s_y,
             SCM s_w,
             SCM s_h),
"Makes sure a given area on a surface is updated.
Arguments are the surface to be updated, followed by either an
SDL-Rect or the four coordinates x, y, w, and h.")
#define FUNC_NAME s_update_rect
{
  SDL_Surface *surface;
  SDL_Rect *rect;
  Sint32 x, y, w, h;

  /* first arg is a surface */
  SCM_ASSERT_SMOB (s_surface, surface_tag, SCM_ARG1, "sdl-update-rect");
  surface = (SDL_Surface *) SCM_SMOB_DATA (s_surface);

  /* remaining args are a single rect, or 4 coords */
  if (SCM_NIMP (s_x) && (long) SCM_CAR (s_x) == rect_tag) {
    rect = (SDL_Rect*) SCM_SMOB_DATA (s_x);
    x = rect->x;
    y = rect->y;
    w = rect->w;
    h = rect->h;
  } else {
    SCM_ASSERT (scm_exact_p (s_x), s_x, SCM_ARG2, "sdl-update-rect");
    SCM_ASSERT (scm_exact_p (s_y), s_y, SCM_ARG3, "sdl-update-rect");
    SCM_ASSERT (scm_exact_p (s_w), s_w, SCM_ARG4, "sdl-update-rect");
    SCM_ASSERT (scm_exact_p (s_h), s_h, SCM_ARG5, "sdl-update-rect");
    x = (Sint32) scm_num2long (s_x, SCM_ARG1, "sdl-update-rect");
    y = (Sint32) scm_num2long (s_y, SCM_ARG2, "sdl-update-rect");
    w = (Sint32) scm_num2long (s_w, SCM_ARG3, "sdl-update-rect");
    h = (Sint32) scm_num2long (s_h, SCM_ARG4, "sdl-update-rect");
  }

  SDL_UpdateRect (surface, x, y, w, h);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE( update_rects, "sdl-update-rects", 2, 0, 0,
            (SCM surface_smob,
             SCM rect_list),
"Updates the given list of rectangles on the given surface.")
#define FUNC_NAME s_update_rects
{
  SCM rect_smob;
  SDL_Surface *surface;
  SDL_Rect *rect;

  SCM_ASSERT( SCM_NIMP (surface_smob) &&
              (long) SCM_CAR (surface_smob) == surface_tag,
              surface_smob, SCM_ARG1, FUNC_NAME );

  surface = (SDL_Surface*) SCM_SMOB_DATA (surface_smob);

  for( ; ! SCM_NULLP (rect_list); rect_list = SCM_CDR (rect_list) ) {
    rect_smob = SCM_CAR (rect_list);
    SCM_ASSERT( SCM_NIMP (rect_smob) &&
                (long) SCM_CAR (rect_smob) == rect_tag,
                rect_smob, SCM_ARG2, FUNC_NAME );

    rect = (SDL_Rect*) SCM_SMOB_DATA (rect_smob);

    SDL_UpdateRect( surface, rect->x, rect->y, rect->w, rect->h );
  }

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE( sdl_flip, "sdl-flip", 0, 1, 0,
            (SCM s_surface),
"Swaps surface double buffers.")
#define FUNC_NAME s_sdl_flip
{
  SDL_Surface *surface;

  if (s_surface != SCM_UNDEFINED) {
    /* verify and unpack a surface */
    SCM_ASSERT_SMOB (s_surface, surface_tag, SCM_ARG1, "sdl-flip");
    surface = (SDL_Surface *) SCM_SMOB_DATA (s_surface);
  } else {
    /* otherwise default to the current display */
    surface = SDL_GetVideoSurface();
  }

  SDL_Flip (surface);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE( set_colors, "sdl-set-colors!", 2, 0, 0,
            (SCM s_surface,
             SCM s_colors),
"Sets a portion of the colormap for the given 8-bit surface.")
#define FUNC_NAME s_set_colors
{
  SDL_Surface *surface;
  SDL_Color *colors;
  SDL_Color *color;
  int i, length, result;

  SCM_ASSERT_SMOB (s_surface, surface_tag, SCM_ARG1, "sdl-set-colors!");
  SCM_ASSERT (SCM_VECTORP (s_colors), s_colors, SCM_ARG2, "sdl-set-colors!");

  surface = (SDL_Surface*) SCM_SMOB_DATA (s_surface);
  length = SCM_VECTOR_LENGTH (s_colors);
  colors = (SDL_Color*) scm_gc_malloc (length, "sdl-set-colors!");

  for (i=0; i<length; i++) {
    color = (SDL_Color*) SCM_SMOB_DATA
      (scm_vector_ref (s_colors, scm_long2num (i)));
    colors[i] = *color;
  }

  result = SDL_SetColors (surface, colors, 0, length);
  scm_must_free (colors);

  return result ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE( set_palette, "sdl-set-palette", 3, 0, 0,
            (SCM s_surface,
             SCM s_flags,
             SCM s_colors),
"Sets the colors in the palette of an 8-bit surface.")
#define FUNC_NAME s_set_palette
{
  SDL_Surface *surface;
  SDL_Color *colors;
  SDL_Color *color;
  int flags, i, length, result;

  SCM_ASSERT_SMOB (s_surface, surface_tag, SCM_ARG1, "sdl-set-palette!");
  SCM_ASSERT (SCM_VECTORP (s_colors), s_colors, SCM_ARG3, "sdl-set-palette!");

  surface = (SDL_Surface*) SCM_SMOB_DATA (s_surface);
  flags   = scm_flags2ulong (s_flags, sdl_palette_flags,
                             SCM_ARG2, "sdl-set-palette!");
  length  = SCM_VECTOR_LENGTH (s_colors);
  colors  = (SDL_Color*) scm_gc_malloc (length, "sdl-set-palette!");

  for (i=0; i<length; i++) {
    color = (SDL_Color*) SCM_SMOB_DATA
      (scm_vector_ref (s_colors, scm_long2num (i)));
    colors[i] = *color;
  }

  result = SDL_SetPalette (surface, flags, colors, 0, length);
  scm_must_free (colors);

  return result ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE( set_gamma, "sdl-set-gamma", 3, 0, 0,
            (SCM s_redgamma,
             SCM s_greengamma,
             SCM s_bluegamma),
"Sets the color gamma function for the display.")
#define FUNC_NAME s_set_gamma
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
#undef FUNC_NAME


SCM_DEFINE( get_gamma_ramp, "sdl-get-gamma-ramp", 0, 0, 0,
            (void),
"Gets the gamma translation lookup tables currently used by the display.
Each table is an vector of 256 integer values.")
#define FUNC_NAME s_get_gamma_ramp
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
    return scm_list_3 (scm_cons (scm_str2symbol ("redtable"), redtable),
                       scm_cons (scm_str2symbol ("greentable"), greentable),
                       scm_cons (scm_str2symbol ("bluetable"), bluetable));
  } else {
    /* error, return false */
    return SCM_BOOL_F;
  }
}
#undef FUNC_NAME


SCM_DEFINE( set_gamma_ramp, "sdl-set-gamma-ramp", 3, 0, 0,
            (SCM s_redtable,
             SCM s_greentable,
             SCM s_bluetable),
"Sets the gamma translation lookup tables currently used by the display.
Each table is an vector of 256 integer values.")
#define FUNC_NAME s_get_gamma_ramp
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
                          SCM_ARG1, "sdl-set-gamma-ramp");
    gt[i] = scm_num2long (scm_vector_ref (s_greentable, scm_long2num (i)),
                          SCM_ARG2, "sdl-set-gamma-ramp");
    bt[i] = scm_num2long (scm_vector_ref (s_bluetable,  scm_long2num (i)),
                          SCM_ARG3, "sdl-set-gamma-ramp");
  }

  SCM_RETURN_TRUE_IF_0 (SDL_SetGammaRamp (rt, gt, bt));
}
#undef FUNC_NAME


SCM_DEFINE( map_rgb, "sdl-map-rgb", 2, 2, 0,
            (SCM s_pixel_format,
             SCM s_r,
             SCM s_g,
             SCM s_b),
"Map a RGB color value to a pixel format.
Takes a pixel format followed by either an SDL-Color or the three
r, g, b values.")
#define FUNC_NAME s_map_rgb
{
  SDL_PixelFormat *fmt;
  SDL_Color *color;
  Uint8 r, g, b;

  SCM_ASSERT_SMOB (s_pixel_format, pixel_format_tag, SCM_ARG1, "sdl-map-rbg");
  fmt = (SDL_PixelFormat*) SCM_SMOB_DATA (s_pixel_format);

  if (SCM_NIMP (s_r) && (long) SCM_CAR (s_r) == color_tag) {
    color = (SDL_Color*) SCM_SMOB_DATA (s_r);
    r = color->r;
    g = color->g;
    b = color->b;
  } else {
    SCM_ASSERT (scm_exact_p (s_r), s_r, SCM_ARG2, "sdl-map-rgb");
    SCM_ASSERT (scm_exact_p (s_g), s_g, SCM_ARG3, "sdl-map-rgb");
    SCM_ASSERT (scm_exact_p (s_b), s_b, SCM_ARG4, "sdl-map-rgb");
    r = (Uint8) scm_num2long (s_r, SCM_ARG1, "sdl-map-rgb");
    g = (Uint8) scm_num2long (s_g, SCM_ARG2, "sdl-map-rgb");
    b = (Uint8) scm_num2long (s_b, SCM_ARG3, "sdl-map-rgb");
  }

  return scm_long2num (SDL_MapRGB (fmt, r, g, b));
}
#undef FUNC_NAME


SCM_DEFINE( map_rgba, "sdl-map-rgba", 3, 2, 0,
            (SCM s_pixel_format,
             SCM s_r,
             SCM s_g,
             SCM s_b,
             SCM s_a),
"Map a RGB color value to a pixel format.
Takes a pixel format followed by either an SDL-Color or the three r,
g, b values, followed by the alpha value.")
#define FUNC_NAME s_map_rgba
{
  SDL_PixelFormat *fmt;
  SDL_Color *color;
  Uint8 r, g, b, a;

  SCM_ASSERT_SMOB (s_pixel_format, pixel_format_tag, SCM_ARG1, "sdl-map-rbga");
  fmt = (SDL_PixelFormat*) SCM_SMOB_DATA (s_pixel_format);

  if (SCM_NIMP (s_r) && (long) SCM_CAR (s_r) == color_tag) {
    color = (SDL_Color*) SCM_SMOB_DATA (s_r);
    r = color->r;
    g = color->g;
    b = color->b;
    SCM_ASSERT (scm_exact_p (s_g), s_g, SCM_ARG3, "sdl-map-rgba");
    a = (Uint8) scm_num2long (s_g, SCM_ARG3, "sdl-map-rgba");
  } else {
    SCM_ASSERT (scm_exact_p (s_r), s_r, SCM_ARG2, "sdl-map-rgba");
    SCM_ASSERT (scm_exact_p (s_g), s_g, SCM_ARG3, "sdl-map-rgba");
    SCM_ASSERT (scm_exact_p (s_b), s_b, SCM_ARG4, "sdl-map-rgba");
    SCM_ASSERT (scm_exact_p (s_a), s_a, SCM_ARG5, "sdl-map-rgba");
    r = (Uint8) scm_num2long (s_r, SCM_ARG2, "sdl-map-rgba");
    g = (Uint8) scm_num2long (s_g, SCM_ARG3, "sdl-map-rgba");
    b = (Uint8) scm_num2long (s_b, SCM_ARG4, "sdl-map-rgba");
    a = (Uint8) scm_num2long (s_a, SCM_ARG5, "sdl-map-rgba");
  }

  return scm_long2num (SDL_MapRGBA (fmt, r, g, b, a));
}
#undef FUNC_NAME


SCM_DEFINE( get_rgb, "sdl-get-rgb", 2, 0, 0,
            (SCM s_pixel,
             SCM s_pixel_format),
"Get RGB values from a pixel in the specified pixel format.
Returns an alist with r, g and b entries.")
#define FUNC_NAME s_get_rgb
{
  SDL_PixelFormat *fmt;
  Uint32 pixel;
  Uint8 r, g, b;

  SCM_ASSERT (scm_exact_p (s_pixel), s_pixel, SCM_ARG1, "sdl-get-rgb");
  SCM_ASSERT_SMOB (s_pixel_format, pixel_format_tag, SCM_ARG2, "sdl-get-rbg");

  fmt = (SDL_PixelFormat*) SCM_SMOB_DATA (s_pixel_format);
  pixel = (Uint32) scm_num2long (s_pixel, SCM_ARG1, "sdl-get-rgb");

  SDL_GetRGB (pixel, fmt, &r, &g, &b);

  return scm_list_3 (scm_cons (scm_str2symbol ("r"), scm_long2num (r)),
                     scm_cons (scm_str2symbol ("g"), scm_long2num (g)),
                     scm_cons (scm_str2symbol ("b"), scm_long2num (b)));
}
#undef FUNC_NAME


SCM_DEFINE( get_rgba, "sdl-get-rgba", 2, 0, 0,
            (SCM s_pixel,
             SCM s_pixel_format),
"Get RGBA values from a pixel in the specified pixel format.
Returns an alist with r, g, b and a entries.")
#define FUNC_NAME s_get_rgba
{
  SDL_PixelFormat *fmt;
  Uint32 pixel;
  Uint8 r, g, b, a;

  SCM_ASSERT (scm_exact_p (s_pixel), s_pixel, SCM_ARG1, "sdl-get-rgba");
  SCM_ASSERT_SMOB (s_pixel_format, pixel_format_tag, SCM_ARG2, "sdl-get-rbga");

  fmt = (SDL_PixelFormat*) SCM_SMOB_DATA (s_pixel_format);
  pixel = (Uint32) scm_num2long (s_pixel, SCM_ARG1, "sdl-get-rgba");

  SDL_GetRGBA (pixel, fmt, &r, &g, &b, &a);

  return scm_list_4 (scm_cons (scm_str2symbol ("r"), scm_long2num (r)),
                     scm_cons (scm_str2symbol ("g"), scm_long2num (g)),
                     scm_cons (scm_str2symbol ("b"), scm_long2num (b)),
                     scm_cons (scm_str2symbol ("a"), scm_long2num (a)));
}
#undef FUNC_NAME


SCM_DEFINE( fill_rect, "sdl-fill-rect", 3, 0, 0,
            (SCM s_dst,
             SCM s_dstrect,
             SCM s_color),
"Fill a given rectangle with a color.")
#define FUNC_NAME s_fill_rect
{
  SDL_Surface *dst;
  SDL_Rect *dstrect;
  Uint32 color;

  SCM_ASSERT_SMOB (s_dst, surface_tag,  SCM_ARG1, "sdl-fill-rect");
  SCM_ASSERT_SMOB (s_dstrect, rect_tag, SCM_ARG2, "sdl-fill-rect");
  SCM_ASSERT (scm_exact_p (s_color), s_color, SCM_ARG3, "sdl-fill-rect");

  dst = (SDL_Surface *) SCM_SMOB_DATA (s_dst);
  dstrect = (SDL_Rect *) SCM_SMOB_DATA (s_dstrect);
  color = (Uint32) scm_num2long (s_color, SCM_ARG3, "sdl-fill-rect");

  return scm_long2num (SDL_FillRect (dst, dstrect, color));
}
#undef FUNC_NAME


SCM_DEFINE( display_format, "sdl-display-format", 1, 0, 0,
            (SCM s_surface),
"Convert a surface to the display format.")
#define FUNC_NAME s_display_format
{
  SDL_Surface *surface;

  SCM_ASSERT_SMOB (s_surface, surface_tag,  SCM_ARG1, "sdl-display-format");

  surface = SDL_DisplayFormat ((SDL_Surface*) SCM_SMOB_DATA (s_surface));

  if (! surface) {
    return SCM_BOOL_F;
  }

  SCM_RETURN_NEWSMOB (surface_tag, surface);
}
#undef FUNC_NAME


SCM_DEFINE( display_format_alpha, "sdl-display-format-alpha", 1, 0, 0,
            (SCM s_surface),
"Convert a surface to the display format, with an alpha channel.")
#define FUNC_NAME s_display_format_alpha
{
  SDL_Surface *surface;

  SCM_ASSERT_SMOB (s_surface, surface_tag,  SCM_ARG1, "sdl-display-format-alpha");

  surface = SDL_DisplayFormatAlpha ((SDL_Surface*) SCM_SMOB_DATA (s_surface));

  if (! surface) {
    return SCM_BOOL_F;
  }

  SCM_RETURN_NEWSMOB (surface_tag, surface);
}
#undef FUNC_NAME


SCM_DEFINE( warp_mouse, "sdl-warp-mouse", 2, 0, 0,
            (SCM s_x,
             SCM s_y),
"Set the position of the mouse cursor.")
#define FUNC_NAME s_warp_mouse
{
  Uint16 x, y;

  SCM_ASSERT (scm_exact_p (s_x), s_x, SCM_ARG1, "sdl-warp-mouse");
  SCM_ASSERT (scm_exact_p (s_y), s_y, SCM_ARG2, "sdl-warp-mouse");

  x = (Uint16) scm_num2long (s_x, SCM_ARG1, "sdl-warp-mouse");
  y = (Uint16) scm_num2long (s_y, SCM_ARG2, "sdl-warp-mouse");

  SDL_WarpMouse (x, y);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE( set_cursor, "sdl-set-cursor", 1, 0, 0,
            (SCM s_cursor),
"Set the current mouse cursor.")
#define FUNC_NAME s_set_cursor
{
  SCM_ASSERT_SMOB (s_cursor, cursor_tag, SCM_ARG1, "sdl-set-cursor");
  SDL_SetCursor ((SDL_Cursor*) SCM_SMOB_DATA (s_cursor));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE( get_cursor, "sdl-get-cursor", 0, 0, 0,
            (void),
"Get the current mouse cursor.")
#define FUNC_NAME s_get_cursor
{
  SCM_RETURN_NEWSMOB (cursor_tag, SDL_GetCursor());
}
#undef FUNC_NAME


SCM_DEFINE( show_cursor, "sdl-show-cursor", 1, 0, 0,
            (SCM s_toggle),
"Toggle the visibility of the mouse cursor.")
#define FUNC_NAME s_show_cursor
{
  SCM_ASSERT (scm_exact_p (s_toggle), s_toggle, SCM_ARG1, "sdl-show-cursor");
  return scm_long2num (SDL_ShowCursor (scm_num2long (s_toggle, SCM_ARG1, "sdl-show-cursor")));
}
#undef FUNC_NAME


SCM_DEFINE( gl_get_attribute, "sdl-gl-get-attribute", 1, 0, 0,
            (SCM s_attr),
"Get the value of a special SDL/OpenGL attribute.")
#define FUNC_NAME s_gl_get_attribute
{
  SDL_GLattr attr;
  int value;

  SCM_ASSERT (scm_exact_p (s_attr), s_attr, SCM_ARG1, "sdl-gl-get-attribute");
  attr = (SDL_GLattr) scm_num2long (s_attr, SCM_ARG1, "sdl-gl-get-attribute");

  SDL_GL_GetAttribute(attr, &value);

  return scm_long2num (value);
}
#undef FUNC_NAME


SCM_DEFINE( gl_set_attribute, "sdl-gl-set-attribute", 2, 0, 0,
            (SCM s_attr,
             SCM s_value),
"Set the value of a special SDL/OpenGL attribute.")
#define FUNC_NAME s_gl_set_attribute
{
  SDL_GLattr attr;
  int value;

  SCM_ASSERT (scm_exact_p (s_attr), s_attr, SCM_ARG1, "sdl-gl-set-attribute");
  SCM_ASSERT (scm_exact_p (s_value), s_value, SCM_ARG2, "sdl-gl-set-attribute");

  attr = (SDL_GLattr) scm_num2long (s_attr, SCM_ARG1, "sdl-gl-set-attribute");
  value = (int) scm_num2long (s_value, SCM_ARG2, "sdl-gl-set-attribute");

  SDL_GL_SetAttribute(attr, value);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE( gl_swap_buffers, "sdl-gl-swap-buffers", 0, 0, 0,
            (void),
"Swap OpenGL framebuffers/Update Display.")
#define FUNC_NAME s_gl_swap_buffers
{
  SDL_GL_SwapBuffers ();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE( lock_yuv_overlay, "sdl-lock-yuv-overlay", 1, 0, 0,
            (SCM s_overlay),
"Lock the given YUV overlay.")
#define FUNC_NAME s_lock_yuv_overlay
{
  SDL_Overlay *overlay;

  SCM_ASSERT_SMOB (s_overlay, overlay_tag, SCM_ARG1, "sdl-lock-yuv-overlay");
  overlay = (SDL_Overlay*) SCM_SMOB_DATA (s_overlay);

  SCM_RETURN_TRUE_IF_0 (SDL_LockYUVOverlay (overlay));
}
#undef FUNC_NAME


SCM_DEFINE( unlock_yuv_overlay, "sdl-unlock-yuv-overlay", 1, 0, 0,
            (SCM s_overlay),
"Unlock a previously locked YUV overlay.")
#define FUNC_NAME s_unlock_yuv_overlay
{
  SDL_Overlay *overlay;

  SCM_ASSERT_SMOB (s_overlay, overlay_tag, SCM_ARG1, "sdl-unlock-yuv-overlay");
  overlay = (SDL_Overlay*) SCM_SMOB_DATA (s_overlay);

  SDL_UnlockYUVOverlay (overlay);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE( display_yuv_overlay, "sdl-display-yuv-overlay", 2, 0, 0,
            (SCM s_overlay,
             SCM s_dstrect),
"Blit a YUV overlay to the display over which it was created.")
#define FUNC_NAME s_display_yuv_overlay
{
  SDL_Overlay *overlay;
  SDL_Rect *rect;

  SCM_ASSERT_SMOB (s_overlay, overlay_tag, SCM_ARG1, "sdl-display-yuv-overlay");
  SCM_ASSERT_SMOB (s_dstrect, rect_tag, SCM_ARG2, "sdl-display-yuv-overlay");

  overlay = (SDL_Overlay*) SCM_SMOB_DATA (s_overlay);
  rect = (SDL_Rect*) SCM_SMOB_DATA (s_dstrect);

  SCM_RETURN_TRUE_IF_0 (SDL_DisplayYUVOverlay (overlay, rect));
}
#undef FUNC_NAME


/* window manager functions */

SCM_DEFINE( wm_set_caption, "sdl-set-caption", 2, 0, 0,
            (SCM s_title,
             SCM s_icon),
"Sets the title-bar and icon name of the display window.")
#define FUNC_NAME s_wm_set_caption
{
  char *title, *icon;

  SCM_ASSERT ((SCM_NIMP (s_title) && SCM_STRINGP (s_title)),
              s_title, SCM_ARG1, "sdl-set-caption");

  title = SCM_STRING_CHARS (s_title);

  if (s_icon == SCM_UNDEFINED) {
    icon = title;
  } else {
    SCM_ASSERT ((SCM_NIMP (s_icon) && SCM_STRINGP (s_icon)),
                s_icon, SCM_ARG2, "sdl-set-caption");
    icon = SCM_STRING_CHARS (s_icon);
  }

  SDL_WM_SetCaption (title, icon);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE( wm_get_caption, "sdl-get-caption", 0, 0, 0,
            (void),
"Gets the title-bar and icon name of the display window.")
#define FUNC_NAME s_wm_get_caption
{
  char *title;
  char *icon;

  SDL_WM_GetCaption (&title, &icon);

  return scm_list_2 (scm_cons (scm_str2symbol ("title"), scm_makfrom0str (title)),
                     scm_cons (scm_str2symbol ("icon"), scm_makfrom0str (icon)));
}
#undef FUNC_NAME


SCM_DEFINE( wm_set_icon, "sdl-set-icon", 1, 0, 0,
            (SCM icon),
"Sets the icon for the display window.")
#define FUNC_NAME s_wm_set_icon
{
  SDL_Surface *surface;

  SCM_ASSERT_SMOB (icon, surface_tag, SCM_ARG1, "sdl-set-icon");
  surface = (SDL_Surface*) SCM_SMOB_DATA (icon);

  /* set w/ a NULL mask for now */
  SDL_WM_SetIcon (surface, NULL);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE( wm_iconify_window, "sdl-iconify-window", 0, 0, 0,
            (void),
"Iconify/Minimise the window.")
#define FUNC_NAME s_wm_iconify_window
{
  return SDL_WM_IconifyWindow () ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE( wm_toggle_full_screen, "sdl-toggle-full-screen", 0, 1, 0,
            (SCM s_surface),
"Toggles the application between windowed and fullscreen mode, if supported.")
#define FUNC_NAME s_wm_toggle_full_screen
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
#undef FUNC_NAME


SCM_DEFINE( wm_grab_input, "sdl-grab-input", 0, 1, 0,
            (SCM s_mode),
"Grabs mouse and keyboard input.")
#define FUNC_NAME s_wm_grab_input
{
  int mode = SDL_GRAB_QUERY;

  if (s_mode != SCM_UNDEFINED) {
    SCM_ASSERT (scm_exact_p (s_mode), s_mode, SCM_ARG1, "sdl-grab-input");
    mode = scm_num2long (s_mode, SCM_ARG1, "sdl-grab-input");
  }

  return scm_long2num (SDL_WM_GrabInput (mode));
}
#undef FUNC_NAME


void
sdl_init_video (void)
{
  /* smobs */
  cursor_tag    = scm_make_smob_type ("SDL-Cursor", sizeof (SDL_Cursor));
  pixel_format_tag = scm_make_smob_type ("SDL-Pixel-Format",
                                         sizeof (SDL_PixelFormat));
  overlay_tag   = scm_make_smob_type ("SDL-Overlay", sizeof (SDL_Overlay));

  scm_set_smob_free (cursor_tag, free_cursor);
  scm_set_smob_free (overlay_tag, free_yuv_overlay);
  scm_set_smob_free (pixel_format_tag, free_pixel_format);

  /* alpha constants */
  sdl_alpha_enums = scm_c_define_enum (
    "sdl-alpha-enums",
    "SDL_ALPHA_OPAQUE",      SDL_ALPHA_OPAQUE,
    "SDL_ALPHA_TRANSPARENT", SDL_ALPHA_TRANSPARENT,
    NULL);

  /* video flags */
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

  /* palette flags */
  sdl_palette_flags = scm_c_define_flag (
    "sdl-palette-flags",
    "SDL_LOGPAL",  SDL_LOGPAL,
    "SDL_PHYSPAL", SDL_PHYSPAL,
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

  /* exported symbols */
  scm_c_export (
    /* flags & enums */
    "sdl-video-flags",        "sdl-alpha-enums",
    "sdl-gl-enums",           "sdl-palette-flags",
    /* yuv constants */
    "sdl-yv12-overlay",       "sdl-iyuv-overlay",       "sdl-yuy2-overlay",
    "sdl-uyvy-overlay",       "sdl-yvyu-overlay",
    /* pixel formats */
    "sdl-map-rgb",  "sdl-map-rgba",  "sdl-get-rgb",  "sdl-get-rgba",
    /* overlays */
    "sdl-create-yuv-overlay",   "sdl-lock-yuv-overlay",
    "sdl-unlock-yuv-overlay",   "sdl-display-yuv-overlay",
    /* video */
    "sdl-set-video-mode",     "sdl-update-rect",
    "sdl-update-rects",
    "sdl-flip",               "sdl-fill-rect",
    "sdl-get-video-surface",  "sdl-set-colors!",
    "sdl-set-palette!",       "sdl-set-gamma",
    "sdl-get-gamma-ramp",     "sdl-set-gamma-ramp",
    "sdl-display-format",     "sdl-display-format-alpha",
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
    "sdl-video-driver-name",  "sdl-get-video-info",
    NULL);

#ifndef SCM_MAGIC_SNARFER
#include "sdlvideo.x"
#endif

}

