/* sdlvideo.c --- SDL Video functions for Guile
 *
 * 	Copyright (C) 2003 Thien-Thi Nguyen
 * 	Copyright (C) 2001 Alex Shinn
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program; if not, write to the Free
 * Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 * MA 02111-1307 USA
 */

#include <guile/gh.h>
#include <SDL/SDL.h>
#include <SDL/SDL_image.h>

#include "config.h"
#include "argcheck.h"
#include "sdlenums.h"
#include "sdlsmobs.h"


#define MAX_DRIVER_LEN    100
#define GAMMA_TABLE_SIZE  256


static SCM sdl_gl_enums;
SCM gsdl_alpha_enums;

SCM gsdl_video_flags;
SCM gsdl_palette_flags;
static SCM gsdl_overlay_formats;

MDEFLOCEXP (sdl_get_video_flags, "flagstash:video", 0, 0, 0, (),
            "Return the flagstash object for video flags.")
{
  return gsdl_video_flags;
}

MDEFLOCEXP (sdl_get_palette_flags, "flagstash:palette", 0, 0, 0, (),
            "Return the flagstash object for palette flags.")
{
  return gsdl_palette_flags;
}

MDEFLOCEXP (sdl_get_overlay_formats, "flagstash:overlay", 0, 0, 0, (),
            "Return the flagstash object for overlay flags.\n"
            "(Actually, these are \"common overlay formats\", not flags.)")
{
  return gsdl_overlay_formats;
}



/* tags for SDL smobs */
static long cursor_tag;
static long overlay_tag;

#define ASSERT_CURSOR(obj,which)   ASSERT_SMOB (obj, cursor_tag, which)
#define ASSERT_OVERLAY(obj,which)  ASSERT_SMOB (obj, overlay_tag, which)

#define UNPACK_CURSOR(smob)       (SMOBGET (smob, SDL_Cursor *))
#define UNPACK_OVERLAY(smob)      (SMOBGET (smob, SDL_Overlay *))

#define RETURN_NEW_CURSOR(x)    SCM_RETURN_NEWSMOB (cursor_tag, x)
#define RETURN_NEW_OVERLAY(x)   SCM_RETURN_NEWSMOB (overlay_tag, x)

/* smob functions */

static
SCM
mark_cursor (SCM s_cursor)
{
  return s_cursor;
}

static
size_t
free_cursor (SCM s_cursor)
{
  SDL_FreeCursor (UNPACK_CURSOR (s_cursor));
  /* return sizeof (SDL_Cursor); */
  return 0;
}

static
SCM
mark_yuv_overlay (SCM s_overlay)
{
  return s_overlay;
}

static
size_t
free_yuv_overlay (SCM s_overlay)
{
  SDL_FreeYUVOverlay (UNPACK_OVERLAY (s_overlay));
  /* return sizeof (SDL_Overlay); */
  return 0;
}

static
SCM
mark_pixel_format (SCM s_pixel_format)
{
  return s_pixel_format;
}

static
size_t
free_pixel_format (SCM s_pixel_format)
{
  /* always part of a surface, no need to free */
  return 0;
}


/* scheme callable functions */

MDEFLOCEXP (create_cursor, "sdl-create-cursor", 6, 0, 0,
            (SCM s_data, SCM s_mask,
             SCM s_w, SCM s_h,
             SCM s_hot_x, SCM s_hot_y),
            "Return a new cursor from @var{data} and @var{mask}\n"
            "(vectors), sized @var{width} by @var{height}\n"
            "and with hot pixel located at @var{hotx},@var{hoty}.")
#define FUNC_NAME s_create_cursor
{
  SDL_Cursor *cursor;
  Uint8 *data, *mask;

  ASSERT_VECTOR (s_data, ARGH1);
  ASSERT_VECTOR (s_mask, ARGH2);
  ASSERT_EXACT (s_w, ARGH3);
  ASSERT_EXACT (s_h, ARGH4);
  ASSERT_EXACT (s_hot_x, ARGH5);
  ASSERT_EXACT (s_hot_y, ARGH6);

  /* build the arrays */
  data = (Uint8 *) gh_scm2chars (s_data, NULL);
  mask = (Uint8 *) gh_scm2chars (s_mask, NULL);

  /* create the cursor */
  cursor = SDL_CreateCursor (data, mask,
                             gh_scm2long (s_w),
                             gh_scm2long (s_h),
                             gh_scm2long (s_hot_x),
                             gh_scm2long (s_hot_y));

  /* free the arrays */
  /*scm_must_*/free (data);
  /*scm_must_*/free (mask);

  /* return the new smob */
  RETURN_NEW_CURSOR (cursor);
}
#undef FUNC_NAME


#define GSDL_FLAG2ULONG(flag,table) \
  gsdl_flags2ulong ((flag), (table), 0, NULL) /* DWR! */

MDEFLOCEXP (create_yuv_overlay, "sdl-create-yuv-overlay", 3, 1, 0,
            (SCM s_width, SCM s_height, SCM s_format, SCM s_display),
            "Create a new YUV overlay, sized @var{width} by @var{height}\n"
            "with format @var{f} (a symbol or an exact number).  Optional\n"
            "arg @var{display} specifies a surface to use instead of\n"
            "creating a new one.")
#define FUNC_NAME s_create_yuv_overlay
{
  Uint32 format;
  SDL_Surface *display;

  ASSERT_EXACT (s_width, ARGH1);
  ASSERT_EXACT (s_height, ARGH2);

  if (gh_symbol_p (s_format)) {
    format = GSDL_FLAG2ULONG (s_format, gsdl_overlay_formats);
  } else {
    ASSERT_EXACT (s_format, ARGH3);
    format = gh_scm2ulong (s_format);
  }

  if (UNBOUNDP (s_display)) {
    display = SDL_GetVideoSurface ();
  } else {
    ASSERT_SURFACE (s_display, ARGH4);
    display = UNPACK_SURFACE (s_display);
  }

  RETURN_NEW_OVERLAY
    (SDL_CreateYUVOverlay (gh_scm2long (s_width),
                           gh_scm2long (s_height),
                           format,
                           display));
}
#undef FUNC_NAME


MDEFLOCEXP (get_video_surface, "sdl-get-video-surface", 0, 0, 0,
            (void),
            "Return the current display surface.")
#define FUNC_NAME s_get_video_surface
{
  RETURN_NEW_SURFACE (SDL_GetVideoSurface ());
}
#undef FUNC_NAME


SCM_SYMBOL (gsdl_sym_hw_available, "hw-available");
SCM_SYMBOL (gsdl_sym_ww_available, "ww-available");
SCM_SYMBOL (gsdl_sym_blit_hw,      "blit-hw");
SCM_SYMBOL (gsdl_sym_blit_hw_CC,   "blit-hw-CC");
SCM_SYMBOL (gsdl_sym_blit_hw_A,    "blit-hw-A");
SCM_SYMBOL (gsdl_sym_blit_sw,      "blit-sw");
SCM_SYMBOL (gsdl_sym_blit_sw_CC,   "blit-sw-CC");
SCM_SYMBOL (gsdl_sym_blit_sw_A,    "blit-sw-A");
SCM_SYMBOL (gsdl_sym_blit_fill,    "blit-fill");
SCM_SYMBOL (gsdl_sym_video_mem,    "video-mem");
SCM_SYMBOL (gsdl_sym_vfmt,         "vfmt");

MDEFLOCEXP (get_video_info, "sdl-get-video-info", 0, 0, 0,
            (void),
            "Return information about the video hardware as an alist.\n"
            "Keys are: @code{hw-available}, @code{ww-available},\n"
            "@code{bit-hw}, @code{blit-hw-CC}, @code{blit-hw-A},\n"
            "@code{blit-sw}, @code{blit-sw-CC}, @code{blit-sw-A},\n"
            "@code{blit-fill}, @code{video-mem} and @code{vfmt}.")
#define FUNC_NAME s_get_video_info
{
  const SDL_VideoInfo *info = SDL_GetVideoInfo ();
  SCM format;

  SCM_NEWSMOB (format, pixel_format_tag, info->vfmt);

  return gh_list
    (gh_cons (gsdl_sym_hw_available, SCM_BOOL (info->hw_available)),
     gh_cons (gsdl_sym_ww_available, SCM_BOOL (info->wm_available)),
     gh_cons (gsdl_sym_blit_hw,      SCM_BOOL (info->blit_hw)),
     gh_cons (gsdl_sym_blit_hw_CC,   SCM_BOOL (info->blit_hw_CC)),
     gh_cons (gsdl_sym_blit_hw_A,    SCM_BOOL (info->blit_hw_A)),
     gh_cons (gsdl_sym_blit_sw,      SCM_BOOL (info->blit_sw)),
     gh_cons (gsdl_sym_blit_sw_CC,   SCM_BOOL (info->blit_sw_CC)),
     gh_cons (gsdl_sym_blit_sw_A,    SCM_BOOL (info->blit_sw_A)),
     gh_cons (gsdl_sym_blit_fill,    gh_ulong2scm (info->blit_fill)),
     gh_cons (gsdl_sym_video_mem,    gh_ulong2scm (info->video_mem)),
     gh_cons (gsdl_sym_vfmt,         format),
     SCM_UNDEFINED);
}
#undef FUNC_NAME


MDEFLOCEXP (video_driver_name, "sdl-video-driver-name", 0, 0, 0,
            (void),
            "Return the name of the video driver.")
#define FUNC_NAME s_video_driver_name
{
  char name[MAX_DRIVER_LEN];
  SDL_VideoDriverName (name, MAX_DRIVER_LEN);
  return gh_str02scm (name);
}
#undef FUNC_NAME


MDEFLOCEXP (list_modes, "sdl-list-modes", 0, 2, 0,
            (SCM s_pixel_format, SCM s_flags),
            "Return a list of available screen dimensions for pixel\n"
            "@var{format} and @var{flags}.  Format defaults to that for\n"
            "the current screen.  Flags default to none\n"
            "(see @code{flagstash:video}).\n"
            "Return #f if no modes are available, #t if all are available.")
#define FUNC_NAME s_list_modes
{
  SDL_PixelFormat *format = NULL;
  Uint32 flags = 0;
  SDL_Rect **modes;
  SCM result;

  if (BOUNDP (SCM_UNDEFINED)) {
    ASSERT_PIXEL_FORMAT (s_pixel_format, ARGH1);
    format = UNPACK_PIXEL_FORMAT (s_pixel_format);
  }

  if (BOUNDP (s_flags)) {
    ASSERT_EXACT (s_flags, ARGH2);
    flags = (Uint32) GSDL_FLAGS2ULONG (s_flags, gsdl_video_flags, ARGH2);
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
    int i;

    /* otherwise return a list of the available resolutions */
    result = SCM_EOL;
    for (i = 0; modes[i]; i++) {
      SCM rect;

      SCM_NEWSMOB (rect, rect_tag, modes[i]);
      result = gh_cons (rect, result);
    }
  }
  return result;
}
#undef FUNC_NAME


MDEFLOCEXP (video_mode_ok, "sdl-video-mode-ok", 4, 0, 0,
            (SCM s_width, SCM s_height, SCM s_bpp, SCM s_flags),
            "Check to see if a particular video mode is supported.\n"
            "Args are @var{width}, @var{height}, @var{bpp} (numbers),\n"
            "and @var{flags} (see @code{flagstash:video}).\n"
            "Return #f if the mode is not supported, or a number\n"
            "indicating the bits-per-pixel of the closest available\n"
            "mode supporting @var{width} and @var{height}.")
#define FUNC_NAME s_video_mode_ok
{
  Uint32 flags = 0;
  int result;

  ASSERT_EXACT (s_width,  ARGH1);
  ASSERT_EXACT (s_height, ARGH2);
  ASSERT_EXACT (s_bpp,    ARGH3);

  if (BOUNDP (s_flags)) {
    flags = (Uint32) GSDL_FLAGS2ULONG (s_flags, gsdl_video_flags, ARGH4);
  }

  result = SDL_VideoModeOK (gh_scm2long (s_width),
                            gh_scm2long (s_height),
                            gh_scm2long (s_bpp),
                            flags);
  return result ? gh_long2scm (result) : SCM_BOOL_F;
}
#undef FUNC_NAME


MDEFLOCEXP (set_video_mode, "sdl-set-video-mode", 3, 1, 0,
            (SCM s_width, SCM s_height, SCM s_bpp, SCM s_flags),
            "Set the SDL video mode with width @var{w}, height\n"
            "@var{h} and bits-per-pixel @var{bpp}.  Optional arg\n"
            "@var{flags} (see @code{flagstash:video}) is supported.\n"
            "Return a new surface.")
#define FUNC_NAME s_set_video_mode
{
  Uint32 flags = 0;

  ASSERT_EXACT (s_width,  ARGH1);
  ASSERT_EXACT (s_height, ARGH2);
  ASSERT_EXACT (s_bpp,    ARGH3);

  if (BOUNDP (s_flags)) {
    flags = (Uint32) GSDL_FLAGS2ULONG (s_flags, gsdl_video_flags, ARGH4);
  }

  RETURN_NEW_SURFACE
    (SDL_SetVideoMode (gh_scm2long (s_width),
                       gh_scm2long (s_height),
                       gh_scm2long (s_bpp),
                       flags));
}
#undef FUNC_NAME


MDEFLOCEXP (update_rect, "sdl-update-rect", 2, 3, 0,
            (SCM s_surface, SCM s_x, SCM s_y, SCM s_w, SCM s_h),
            "Update @var{surface} within a specified rectangle.\n"
            "The second arg can either be an SDL-Rect object, or\n"
            "the second through fifth args are numbers specifying\n"
            "the x, y, width and height of a rectangular area.\n"
            "The return value is unspecified.")
#define FUNC_NAME s_update_rect
{
  SDL_Rect *rect;
  Sint32 x, y, w, h;

  /* first arg is a surface */
  ASSERT_SURFACE (s_surface, ARGH1);

  /* remaining args are a single rect, or 4 coords */
  if (SCM_SMOB_PREDICATE (rect_tag, s_x)) {
    rect = UNPACK_RECT (s_x);
    x = rect->x;
    y = rect->y;
    w = rect->w;
    h = rect->h;
  } else {
    ASSERT_EXACT (s_x, ARGH2);
    ASSERT_EXACT (s_y, ARGH3);
    ASSERT_EXACT (s_w, ARGH4);
    ASSERT_EXACT (s_h, ARGH5);
    x = (Sint32) gh_scm2long (s_x);
    y = (Sint32) gh_scm2long (s_y);
    w = (Sint32) gh_scm2long (s_w);
    h = (Sint32) gh_scm2long (s_h);
  }

  SDL_UpdateRect (UNPACK_SURFACE (s_surface), x, y, w, h);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


MDEFLOCEXP (update_rects, "sdl-update-rects", 2, 0, 0,
            (SCM s_surface, SCM ls),
            "On @var{surface}, update the rectangles in @var{ls}.\n"
            "The return value is unspecified.")
#define FUNC_NAME s_update_rects
{
  SDL_Surface *surface;
  SDL_Rect *rect;
  SCM p;

  ASSERT_SURFACE (s_surface, ARGH1);
  ASSERT_PAIR (ls, ARGH2);
  for (p = ls; ! gh_null_p (p); p = gh_cdr (p))
    ASSERT_RECT (gh_car (p), ARGH2);

  surface = UNPACK_SURFACE (s_surface);
  for (p = ls; ! gh_null_p (p); p = gh_cdr (p)) {
    rect = UNPACK_RECT (gh_car (p));
    SDL_UpdateRect (surface, rect->x, rect->y, rect->w, rect->h);
  }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


MDEFLOCEXP (sdl_flip, "sdl-flip", 0, 1, 0,
            (SCM s_surface),
            "Swap @var{surface} double buffers.\n"
            "The return value is unspecified.")
#define FUNC_NAME s_sdl_flip
{
  SDL_Surface *surface;

  if (BOUNDP (s_surface)) {
    ASSERT_SURFACE (s_surface, ARGH1);
    surface = UNPACK_SURFACE (s_surface);
  } else {
    surface = SDL_GetVideoSurface ();
  }

  SDL_Flip (surface);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


MDEFLOCEXP (set_colors, "sdl-set-colors!", 2, 0, 0,
            (SCM s_surface, SCM s_colors),
            "Set a portion of the colormap for the 8-bit @var{surface}\n"
            "using @var{colors}, a vector of SDL-Colors.")
#define FUNC_NAME s_set_colors
{
  SDL_Color *colors;
  SDL_Color *color;
  int i, length, result;

  ASSERT_SURFACE (s_surface, ARGH1);
  ASSERT_VECTOR (s_colors, ARGH2);

  length = gh_vector_length (s_colors);
  colors = (SDL_Color*) scm_must_malloc (length, FUNC_NAME);

  for (i = 0; i < length; i++) {
    color = UNPACK_COLOR (gh_vector_ref (s_colors, gh_long2scm (i)));
    colors[i] = *color;
  }

  result = SDL_SetColors (UNPACK_SURFACE (s_surface),
                          colors, 0, length);
  scm_must_free (colors);

  return result ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME


MDEFLOCEXP (set_palette, "sdl-set-palette", 3, 0, 0,
            (SCM s_surface, SCM s_flags, SCM s_colors),
            "Set the palette of an 8-bit @var{surface}\n"
            "using @var{flags} (see @code{flagstash:palette}) and\n"
            "@var{colors}, a vector of SDL-Colors.")
#define FUNC_NAME s_set_palette
{
  SDL_Color *colors;
  SDL_Color *color;
  int flags, i, length, result;

  ASSERT_SURFACE (s_surface, ARGH1);
  ASSERT_VECTOR (s_colors, ARGH3);

  flags   = GSDL_FLAGS2ULONG (s_flags, gsdl_palette_flags, ARGH2);
  length  = gh_vector_length (s_colors);
  colors  = (SDL_Color*) scm_must_malloc (length, FUNC_NAME);

  for (i = 0; i < length; i++) {
    color = UNPACK_COLOR (gh_vector_ref (s_colors, gh_long2scm (i)));
    colors[i] = *color;
  }

  result = SDL_SetPalette (UNPACK_SURFACE (s_surface),
                           flags, colors, 0, length);
  scm_must_free (colors);

  return result ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME


MDEFLOCEXP (set_gamma, "sdl-set-gamma", 3, 0, 0,
            (SCM s_redgamma, SCM s_greengamma, SCM s_bluegamma),
            "Set the color gamma function for the display\n"
            "using real numbers @var{redgamma}, @var{greengamma}\n"
            "and @var{bluegamma}.")
#define FUNC_NAME s_set_gamma
{
  ASSERT_NUMBER (s_redgamma,   ARGH1);
  ASSERT_NUMBER (s_greengamma, ARGH2);
  ASSERT_NUMBER (s_bluegamma,  ARGH3);

  RETURN_TRUE_IF_0
    (SDL_SetGamma ((float) SCM_REAL_VALUE (s_redgamma),
                   (float) SCM_REAL_VALUE (s_greengamma),
                   (float) SCM_REAL_VALUE (s_bluegamma)));
}
#undef FUNC_NAME

SCM_SYMBOL (gsdl_sym_redtable, "redtable");
SCM_SYMBOL (gsdl_sym_greentable, "greentable");
SCM_SYMBOL (gsdl_sym_bluetable, "bluetable");

#define GAMMAVEC(x)  (gh_shorts2svect ((short *) x, GAMMA_TABLE_SIZE))

MDEFLOCEXP (get_gamma_ramp, "sdl-get-gamma-ramp", 0, 0, 0,
            (void),
            "Get the gamma translation lookup tables currently used\n"
            "by the display.  Each table is a vector of 256 integer values.\n"
            "Return an alist with keys @code{redtable}, @code{greentable}\n"
            "and @code{bluetable}, and values the corresponding vectors.\n"
            "Return #f if unsuccesful.")
#define FUNC_NAME s_get_gamma_ramp
{
  Uint16 rt[GAMMA_TABLE_SIZE], gt[GAMMA_TABLE_SIZE], bt[GAMMA_TABLE_SIZE];

  if (SDL_GetGammaRamp (rt, gt, bt) == -1)
    return SCM_BOOL_F;

  return SCM_LIST3 (gh_cons (gsdl_sym_redtable,   GAMMAVEC (rt)),
                    gh_cons (gsdl_sym_greentable, GAMMAVEC (gt)),
                    gh_cons (gsdl_sym_bluetable,  GAMMAVEC (bt)));
}
#undef FUNC_NAME


#define ASSERT_VSZFIT(v,which)                          \
  ASSERT_VECTOR (v, which);                             \
  SCM_ASSERT (gh_vector_length (v) == GAMMA_TABLE_SIZE, \
              v, which, FUNC_NAME)

MDEFLOCEXP (set_gamma_ramp, "sdl-set-gamma-ramp", 3, 0, 0,
            (SCM s_redtable, SCM s_greentable, SCM s_bluetable),
            "Set the gamma translation lookup tables currently\n"
            "used by the display, for @var{redtable}, @var{greentable}\n"
            "and @var{bluetable}.  Each table is an vector of 256\n"
            "integer values.  Return #t if successful.")
#define FUNC_NAME s_get_gamma_ramp
{
  Uint16 rt[GAMMA_TABLE_SIZE], gt[GAMMA_TABLE_SIZE], bt[GAMMA_TABLE_SIZE];

  ASSERT_VSZFIT (s_redtable,   ARGH1);
  ASSERT_VSZFIT (s_greentable, ARGH2);
  ASSERT_VSZFIT (s_bluetable,  ARGH3);

  gh_scm2shorts (s_redtable,   (short *) rt);
  gh_scm2shorts (s_greentable, (short *) gt);
  gh_scm2shorts (s_bluetable,  (short *) bt);

  RETURN_TRUE_IF_0
    (SDL_SetGammaRamp (rt, gt, bt));
}
#undef FUNC_NAME


MDEFLOCEXP (map_rgb, "sdl-map-rgb", 2, 2, 0,
            (SCM s_pixel_format, SCM s_r, SCM s_g, SCM s_b),
            "Map a RGB color value to the pixel @var{format}.\n"
            "The second arg can be an SDL-Color, otherwise the second\n"
            "through fourth args are red, green and blue values (numbers).\n"
            "Return the mapped components as a number.")
#define FUNC_NAME s_map_rgb
{
  Uint8 r, g, b;

  ASSERT_PIXEL_FORMAT (s_pixel_format, ARGH1);

  if (SCM_SMOB_PREDICATE (color_tag, s_r)) {
    SDL_Color *color = UNPACK_COLOR (s_r);
    r = color->r;
    g = color->g;
    b = color->b;
  } else {
    ASSERT_EXACT (s_r, ARGH2);
    ASSERT_EXACT (s_g, ARGH3);
    ASSERT_EXACT (s_b, ARGH4);
    r = (Uint8) gh_scm2long (s_r);
    g = (Uint8) gh_scm2long (s_g);
    b = (Uint8) gh_scm2long (s_b);
  }

  return gh_long2scm (SDL_MapRGB (UNPACK_PIXEL_FORMAT (s_pixel_format),
                                  r, g, b));
}
#undef FUNC_NAME


MDEFLOCEXP (map_rgba, "sdl-map-rgba", 3, 2, 0,
            (SCM s_pixel_format, SCM s_r, SCM s_g, SCM s_b, SCM s_a),
            "Map a RGB color value to the pixel @var{format}.\n"
            "If the second arg is an SDL-Color, the third is an alpha\n"
            "value (number).  Otherwise, the second through fifth args\n"
            "are red, green, blue and alpha values (numbers).\n"
            "Return the mapped components as a number.")
#define FUNC_NAME s_map_rgba
{
  Uint8 r, g, b, a;

  ASSERT_PIXEL_FORMAT (s_pixel_format, ARGH1);

  if (SCM_SMOB_PREDICATE (color_tag, s_r)) {
    SDL_Color *color = UNPACK_COLOR (s_r);
    r = color->r;
    g = color->g;
    b = color->b;
    ASSERT_EXACT (s_g, ARGH3);
    a = (Uint8) gh_scm2long (s_g);
  } else {
    ASSERT_EXACT (s_r, ARGH2);
    ASSERT_EXACT (s_g, ARGH3);
    ASSERT_EXACT (s_b, ARGH4);
    ASSERT_EXACT (s_a, ARGH5);
    r = (Uint8) gh_scm2long (s_r);
    g = (Uint8) gh_scm2long (s_g);
    b = (Uint8) gh_scm2long (s_b);
    a = (Uint8) gh_scm2long (s_a);
  }

  return gh_long2scm (SDL_MapRGBA (UNPACK_PIXEL_FORMAT (s_pixel_format),
                                   r, g, b, a));
}
#undef FUNC_NAME


SCM_SYMBOL (gsdl_sym_r, "r");
SCM_SYMBOL (gsdl_sym_g, "g");
SCM_SYMBOL (gsdl_sym_b, "b");
SCM_SYMBOL (gsdl_sym_a, "a");

MDEFLOCEXP (get_rgb, "sdl-get-rgb", 2, 0, 0,
            (SCM s_pixel,
             SCM s_pixel_format),
            "Get RGB values from @var{pixel} in the specified pixel\n"
            "@var{format}.  Return an alist with keys @code{r}, @code{g}\n"
            "and @code{b}, with red, green and blue values (numbers),\n"
            "respectively.")
#define FUNC_NAME s_get_rgb
{
  Uint8 r, g, b;

  ASSERT_EXACT (s_pixel, ARGH1);
  ASSERT_PIXEL_FORMAT (s_pixel_format, ARGH2);

  SDL_GetRGB ((Uint32) gh_scm2long (s_pixel),
              UNPACK_PIXEL_FORMAT (s_pixel_format),
              &r, &g, &b);

  return SCM_LIST3 (gh_cons (gsdl_sym_r, gh_long2scm (r)),
                    gh_cons (gsdl_sym_g, gh_long2scm (g)),
                    gh_cons (gsdl_sym_b, gh_long2scm (b)));
}
#undef FUNC_NAME


MDEFLOCEXP (get_rgba, "sdl-get-rgba", 2, 0, 0,
            (SCM s_pixel, SCM s_pixel_format),
            "Get RGBA values from @var{pixel} in the specified pixel\n"
            "@var{format}.  Return an alist with keys @code{r}, @code{g},\n"
            "@code{b} and @code{a}, with red, green, blue and alpha values\n"
            "(numbers), respectively.")
#define FUNC_NAME s_get_rgba
{
  Uint8 r, g, b, a;

  ASSERT_EXACT (s_pixel, ARGH1);
  ASSERT_PIXEL_FORMAT (s_pixel_format, ARGH2);

  SDL_GetRGBA ((Uint32) gh_scm2long (s_pixel),
               UNPACK_PIXEL_FORMAT (s_pixel_format),
               &r, &g, &b, &a);

  return SCM_LIST4 (gh_cons (gsdl_sym_r, gh_long2scm (r)),
                    gh_cons (gsdl_sym_g, gh_long2scm (g)),
                    gh_cons (gsdl_sym_b, gh_long2scm (b)),
                    gh_cons (gsdl_sym_a, gh_long2scm (a)));
}
#undef FUNC_NAME


MDEFLOCEXP (fill_rect, "sdl-fill-rect", 3, 0, 0,
            (SCM s_dst, SCM s_dstrect, SCM s_color),
            "Fill @var{surface} @var{rect} with @var{color} (a number).\n"
            "Return #t if successful.")
#define FUNC_NAME s_fill_rect
{
  ASSERT_SURFACE (s_dst, ARGH1);
  ASSERT_RECT (s_dstrect, ARGH2);
  ASSERT_EXACT (s_color, ARGH3);

  RETURN_TRUE_IF_0
    (SDL_FillRect (UNPACK_SURFACE (s_dst),
                   UNPACK_RECT (s_dstrect),
                   (Uint32) gh_scm2long (s_color)));
}
#undef FUNC_NAME


MDEFLOCEXP (display_format, "sdl-display-format", 1, 0, 0,
            (SCM s_surface),
            "Return a new surface made by converting @var{surface}\n"
            "to the display format.  Return #f if not successful.")
#define FUNC_NAME s_display_format
{
  SDL_Surface *surface;

  ASSERT_SURFACE (s_surface, ARGH1);

  surface = SDL_DisplayFormat (UNPACK_SURFACE (s_surface));

  if (! surface)
    return SCM_BOOL_F;

  RETURN_NEW_SURFACE (surface);
}
#undef FUNC_NAME


MDEFLOCEXP (display_format_alpha, "sdl-display-format-alpha", 1, 0, 0,
            (SCM s_surface),
            "Return a new surface made by converting @var{surface}\n"
            "to the display format, with an alpha channel.  Return #f\n"
            "if not successful.")
#define FUNC_NAME s_display_format_alpha
{
  SDL_Surface *surface;

  ASSERT_SURFACE (s_surface, ARGH1);

  surface = SDL_DisplayFormatAlpha (UNPACK_SURFACE (s_surface));

  if (! surface)
    return SCM_BOOL_F;

  RETURN_NEW_SURFACE (surface);
}
#undef FUNC_NAME


MDEFLOCEXP (warp_mouse, "sdl-warp-mouse", 2, 0, 0,
            (SCM s_x, SCM s_y),
            "Set the position of the mouse cursor to @var{x},@var{y}.\n"
            "The return value is unspecified.")
#define FUNC_NAME s_warp_mouse
{
  ASSERT_EXACT (s_x, ARGH1);
  ASSERT_EXACT (s_y, ARGH2);

  SDL_WarpMouse ((Uint16) gh_scm2long (s_x), (Uint16) gh_scm2long (s_y));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


MDEFLOCEXP (set_cursor, "sdl-set-cursor", 1, 0, 0,
            (SCM s_cursor),
            "Set the current mouse cursor to @var{cursor}.\n"
            "The return value is unspecified.")
#define FUNC_NAME s_set_cursor
{
  ASSERT_CURSOR (s_cursor, ARGH1);
  SDL_SetCursor (UNPACK_CURSOR (s_cursor));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


MDEFLOCEXP (get_cursor, "sdl-get-cursor", 0, 0, 0,
            (void),
            "Get the current mouse cursor.")
#define FUNC_NAME s_get_cursor
{
  RETURN_NEW_CURSOR (SDL_GetCursor());
}
#undef FUNC_NAME


MDEFLOCEXP (show_cursor, "sdl-show-cursor", 0, 1, 0,
            (SCM query),
            "Toggle the visibility of the mouse cursor.\n"
            "Return #t if was being displayed before the call,\n"
            "and #f if not.  Optional arg @var{query} non-#f\n"
            "means to return the current state without toggling.")
#define FUNC_NAME s_show_cursor
{
  UNBOUND_MEANS_FALSE (query);
  return gh_bool2scm (SDL_ShowCursor (SCM_FALSEP (query) - 1));
}
#undef FUNC_NAME


MDEFLOCEXP (gl_get_attribute, "sdl-gl-get-attribute", 1, 0, 0,
            (SCM s_attr),
            "Return the value of a special SDL/OpenGL @var{attribute}.")
#define FUNC_NAME s_gl_get_attribute
{
  int value;

  ASSERT_EXACT (s_attr, ARGH1);

  SDL_GL_GetAttribute ((SDL_GLattr) gh_scm2long (s_attr), &value);
  return gh_long2scm (value);
}
#undef FUNC_NAME


MDEFLOCEXP (gl_set_attribute, "sdl-gl-set-attribute", 2, 0, 0,
            (SCM s_attr,
             SCM s_value),
            "Set the special SDL/OpenGL @var{attribute} to @var{value}.\n"
            "Both args are numbers.  The return value is unspecified.")
#define FUNC_NAME s_gl_set_attribute
{
  ASSERT_EXACT (s_attr, ARGH1);
  ASSERT_EXACT (s_value, ARGH2);

  SDL_GL_SetAttribute ((SDL_GLattr) gh_scm2long (s_attr),
                       (int) gh_scm2long (s_value));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


MDEFLOCEXP (gl_swap_buffers, "sdl-gl-swap-buffers", 0, 0, 0,
            (void),
            "Swap OpenGL framebuffers/Update Display.\n"
            "The return value is unspecified.")
#define FUNC_NAME s_gl_swap_buffers
{
  SDL_GL_SwapBuffers ();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


MDEFLOCEXP (lock_yuv_overlay, "sdl-lock-yuv-overlay", 1, 0, 0,
            (SCM s_overlay),
            "Lock the given YUV @var{overlay}.\n"
            "Return #f if successful.")
#define FUNC_NAME s_lock_yuv_overlay
{
  ASSERT_OVERLAY (s_overlay, ARGH1);

  RETURN_TRUE_IF_0
    (SDL_LockYUVOverlay (UNPACK_OVERLAY (s_overlay)));
}
#undef FUNC_NAME


MDEFLOCEXP (unlock_yuv_overlay, "sdl-unlock-yuv-overlay", 1, 0, 0,
            (SCM s_overlay),
            "Unlock a previously locked YUV @var{overlay}.\n"
            "The return value is unspecified.")
#define FUNC_NAME s_unlock_yuv_overlay
{
  ASSERT_OVERLAY (s_overlay, ARGH1);

  SDL_UnlockYUVOverlay (UNPACK_OVERLAY (s_overlay));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


MDEFLOCEXP (display_yuv_overlay, "sdl-display-yuv-overlay", 2, 0, 0,
            (SCM s_overlay, SCM s_dstrect),
            "Blit a YUV @var{overlay} to the display @var{dstrect}\n"
            "over which it was created.  Return #t if successful.")
#define FUNC_NAME s_display_yuv_overlay
{
  ASSERT_OVERLAY (s_overlay, ARGH1);
  ASSERT_RECT (s_dstrect, ARGH2);

  RETURN_TRUE_IF_0
    (SDL_DisplayYUVOverlay (UNPACK_OVERLAY (s_overlay),
                            UNPACK_RECT (s_dstrect)));
}
#undef FUNC_NAME


/* window manager functions */

MDEFLOCEXP (wm_set_caption, "sdl-set-caption", 2, 0, 0,
            (SCM s_title, SCM s_icon),
            "Set the title-bar and icon name of the display window\n"
            "to @var{title} and @var{icon} (both strings), respectively.")
#define FUNC_NAME s_wm_set_caption
{
  char *title, *icon;

  ASSERT_STRING (s_title, ARGH1);

  title = SCM_CHARS (s_title);

  if (UNBOUNDP (s_icon)) {
    icon = title;
  } else {
    ASSERT_STRING (s_icon, ARGH2);
    icon = SCM_CHARS (s_icon);
  }

  SDL_WM_SetCaption (title, icon);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_SYMBOL (gsdl_sym_title, "title");
SCM_SYMBOL (gsdl_sym_icon, "icon");

MDEFLOCEXP (wm_get_caption, "sdl-get-caption", 0, 0, 0,
            (void),
            "Return an alist with keys @code{title} and @code{icon}\n"
            "and values the title-bar and icon name of the display\n"
            "window, respectively.")
#define FUNC_NAME s_wm_get_caption
{
  char *title, *icon;

  SDL_WM_GetCaption (&title, &icon);
  return SCM_LIST2 (gh_cons (gsdl_sym_title, gh_str02scm (title)),
                    gh_cons (gsdl_sym_icon,  gh_str02scm (icon)));
}
#undef FUNC_NAME


MDEFLOCEXP (wm_set_icon, "sdl-set-icon", 1, 0, 0,
            (SCM icon),
            "Set @var{icon} for the display window.")
#define FUNC_NAME s_wm_set_icon
{
  ASSERT_SURFACE (icon, ARGH1);

  /* set w/ a NULL mask for now */
  SDL_WM_SetIcon (UNPACK_SURFACE (icon), NULL);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


MDEFLOCEXP (wm_iconify_window, "sdl-iconify-window", 0, 0, 0,
            (void),
            "Iconify/Minimize the window.\n"
            "Return #t if successful.")
#define FUNC_NAME s_wm_iconify_window
{
  return SDL_WM_IconifyWindow () ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME


MDEFLOCEXP (wm_toggle_full_screen, "sdl-toggle-full-screen", 0, 1, 0,
            (SCM s_surface),
            "Toggle the default video surface between windowed\n"
            "and fullscreen mode, if supported.  Optional arg\n"
            "@var{surface} specifies another surface to toggle.\n"
            "Return #t if successful.")
#define FUNC_NAME s_wm_toggle_full_screen
{
  SDL_Surface *surface;

  if (UNBOUNDP (s_surface)) {
    surface = SDL_GetVideoSurface ();
  } else {
    ASSERT_SURFACE (s_surface, ARGH1);
    surface = UNPACK_SURFACE (s_surface);
  }

  return SDL_WM_ToggleFullScreen (surface) ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME


MDEFLOCEXP (wm_grab_input, "sdl-grab-input", 0, 1, 0,
            (SCM s_mode),
            "Grab mouse and keyboard input.\n"
            "Optional arg @var{mode} (a number) specifies the kind\n"
            "of grab (default SDL_GRAB_QUERY).")
#define FUNC_NAME s_wm_grab_input
{
  int mode = SDL_GRAB_QUERY;

  if (BOUNDP (s_mode)) {
    ASSERT_EXACT (s_mode, ARGH1);
    mode = gh_scm2long (s_mode);
  }

  return gh_long2scm (SDL_WM_GrabInput (mode));
}
#undef FUNC_NAME



extern flagstash_t gsdl_video_flagstash;
extern flagstash_t gsdl_palette_flagstash;
extern flagstash_t gsdl_overlay_flagstash;

void
gsdl_init_video (void)
{
  cursor_tag = scm_make_smob_type ("SDL-Cursor", sizeof (SDL_Cursor));
  scm_set_smob_mark (cursor_tag, mark_cursor);
  scm_set_smob_free (cursor_tag, free_cursor);

  pixel_format_tag = scm_make_smob_type ("SDL-Pixel-Format",
                                         sizeof (SDL_PixelFormat));
  scm_set_smob_mark (pixel_format_tag, mark_pixel_format);
  scm_set_smob_free (pixel_format_tag, free_pixel_format);

  overlay_tag = scm_make_smob_type ("SDL-Overlay", sizeof (SDL_Overlay));
  scm_set_smob_mark (overlay_tag, mark_yuv_overlay);
  scm_set_smob_free (overlay_tag, free_yuv_overlay);

  /* alpha constants */
  gsdl_alpha_enums = gsdl_define_enum (
    "sdl-alpha-enums",
    "SDL_ALPHA_OPAQUE",      SDL_ALPHA_OPAQUE,
    "SDL_ALPHA_TRANSPARENT", SDL_ALPHA_TRANSPARENT,
    NULL);

  /* video flags */
  gsdl_video_flags = gsdl_make_flagstash (&gsdl_video_flagstash);

  /* palette flags */
  gsdl_palette_flags = gsdl_make_flagstash (&gsdl_palette_flagstash);

  /* yuv overlay formats */
  gsdl_overlay_formats = gsdl_make_flagstash (&gsdl_overlay_flagstash);

  /* GL constants */
  sdl_gl_enums = gsdl_define_enum (
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

#include "sdlvideo.x"
}

/* sdlvideo.c ends here */
