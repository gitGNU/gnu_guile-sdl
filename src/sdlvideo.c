/* sdlvideo.c --- SDL Video functions for Guile
 *
 * 	Copyright (C) 2003,2004,2005 Thien-Thi Nguyen
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
#include "retval.h"
#include "sym.h"
#include "bool.h"


#define MAX_DRIVER_LEN    100
#define GAMMA_TABLE_SIZE  256

#define COLOR_P(x) \
  (SCM_SMOB_PREDICATE (color_tag, x))

#define RECT_P(x) \
  (SCM_SMOB_PREDICATE (rect_tag, x))



static SCM gl_enums;
SCM gsdl_alpha_enums;

SCM gsdl_video_flags;
SCM gsdl_palette_flags;
static SCM gsdl_overlay_formats;

GH_DEFPROC (get_video_flags, "flagstash:video", 0, 0, 0, (),
            "Return the flagstash object for video flags.")
{
  return gsdl_video_flags;
}

GH_DEFPROC (get_palette_flags, "flagstash:palette", 0, 0, 0, (),
            "Return the flagstash object for palette flags.")
{
  return gsdl_palette_flags;
}

GH_DEFPROC (get_overlay_formats, "flagstash:overlay", 0, 0, 0, (),
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
mark_cursor (SCM cursor)
{
  return cursor;
}

static
size_t
free_cursor (SCM cursor)
{
  SDL_FreeCursor (UNPACK_CURSOR (cursor));
  /* return sizeof (SDL_Cursor); */
  return 0;
}

static
SCM
mark_yuv_overlay (SCM overlay)
{
  return overlay;
}

static
size_t
free_yuv_overlay (SCM overlay)
{
  SDL_FreeYUVOverlay (UNPACK_OVERLAY (overlay));
  /* return sizeof (SDL_Overlay); */
  return 0;
}

static
SCM
mark_pixel_format (SCM pixel_format)
{
  return pixel_format;
}

static
size_t
free_pixel_format (SCM pixel_format)
{
  /* Always part of a surface, no need to free.  */
  return 0;
}


/* scheme callable functions */

GH_DEFPROC (create_cursor, "create-cursor", 6, 0, 0,
            (SCM data, SCM mask,
             SCM w, SCM h,
             SCM x, SCM y),
            "Return a new cursor from @var{data} and @var{mask}\n"
            "(vectors), sized @var{w} by @var{h}\n"
            "and with hot pixel located at @var{x},@var{y}.")
{
#define FUNC_NAME s_create_cursor
  SDL_Cursor *cursor;
  Uint8 *cdata, *cmask;

  ASSERT_VECTOR (data, ARGH1);
  ASSERT_VECTOR (mask, ARGH2);
  ASSERT_EXACT (w, ARGH3);
  ASSERT_EXACT (h, ARGH4);
  ASSERT_EXACT (x, ARGH5);
  ASSERT_EXACT (y, ARGH6);

  /* Build the arrays.  */
  cdata = (Uint8 *) gh_scm2chars (data, NULL);
  cmask = (Uint8 *) gh_scm2chars (mask, NULL);

  /* Create the cursor.  */
  cursor = SDL_CreateCursor (cdata, cmask,
                             gh_scm2long (w),
                             gh_scm2long (h),
                             gh_scm2long (x),
                             gh_scm2long (y));

  /* Free the arrays.  */
  /*scm_must_*/free (cdata);
  /*scm_must_*/free (cmask);

  /* Return the new smob.  */
  RETURN_NEW_CURSOR (cursor);
#undef FUNC_NAME
}


#define GSDL_FLAG2ULONG(flag,table) \
  gsdl_flags2ulong ((flag), (table), 0, NULL) /* DWR! */

GH_DEFPROC (create_yuv_overlay, "create-yuv-overlay", 3, 1, 0,
            (SCM width, SCM height, SCM format, SCM display),
            "Create a new YUV overlay, sized @var{width} by @var{height}\n"
            "with overlay @var{format} (a symbol or an exact number).\n"
            "Optional arg @var{display} specifies a surface to use\n"
            "instead of creating a new one.")
{
#define FUNC_NAME s_create_yuv_overlay
  Uint32 cformat;
  SDL_Surface *cdisplay;

  ASSERT_EXACT (width, ARGH1);
  ASSERT_EXACT (height, ARGH2);

  if (gh_symbol_p (format))
    cformat = GSDL_FLAG2ULONG (format, gsdl_overlay_formats);
  else
    {
      ASSERT_EXACT (format, ARGH3);
      cformat = gh_scm2ulong (format);
    }

  if (UNBOUNDP (display))
    cdisplay = SDL_GetVideoSurface ();
  else
    {
      ASSERT_SURFACE (display, ARGH4);
      cdisplay = UNPACK_SURFACE (display);
    }

  RETURN_NEW_OVERLAY
    (SDL_CreateYUVOverlay (gh_scm2long (width),
                           gh_scm2long (height),
                           cformat,
                           cdisplay));
#undef FUNC_NAME
}


GH_DEFPROC (get_video_surface, "get-video-surface", 0, 0, 0,
            (void),
            "Return the current display surface.")
{
#define FUNC_NAME s_get_video_surface
  RETURN_NEW_SURFACE (SDL_GetVideoSurface ());
#undef FUNC_NAME
}


DECLARE_SYM (hw_available, "hw-available");
DECLARE_SYM (ww_available, "ww-available");
DECLARE_SYM (blit_hw,      "blit-hw");
DECLARE_SYM (blit_hw_CC,   "blit-hw-CC");
DECLARE_SYM (blit_hw_A,    "blit-hw-A");
DECLARE_SYM (blit_sw,      "blit-sw");
DECLARE_SYM (blit_sw_CC,   "blit-sw-CC");
DECLARE_SYM (blit_sw_A,    "blit-sw-A");
DECLARE_SYM (blit_fill,    "blit-fill");
DECLARE_SYM (video_mem,    "video-mem");
DECLARE_SIMPLE_SYM (vfmt);

GH_DEFPROC (get_video_info, "get-video-info", 0, 0, 0,
            (void),
            "Return information about the video hardware as an alist.\n"
            "Keys are: @code{hw-available}, @code{ww-available},\n"
            "@code{bit-hw}, @code{blit-hw-CC}, @code{blit-hw-A},\n"
            "@code{blit-sw}, @code{blit-sw-CC}, @code{blit-sw-A},\n"
            "@code{blit-fill}, @code{video-mem} and @code{vfmt}.")
{
#define FUNC_NAME s_get_video_info
  const SDL_VideoInfo *info = SDL_GetVideoInfo ();
  SCM format;

  SCM_NEWSMOB (format, pixel_format_tag, info->vfmt);

  RETURN_LIST11
    (gh_cons (SYM (hw_available), gh_bool2scm (info->hw_available)),
     gh_cons (SYM (ww_available), gh_bool2scm (info->wm_available)),
     gh_cons (SYM (blit_hw),      gh_bool2scm (info->blit_hw)),
     gh_cons (SYM (blit_hw_CC),   gh_bool2scm (info->blit_hw_CC)),
     gh_cons (SYM (blit_hw_A),    gh_bool2scm (info->blit_hw_A)),
     gh_cons (SYM (blit_sw),      gh_bool2scm (info->blit_sw)),
     gh_cons (SYM (blit_sw_CC),   gh_bool2scm (info->blit_sw_CC)),
     gh_cons (SYM (blit_sw_A),    gh_bool2scm (info->blit_sw_A)),
     gh_cons (SYM (blit_fill),    gh_ulong2scm (info->blit_fill)),
     gh_cons (SYM (video_mem),    gh_ulong2scm (info->video_mem)),
     gh_cons (SYM (vfmt),         format));
#undef FUNC_NAME
}


GH_DEFPROC (video_driver_name, "video-driver-name", 0, 0, 0,
            (void),
            "Return the name of the video driver.")
{
#define FUNC_NAME s_video_driver_name
  char name[MAX_DRIVER_LEN];
  SDL_VideoDriverName (name, MAX_DRIVER_LEN);
  RETURN_0STR (name);
#undef FUNC_NAME
}


GH_DEFPROC (list_modes, "list-modes", 0, 2, 0,
            (SCM format, SCM flags),
            "Return a list of available screen dimensions for pixel\n"
            "@var{format} and @var{flags}.  Format defaults to that for\n"
            "the current screen.  Flags default to none\n"
            "(see @code{flagstash:video}).\n"
            "Return #f if no modes are available, #t if all are available.")
{
#define FUNC_NAME s_list_modes
  SDL_PixelFormat *cformat = NULL;
  Uint32 cflags = 0;
  SDL_Rect **modes;
  SCM result;

  UNBOUND_MEANS_FALSE (format);
  if (NOT_FALSEP (format))
    {
      ASSERT_PIXEL_FORMAT (format, ARGH1);
      cformat = UNPACK_PIXEL_FORMAT (format);
    }

  UNBOUND_MEANS_FALSE (flags);
  if (NOT_FALSEP (flags))
    {
      ASSERT_EXACT (flags, ARGH2);
      cflags = GSDL_FLAGS2ULONG (flags, gsdl_video_flags, ARGH2);
    }

  modes = SDL_ListModes (cformat, cflags);

  if (modes == (SDL_Rect**)0)
    /* Return #f to signify no resolutions are available.  */
    SET_FALSE (result);
  else if (modes == (SDL_Rect**)-1)
    /* Return #t to signify all resolutions are available.  */
    SET_TRUE (result);
  else
    {
      int i;

      /* Otherwise return a list of the available resolutions.  */
      result = SCM_EOL;
      for (i = 0; modes[i]; i++)
        {
          SCM rect;

          SCM_NEWSMOB (rect, rect_tag, modes[i]);
          result = gh_cons (rect, result);
        }
    }
  return result;
#undef FUNC_NAME
}


GH_DEFPROC (video_mode_ok, "video-mode-ok", 3, 1, 0,
            (SCM width, SCM height, SCM bpp, SCM flags),
            "Check to see if a particular video mode is supported.\n"
            "Args are @var{width}, @var{height}, @var{bpp} (numbers),\n"
            "and @var{flags} (see @code{flagstash:video}).\n"
            "Return #f if the mode is not supported, or a number\n"
            "indicating the bits-per-pixel of the closest available\n"
            "mode supporting @var{width} and @var{height}.")
{
#define FUNC_NAME s_video_mode_ok
  Uint32 cflags = 0;
  int result;

  ASSERT_EXACT (width,  ARGH1);
  ASSERT_EXACT (height, ARGH2);
  ASSERT_EXACT (bpp,    ARGH3);

  if (BOUNDP (flags))
    cflags = GSDL_FLAGS2ULONG (flags, gsdl_video_flags, ARGH4);

  result = SDL_VideoModeOK (gh_scm2long (width),
                            gh_scm2long (height),
                            gh_scm2long (bpp),
                            cflags);
  return result ? gh_long2scm (result) : BOOL_FALSE;
#undef FUNC_NAME
}


GH_DEFPROC (set_video_mode, "set-video-mode", 3, 1, 0,
            (SCM width, SCM height, SCM bpp, SCM flags),
            "Set the SDL video mode with @var{width},\n"
            "@var{height} and bits-per-pixel @var{bpp}.  Optional arg\n"
            "@var{flags} (see @code{flagstash:video}) is supported.\n"
            "Return a new surface.")
{
#define FUNC_NAME s_set_video_mode
  Uint32 cflags = 0;

  ASSERT_EXACT (width,  ARGH1);
  ASSERT_EXACT (height, ARGH2);
  ASSERT_EXACT (bpp,    ARGH3);

  if (BOUNDP (flags))
    cflags = GSDL_FLAGS2ULONG (flags, gsdl_video_flags, ARGH4);

  RETURN_NEW_SURFACE
    (SDL_SetVideoMode (gh_scm2long (width),
                       gh_scm2long (height),
                       gh_scm2long (bpp),
                       cflags));
#undef FUNC_NAME
}


GH_DEFPROC (update_rect, "update-rect", 2, 3, 0,
            (SCM surface, SCM x, SCM y, SCM w, SCM h),
            "Update @var{surface} within a specified rectangle.\n"
            "The second arg can either be an SDL-Rect object, or\n"
            "the second through fifth args are numbers specifying\n"
            "the x, y, width and height of a rectangular area.\n"
            "The return value is unspecified.")
{
#define FUNC_NAME s_update_rect
  SDL_Rect *rect;
  Sint32 cx, cy, cw, ch;

  /* First arg is a surface.  */
  ASSERT_SURFACE (surface, ARGH1);

  /* Remaining args are a single rect, or 4 coords.  */
  if (RECT_P (x))
    {
      rect = UNPACK_RECT (x);
      cx = rect->x;
      cy = rect->y;
      cw = rect->w;
      ch = rect->h;
    }
  else
    {
      ASSERT_EXACT (x, ARGH2);
      ASSERT_EXACT (y, ARGH3);
      ASSERT_EXACT (w, ARGH4);
      ASSERT_EXACT (h, ARGH5);
      cx = gh_scm2long (x);
      cy = gh_scm2long (y);
      cw = gh_scm2long (w);
      ch = gh_scm2long (h);
    }

  SDL_UpdateRect (UNPACK_SURFACE (surface), cx, cy, cw, ch);
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


GH_DEFPROC (update_rects, "update-rects", 2, 0, 0,
            (SCM surface, SCM ls),
            "On @var{surface}, update the rectangles in @var{ls},\n"
            "a list of rectangles.\n"
            "The return value is unspecified.")
{
#define FUNC_NAME s_update_rects
  SDL_Surface *csurface;
  SDL_Rect *rect;
  SCM p;

  ASSERT_SURFACE (surface, ARGH1);
  ASSERT_PAIR (ls, ARGH2);
  for (p = ls; ! gh_null_p (p); p = gh_cdr (p))
    ASSERT_RECT (gh_car (p), ARGH2);

  csurface = UNPACK_SURFACE (surface);
  for (p = ls; ! gh_null_p (p); p = gh_cdr (p))
    {
      rect = UNPACK_RECT (gh_car (p));
      SDL_UpdateRect (csurface, rect->x, rect->y, rect->w, rect->h);
    }
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


GH_DEFPROC (flip, "flip", 0, 1, 0,
            (SCM surface),
            "Swap double buffers of the default surface,\n"
            "or of @var{surface} if specified.\n"
            "The return value is unspecified.")
{
#define FUNC_NAME s_flip
  SDL_Surface *csurface;

  if (BOUNDP (surface))
    {
      ASSERT_SURFACE (surface, ARGH1);
      csurface = UNPACK_SURFACE (surface);
    }
  else
    csurface = SDL_GetVideoSurface ();

  SDL_Flip (csurface);
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


GH_DEFPROC (set_colors, "set-colors!", 2, 0, 0,
            (SCM surface, SCM colors),
            "Set a portion of the colormap for the 8-bit @var{surface}\n"
            "using @var{colors}, a vector of SDL-Colors.")
{
#define FUNC_NAME s_set_colors
  SDL_Color *ccolors;
  SDL_Color *color;
  int i, length, result;

  ASSERT_SURFACE (surface, ARGH1);
  ASSERT_VECTOR (colors, ARGH2);

  length = gh_vector_length (colors);
  ccolors = (SDL_Color*) scm_must_malloc (length, FUNC_NAME);

  for (i = 0; i < length; i++)
    {
      color = UNPACK_COLOR (gh_vector_ref (colors, gh_long2scm (i)));
      ccolors[i] = *color;
    }

  result = SDL_SetColors (UNPACK_SURFACE (surface),
                          ccolors, 0, length);
  scm_must_free (ccolors);

  RETURN_BOOL
    (result);
#undef FUNC_NAME
}


GH_DEFPROC (set_palette, "set-palette", 3, 0, 0,
            (SCM surface, SCM flags, SCM colors),
            "Set the palette of an 8-bit @var{surface}\n"
            "using @var{flags} (see @code{flagstash:palette}) and\n"
            "@var{colors}, a vector of SDL-Colors.")
{
#define FUNC_NAME s_set_palette
  SDL_Color *ccolors;
  SDL_Color *color;
  int cflags, i, length, result;

  ASSERT_SURFACE (surface, ARGH1);
  ASSERT_VECTOR (colors, ARGH3);

  cflags   = GSDL_FLAGS2ULONG (flags, gsdl_palette_flags, ARGH2);
  length  = gh_vector_length (colors);
  ccolors  = (SDL_Color*) scm_must_malloc (length, FUNC_NAME);

  for (i = 0; i < length; i++)
    {
      color = UNPACK_COLOR (gh_vector_ref (colors, gh_long2scm (i)));
      ccolors[i] = *color;
    }

  result = SDL_SetPalette (UNPACK_SURFACE (surface),
                           cflags, ccolors, 0, length);
  scm_must_free (ccolors);

  RETURN_BOOL
    (result);
#undef FUNC_NAME
}


GH_DEFPROC (set_gamma, "set-gamma", 3, 0, 0,
            (SCM redgamma, SCM greengamma, SCM bluegamma),
            "Set the color gamma function for the display\n"
            "using real numbers @var{redgamma}, @var{greengamma}\n"
            "and @var{bluegamma}.")
{
#define FUNC_NAME s_set_gamma
  ASSERT_NUMBER (redgamma,   ARGH1);
  ASSERT_NUMBER (greengamma, ARGH2);
  ASSERT_NUMBER (bluegamma,  ARGH3);

  RETURN_TRUE_IF_0
    (SDL_SetGamma ((float) SCM_REAL_VALUE (redgamma),
                   (float) SCM_REAL_VALUE (greengamma),
                   (float) SCM_REAL_VALUE (bluegamma)));
#undef FUNC_NAME
}

DECLARE_SIMPLE_SYM (redtable);
DECLARE_SIMPLE_SYM (greentable);
DECLARE_SIMPLE_SYM (bluetable);

#define GAMMAVEC(x)  (gh_shorts2svect ((short *) x, GAMMA_TABLE_SIZE))

GH_DEFPROC (get_gamma_ramp, "get-gamma-ramp", 0, 0, 0,
            (void),
            "Get the gamma translation lookup tables currently used\n"
            "by the display.  Each table is a vector of 256 integer values.\n"
            "Return an alist with keys @code{redtable}, @code{greentable}\n"
            "and @code{bluetable}, and values the corresponding vectors.\n"
            "Return #f if unsuccessful.")
{
#define FUNC_NAME s_get_gamma_ramp
  Uint16 rt[GAMMA_TABLE_SIZE], gt[GAMMA_TABLE_SIZE], bt[GAMMA_TABLE_SIZE];

  if (SDL_GetGammaRamp (rt, gt, bt) == -1)
    RETURN_FALSE;

  RETURN_LIST3 (gh_cons (SYM (redtable),   GAMMAVEC (rt)),
                gh_cons (SYM (greentable), GAMMAVEC (gt)),
                gh_cons (SYM (bluetable),  GAMMAVEC (bt)));
#undef FUNC_NAME
}


#define ASSERT_VSZFIT(v,which)                          \
  ASSERT_VECTOR (v, which);                             \
  SCM_ASSERT (gh_vector_length (v) == GAMMA_TABLE_SIZE, \
              v, which, FUNC_NAME)

GH_DEFPROC (set_gamma_ramp, "set-gamma-ramp", 3, 0, 0,
            (SCM redtable, SCM greentable, SCM bluetable),
            "Set the gamma translation lookup tables currently\n"
            "used by the display, for @var{redtable}, @var{greentable}\n"
            "and @var{bluetable}.  Each table is an vector of 256\n"
            "integer values.  Return #t if successful.")
{
#define FUNC_NAME s_get_gamma_ramp
  Uint16 rt[GAMMA_TABLE_SIZE], gt[GAMMA_TABLE_SIZE], bt[GAMMA_TABLE_SIZE];

  ASSERT_VSZFIT (redtable,   ARGH1);
  ASSERT_VSZFIT (greentable, ARGH2);
  ASSERT_VSZFIT (bluetable,  ARGH3);

  gh_scm2shorts (redtable,   (short *) rt);
  gh_scm2shorts (greentable, (short *) gt);
  gh_scm2shorts (bluetable,  (short *) bt);

  RETURN_TRUE_IF_0
    (SDL_SetGammaRamp (rt, gt, bt));
#undef FUNC_NAME
}


GH_DEFPROC (map_rgb, "map-rgb", 2, 2, 0,
            (SCM format, SCM r, SCM g, SCM b),
            "Map a RGB color value to the pixel @var{format}.\n"
            "The second arg can be an SDL-Color, otherwise the second\n"
            "through fourth args are red, green and blue values (numbers).\n"
            "Return the mapped components as a number.")
{
#define FUNC_NAME s_map_rgb
  Uint8 cr, cg, cb;

  ASSERT_PIXEL_FORMAT (format, ARGH1);

  if (COLOR_P (r))
    {
      SDL_Color *color = UNPACK_COLOR (r);
      cr = color->r;
      cg = color->g;
      cb = color->b;
    }
  else
    {
      ASSERT_EXACT (r, ARGH2);
      ASSERT_EXACT (g, ARGH3);
      ASSERT_EXACT (b, ARGH4);
      cr = (Uint8) gh_scm2long (r);
      cg = (Uint8) gh_scm2long (g);
      cb = (Uint8) gh_scm2long (b);
    }

  RETURN_INT (SDL_MapRGB (UNPACK_PIXEL_FORMAT (format),
                          cr, cg, cb));
#undef FUNC_NAME
}


GH_DEFPROC (map_rgba, "map-rgba", 3, 2, 0,
            (SCM format, SCM r, SCM g, SCM b, SCM a),
            "Map a RGB color value to the pixel @var{format}.\n"
            "If the second arg is an SDL-Color, the third is an alpha\n"
            "value (number).  Otherwise, the second through fifth args\n"
            "are red, green, blue and alpha values (numbers).\n"
            "Return the mapped components as a number.")
{
#define FUNC_NAME s_map_rgba
  Uint8 cr, cg, cb, ca;

  ASSERT_PIXEL_FORMAT (format, ARGH1);

  if (COLOR_P (r))
    {
      SDL_Color *color = UNPACK_COLOR (r);
      cr = color->r;
      cg = color->g;
      cb = color->b;
      ASSERT_EXACT (g, ARGH3);
      ca = (Uint8) gh_scm2long (g);
    }
  else
    {
      ASSERT_EXACT (r, ARGH2);
      ASSERT_EXACT (g, ARGH3);
      ASSERT_EXACT (b, ARGH4);
      ASSERT_EXACT (a, ARGH5);
      cr = (Uint8) gh_scm2long (r);
      cg = (Uint8) gh_scm2long (g);
      cb = (Uint8) gh_scm2long (b);
      ca = (Uint8) gh_scm2long (a);
    }

  RETURN_INT (SDL_MapRGBA (UNPACK_PIXEL_FORMAT (format),
                           cr, cg, cb, ca));
#undef FUNC_NAME
}


DECLARE_SIMPLE_SYM (r);
DECLARE_SIMPLE_SYM (g);
DECLARE_SIMPLE_SYM (b);
DECLARE_SIMPLE_SYM (a);

GH_DEFPROC (get_rgb, "get-rgb", 2, 0, 0,
            (SCM pixel,
             SCM format),
            "Get RGB values from @var{pixel} in the specified pixel\n"
            "@var{format}.  Return an alist with keys @code{r}, @code{g}\n"
            "and @code{b}, with red, green and blue values (numbers),\n"
            "respectively.")
{
#define FUNC_NAME s_get_rgb
  Uint8 r, g, b;

  ASSERT_EXACT (pixel, ARGH1);
  ASSERT_PIXEL_FORMAT (format, ARGH2);

  SDL_GetRGB ((Uint32) gh_scm2long (pixel),
              UNPACK_PIXEL_FORMAT (format),
              &r, &g, &b);

  RETURN_LIST3 (gh_cons (SYM (r), gh_long2scm (r)),
                gh_cons (SYM (g), gh_long2scm (g)),
                gh_cons (SYM (b), gh_long2scm (b)));
#undef FUNC_NAME
}


GH_DEFPROC (get_rgba, "get-rgba", 2, 0, 0,
            (SCM pixel, SCM format),
            "Get RGBA values from @var{pixel} in the specified pixel\n"
            "@var{format}.  Return an alist with keys @code{r}, @code{g},\n"
            "@code{b} and @code{a}, with red, green, blue and alpha values\n"
            "(numbers), respectively.")
{
#define FUNC_NAME s_get_rgba
  Uint8 r, g, b, a;

  ASSERT_EXACT (pixel, ARGH1);
  ASSERT_PIXEL_FORMAT (format, ARGH2);

  SDL_GetRGBA ((Uint32) gh_scm2long (pixel),
               UNPACK_PIXEL_FORMAT (format),
               &r, &g, &b, &a);

  RETURN_LIST4 (gh_cons (SYM (r), gh_long2scm (r)),
                gh_cons (SYM (g), gh_long2scm (g)),
                gh_cons (SYM (b), gh_long2scm (b)),
                gh_cons (SYM (a), gh_long2scm (a)));
#undef FUNC_NAME
}


GH_DEFPROC (fill_rect, "fill-rect", 3, 0, 0,
            (SCM surface, SCM rect, SCM color),
            "Fill @var{surface} @var{rect} with @var{color} (a number).\n"
            "Return #t if successful.")
{
#define FUNC_NAME s_fill_rect
  ASSERT_SURFACE (surface, ARGH1);
  ASSERT_RECT (rect, ARGH2);
  ASSERT_EXACT (color, ARGH3);

  RETURN_TRUE_IF_0
    (SDL_FillRect (UNPACK_SURFACE (surface),
                   UNPACK_RECT (rect),
                   gh_scm2ulong (color)));
#undef FUNC_NAME
}


GH_DEFPROC (display_format, "display-format", 1, 0, 0,
            (SCM surface),
            "Return a new surface made by converting @var{surface}\n"
            "to the display format.  Return #f if not successful.")
{
#define FUNC_NAME s_display_format
  SDL_Surface *csurface;

  ASSERT_SURFACE (surface, ARGH1);

  csurface = SDL_DisplayFormat (UNPACK_SURFACE (surface));

  if (! csurface)
    RETURN_FALSE;

  RETURN_NEW_SURFACE (csurface);
#undef FUNC_NAME
}


GH_DEFPROC (display_format_alpha, "display-format-alpha", 1, 0, 0,
            (SCM surface),
            "Return a new surface made by converting @var{surface}\n"
            "to the display format, with an alpha channel.  Return #f\n"
            "if not successful.")
{
#define FUNC_NAME s_display_format_alpha
  SDL_Surface *csurface;

  ASSERT_SURFACE (surface, ARGH1);

  csurface = SDL_DisplayFormatAlpha (UNPACK_SURFACE (surface));

  if (! csurface)
    RETURN_FALSE;

  RETURN_NEW_SURFACE (csurface);
#undef FUNC_NAME
}


GH_DEFPROC (warp_mouse, "warp-mouse", 2, 0, 0,
            (SCM x, SCM y),
            "Set the position of the mouse cursor to @var{x},@var{y}.\n"
            "The return value is unspecified.")
{
#define FUNC_NAME s_warp_mouse
  ASSERT_EXACT (x, ARGH1);
  ASSERT_EXACT (y, ARGH2);

  SDL_WarpMouse ((Uint16) gh_scm2long (x), (Uint16) gh_scm2long (y));
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


GH_DEFPROC (set_cursor, "set-cursor", 1, 0, 0,
            (SCM cursor),
            "Set the current mouse cursor to @var{cursor}.\n"
            "The return value is unspecified.")
{
#define FUNC_NAME s_set_cursor
  ASSERT_CURSOR (cursor, ARGH1);
  SDL_SetCursor (UNPACK_CURSOR (cursor));
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


GH_DEFPROC (get_cursor, "get-cursor", 0, 0, 0,
            (void),
            "Get the current mouse cursor.")
{
#define FUNC_NAME s_get_cursor
  RETURN_NEW_CURSOR (SDL_GetCursor ());
#undef FUNC_NAME
}


GH_DEFPROC (show_cursor, "show-cursor", 0, 1, 0,
            (SCM query),
            "Toggle the visibility of the mouse cursor.\n"
            "Return #t if was being displayed before the call,\n"
            "and #f if not.  Optional arg @var{query} non-#f\n"
            "means to return the current state without toggling.")
{
#define FUNC_NAME s_show_cursor
  UNBOUND_MEANS_FALSE (query);
  RETURN_BOOL (SDL_ShowCursor (EXACTLY_FALSEP (query) - 1));
#undef FUNC_NAME
}


GH_DEFPROC (gl_get_attribute, "gl-get-attribute", 1, 0, 0,
            (SCM attribute),
            "Return the value of a special SDL/OpenGL @var{attribute}.")
{
#define FUNC_NAME s_gl_get_attribute
  int value;

  ASSERT_EXACT (attribute, ARGH1);

  SDL_GL_GetAttribute ((SDL_GLattr) gh_scm2long (attribute), &value);
  RETURN_INT (value);
#undef FUNC_NAME
}


GH_DEFPROC (gl_set_attribute, "gl-set-attribute", 2, 0, 0,
            (SCM attribute,
             SCM value),
            "Set the special SDL/OpenGL @var{attribute} to @var{value}.\n"
            "Both args are numbers.  The return value is unspecified.")
{
#define FUNC_NAME s_gl_set_attribute
  ASSERT_EXACT (attribute, ARGH1);
  ASSERT_EXACT (value, ARGH2);

  SDL_GL_SetAttribute ((SDL_GLattr) gh_scm2long (attribute),
                       (int) gh_scm2long (value));
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


GH_DEFPROC (gl_swap_buffers, "gl-swap-buffers", 0, 0, 0,
            (void),
            "Swap OpenGL framebuffers/Update Display.\n"
            "The return value is unspecified.")
{
#define FUNC_NAME s_gl_swap_buffers
  SDL_GL_SwapBuffers ();
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


GH_DEFPROC (lock_yuv_overlay, "lock-yuv-overlay", 1, 0, 0,
            (SCM overlay),
            "Lock the given YUV @var{overlay}.\n"
            "Return #f if successful.")
{
#define FUNC_NAME s_lock_yuv_overlay
  ASSERT_OVERLAY (overlay, ARGH1);

  RETURN_TRUE_IF_0
    (SDL_LockYUVOverlay (UNPACK_OVERLAY (overlay)));
#undef FUNC_NAME
}


GH_DEFPROC (unlock_yuv_overlay, "unlock-yuv-overlay", 1, 0, 0,
            (SCM overlay),
            "Unlock the previously locked YUV @var{overlay}.\n"
            "The return value is unspecified.")
{
#define FUNC_NAME s_unlock_yuv_overlay
  ASSERT_OVERLAY (overlay, ARGH1);

  SDL_UnlockYUVOverlay (UNPACK_OVERLAY (overlay));
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


GH_DEFPROC (display_yuv_overlay, "display-yuv-overlay", 2, 0, 0,
            (SCM overlay, SCM dstrect),
            "Blit the YUV @var{overlay} to the display @var{dstrect}\n"
            "over which it was created.  Return #t if successful.")
{
#define FUNC_NAME s_display_yuv_overlay
  ASSERT_OVERLAY (overlay, ARGH1);
  ASSERT_RECT (dstrect, ARGH2);

  RETURN_TRUE_IF_0
    (SDL_DisplayYUVOverlay (UNPACK_OVERLAY (overlay),
                            UNPACK_RECT (dstrect)));
#undef FUNC_NAME
}


/* window manager functions */

GH_DEFPROC (wm_set_caption, "set-caption", 1, 1, 0,
            (SCM title, SCM icon),
            "Set the title-bar and icon name of the display window\n"
            "to @var{title} and @var{icon} (both strings), respectively.\n"
            "If @var{icon} is not specified, use @var{title} by default.")
{
#define FUNC_NAME s_wm_set_caption
  char *ctitle, *cicon;

  ASSERT_STRING (title, ARGH1);

  ctitle = SCM_CHARS (title);

  if (UNBOUNDP (icon))
    cicon = ctitle;
  else
    {
      ASSERT_STRING (icon, ARGH2);
      cicon = SCM_CHARS (icon);
    }

  SDL_WM_SetCaption (ctitle, cicon);

  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}

DECLARE_SIMPLE_SYM (title);
DECLARE_SIMPLE_SYM (icon);

GH_DEFPROC (wm_get_caption, "get-caption", 0, 0, 0,
            (void),
            "Return an alist with keys @code{title} and @code{icon}\n"
            "and values the title-bar and icon name of the display\n"
            "window, respectively.")
{
#define FUNC_NAME s_wm_get_caption
  char *title, *icon;

  SDL_WM_GetCaption (&title, &icon);
  RETURN_LIST2 (gh_cons (SYM (title), gh_str02scm (title)),
                gh_cons (SYM (icon),  gh_str02scm (icon)));
#undef FUNC_NAME
}


GH_DEFPROC (wm_set_icon, "set-icon", 1, 0, 0,
            (SCM icon),
            "Set @var{icon} for the display window.")
{
#define FUNC_NAME s_wm_set_icon
  ASSERT_SURFACE (icon, ARGH1);

  /* Set w/ a NULL mask for now.  */
  SDL_WM_SetIcon (UNPACK_SURFACE (icon), NULL);
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


GH_DEFPROC (wm_iconify_window, "iconify-window", 0, 0, 0,
            (void),
            "Iconify/Minimize the window.\n"
            "Return #t if successful.")
{
#define FUNC_NAME s_wm_iconify_window
  RETURN_BOOL
    (SDL_WM_IconifyWindow ());
#undef FUNC_NAME
}


GH_DEFPROC (wm_toggle_full_screen, "toggle-full-screen", 0, 1, 0,
            (SCM surface),
            "Toggle the default video surface between windowed\n"
            "and fullscreen mode, if supported.  Optional arg\n"
            "@var{surface} specifies another surface to toggle.\n"
            "Return #t if successful.")
{
#define FUNC_NAME s_wm_toggle_full_screen
  SDL_Surface *csurface;

  if (UNBOUNDP (surface))
    csurface = SDL_GetVideoSurface ();
  else
    {
      ASSERT_SURFACE (surface, ARGH1);
      csurface = UNPACK_SURFACE (surface);
    }

  RETURN_BOOL
    (SDL_WM_ToggleFullScreen (csurface));
#undef FUNC_NAME
}


GH_DEFPROC (wm_grab_input, "grab-input", 0, 1, 0,
            (SCM mode),
            "Grab mouse and keyboard input.\n"
            "Optional arg @var{mode} (a number) specifies the kind\n"
            "of grab (default SDL_GRAB_QUERY).")
{
#define FUNC_NAME s_wm_grab_input
  int cmode = SDL_GRAB_QUERY;

  if (BOUNDP (mode))
    {
      ASSERT_EXACT (mode, ARGH1);
      cmode = gh_scm2long (mode);
    }

  RETURN_INT (SDL_WM_GrabInput (cmode));
#undef FUNC_NAME
}



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
                                       "alpha-enums",
                                       GSDL_CSCS (SDL_ALPHA_OPAQUE),
                                       GSDL_CSCS (SDL_ALPHA_TRANSPARENT),
                                       NULL);

  /* video flags */
  gsdl_video_flags = gsdl_make_flagstash (&gsdl_video_flagstash);

  /* palette flags */
  gsdl_palette_flags = gsdl_make_flagstash (&gsdl_palette_flagstash);

  /* yuv overlay formats */
  gsdl_overlay_formats = gsdl_make_flagstash (&gsdl_overlay_flagstash);

  /* GL constants */
  gl_enums = gsdl_define_enum
    ("gl-enums",
     GSDL_CSCS (SDL_GL_RED_SIZE),
     GSDL_CSCS (SDL_GL_GREEN_SIZE),
     GSDL_CSCS (SDL_GL_BLUE_SIZE),
     GSDL_CSCS (SDL_GL_ALPHA_SIZE),
     GSDL_CSCS (SDL_GL_BUFFER_SIZE),
     GSDL_CSCS (SDL_GL_DOUBLEBUFFER),
     GSDL_CSCS (SDL_GL_DEPTH_SIZE),
     GSDL_CSCS (SDL_GL_STENCIL_SIZE),
     GSDL_CSCS (SDL_GL_ACCUM_RED_SIZE),
     GSDL_CSCS (SDL_GL_ACCUM_GREEN_SIZE),
     GSDL_CSCS (SDL_GL_ACCUM_BLUE_SIZE),
     GSDL_CSCS (SDL_GL_ACCUM_ALPHA_SIZE),
     NULL);

#include "sdlvideo.x"
}

/* sdlvideo.c ends here */
