/* sdlvideo.c --- SDL Video functions for Guile
 *
 * Copyright (C) 2003, 2004, 2005, 2007, 2009, 2011 Thien-Thi Nguyen
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program; if not, write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA  02110-1301  USA
 */

#include "guile-sdl.h"
#include <stdio.h>
#include <alloca.h>
#include <SDL/SDL.h>
#include <SDL/SDL_image.h>

#define MAX_DRIVER_LEN    100
#define GAMMA_TABLE_SIZE  256

#define COLOR_P(x) \
  (SCM_SMOB_PREDICATE (color_tag, x))

#define RECT_P(x) \
  (SCM_SMOB_PREDICATE (rect_tag, x))



static SCM gl_enums;
static valaka_t gl_eback[] = {
  VALAKA (SDL_GL_RED_SIZE),
  VALAKA (SDL_GL_GREEN_SIZE),
  VALAKA (SDL_GL_BLUE_SIZE),
  VALAKA (SDL_GL_ALPHA_SIZE),
  VALAKA (SDL_GL_BUFFER_SIZE),
  VALAKA (SDL_GL_DOUBLEBUFFER),
  VALAKA (SDL_GL_DEPTH_SIZE),
  VALAKA (SDL_GL_STENCIL_SIZE),
  VALAKA (SDL_GL_ACCUM_RED_SIZE),
  VALAKA (SDL_GL_ACCUM_GREEN_SIZE),
  VALAKA (SDL_GL_ACCUM_BLUE_SIZE),
  VALAKA (SDL_GL_ACCUM_ALPHA_SIZE)
};

SCM gsdl_alpha_enums;
static valaka_t alpha_eback[] = {
  VALAKA (SDL_ALPHA_OPAQUE),
  VALAKA (SDL_ALPHA_TRANSPARENT)
};

SCM gsdl_video_flags;
SCM gsdl_palette_flags;
static SCM gsdl_overlay_formats;

PRIMPROC
(get_video_flags, "flagstash:video", 0, 0, 0, (),
 doc: /***********
Return the flagstash object for video flags.
@xref{Enums and Constants}.  */)
{
  return gsdl_video_flags;
}

PRIMPROC
(get_palette_flags, "flagstash:palette", 0, 0, 0, (),
 doc: /***********
Return the flagstash object for palette flags.
@xref{Enums and Constants}.  */)
{
  return gsdl_palette_flags;
}

PRIMPROC
(get_overlay_formats, "flagstash:overlay", 0, 0, 0, (),
 doc: /***********
Return the flagstash object for overlay flags.
(Actually, these are "common overlay formats", not flags.)
@xref{Enums and Constants}.  */)
{
  return gsdl_overlay_formats;
}



/* extended SDL_* structures */

typedef struct {
  int freeable;
  SDL_Cursor *c;
} xSDL_Cursor;

#define MALLOC_XSDL_CURSOR(who) \
  (xSDL_Cursor *) scm_must_malloc (sizeof (xSDL_Cursor), who)



/* tags for SDL smobs */
static long cursor_tag;
static long overlay_tag;

#define ASSERT_CURSOR(obj,which)   ASSERT_SMOB (obj, cursor_tag, which)
#define ASSERT_OVERLAY(obj,which)  ASSERT_SMOB (obj, overlay_tag, which)

#define UNPACK_CURSOR(smob)       (SMOBGET (smob, xSDL_Cursor *))
#define UNPACK_OVERLAY(smob)      (SMOBGET (smob, SDL_Overlay *))

#define RETURN_NEW_CURSOR(x)    NEWSMOB_OR_FALSE (cursor_tag, x)
#define RETURN_NEW_OVERLAY(x)   NEWSMOB_OR_FALSE (overlay_tag, x)

/* smob functions */

static
size_t
free_cursor (SCM cursor)
{
  xSDL_Cursor *ccursor = UNPACK_CURSOR (cursor);

  if (ccursor->freeable)
    SDL_FreeCursor (ccursor->c);
  free (ccursor);
  return sizeof (xSDL_Cursor);
}

static
size_t
free_yuv_overlay (SCM overlay)
{
  SDL_FreeYUVOverlay (UNPACK_OVERLAY (overlay));
  return 0;
}

static
size_t
free_pixel_format (SCM pixel_format)
{
  /* Always part of a surface.  */
  return 0;
}

static
int
print_pixel_format (SCM pixel_format, SCM port, scm_print_state *pstate)
{
  SDL_PixelFormat *f = UNPACK_PIXEL_FORMAT (pixel_format);
  char buf[80];

  snprintf (buf, 80, "#<SDL-Pixel-Format %d %d %x %d %s%s%s%s>",
            f->palette ? f->palette->ncolors : -1,
            f->BitsPerPixel,
            f->colorkey,
            f->alpha,
            f->Rmask ? "R" : "",
            f->Gmask ? "G" : "",
            f->Bmask ? "B" : "",
            f->Amask ? "A" : "");
  scm_puts (buf, port);
  return 1;
}


/* scheme callable functions */

PRIMPROC
(create_cursor, "create-cursor", 6, 0, 0,
 (SCM data, SCM mask,
  SCM w, SCM h,
  SCM x, SCM y),
 doc: /***********
Return a new cursor from @var{data} and @var{mask}
(vectors), sized @var{w} by @var{h}
and with hot pixel located at @var{x},@var{y}.  */)
{
#define FUNC_NAME s_create_cursor
  xSDL_Cursor *cursor;
  Uint8 *cdata, *cmask;

  ASSERT_VECTOR (data, 1);
  ASSERT_VECTOR (mask, 2);
  ASSERT_EXACT (w, 3);
  ASSERT_EXACT (h, 4);
  ASSERT_EXACT (x, 5);
  ASSERT_EXACT (y, 6);

  /* Build the arrays.  */
  cdata = alloca (sizeof (Uint8) * VECLENGTH (data));
  cmask = alloca (sizeof (Uint8) * VECLENGTH (mask));

  /* Create the cursor.  */
  if ((cursor = MALLOC_XSDL_CURSOR (FUNC_NAME)))
    {
      cursor->c = SDL_CreateCursor (gsdl_scm_to_uint8s (data, cdata),
                                    gsdl_scm_to_uint8s (mask, cmask),
                                    C_LONG (w), C_LONG (h),
                                    C_LONG (x), C_LONG (y));
      cursor->freeable = 1;
    }

  /* Return the new smob.  */
  RETURN_NEW_CURSOR (cursor);
#undef FUNC_NAME
}


#define GSDL_FLAG2ULONG(flag,table) \
  gsdl_flags2ulong ((flag), (table), 0, NULL) /* DWR! */

PRIMPROC
(create_yuv_overlay, "create-yuv-overlay", 3, 1, 0,
 (SCM width, SCM height, SCM format, SCM display),
 doc: /***********
Create a new YUV overlay, sized @var{width} by @var{height}
with overlay @var{format} (a symbol or an exact number).
Optional arg @var{display} specifies a surface to use
instead of creating a new one.  */)
{
#define FUNC_NAME s_create_yuv_overlay
  Uint32 cformat;
  SDL_Surface *cdisplay;

  ASSERT_EXACT (width, 1);
  ASSERT_EXACT (height, 2);

  if (SYMBOLP (format))
    cformat = GSDL_FLAG2ULONG (format, gsdl_overlay_formats);
  else
    {
      ASSERT_EXACT (format, 3);
      cformat = C_ULONG (format);
    }

  if (UNBOUNDP (display))
    cdisplay = SDL_GetVideoSurface ();
  else
    {
      ASSERT_SURFACE (display, 4);
      cdisplay = UNPACK_SURFACE (display);
    }

  RETURN_NEW_OVERLAY
    (SDL_CreateYUVOverlay (C_LONG (width), C_LONG (height),
                           cformat, cdisplay));
#undef FUNC_NAME
}


PRIMPROC
(get_video_surface, "get-video-surface", 0, 0, 0,
 (void),
 doc: /***********
Return the current display surface.  */)
{
#define FUNC_NAME s_get_video_surface
  RETURN_NEW_SURFACE (SDL_GetVideoSurface ());
#undef FUNC_NAME
}


DECLARE_SYM (hw_available, "hw-available");
DECLARE_SYM (wm_available, "wm-available");
DECLARE_SYM (blit_hw,      "blit-hw");
DECLARE_SYM (blit_hw_CC,   "blit-hw-CC");
DECLARE_SYM (blit_hw_A,    "blit-hw-A");
DECLARE_SYM (blit_sw,      "blit-sw");
DECLARE_SYM (blit_sw_CC,   "blit-sw-CC");
DECLARE_SYM (blit_sw_A,    "blit-sw-A");
DECLARE_SYM (blit_fill,    "blit-fill");
DECLARE_SYM (video_mem,    "video-mem");
DECLARE_SIMPLE_SYM (vfmt);

PRIMPROC
(get_video_info, "get-video-info", 0, 0, 0,
 (void),
 doc: /***********
Return information about the video hardware as an alist.
Keys are: @code{hw-available}, @code{wm-available},
@code{bit-hw}, @code{blit-hw-CC}, @code{blit-hw-A},
@code{blit-sw}, @code{blit-sw-CC}, @code{blit-sw-A},
@code{blit-fill}, @code{video-mem} and @code{vfmt}.  */)
{
#define FUNC_NAME s_get_video_info
  const SDL_VideoInfo *info = SDL_GetVideoInfo ();
  SCM format;

  SCM_NEWSMOB (format, pixel_format_tag, info->vfmt);

  RETURN_LIST11
    (CONS (SYM (hw_available), BOOLEAN (info->hw_available)),
     CONS (SYM (wm_available), BOOLEAN (info->wm_available)),
     CONS (SYM (blit_hw),      BOOLEAN (info->blit_hw)),
     CONS (SYM (blit_hw_CC),   BOOLEAN (info->blit_hw_CC)),
     CONS (SYM (blit_hw_A),    BOOLEAN (info->blit_hw_A)),
     CONS (SYM (blit_sw),      BOOLEAN (info->blit_sw)),
     CONS (SYM (blit_sw_CC),   BOOLEAN (info->blit_sw_CC)),
     CONS (SYM (blit_sw_A),    BOOLEAN (info->blit_sw_A)),
     CONS (SYM (blit_fill),    BOOLEAN (info->blit_fill)),
     CONS (SYM (video_mem),    NUM_ULONG (info->video_mem)),
     CONS (SYM (vfmt),         format));
#undef FUNC_NAME
}


PRIMPROC
(video_driver_name, "video-driver-name", 0, 0, 0,
 (void),
 doc: /***********
Return the name of the video driver.  */)
{
#define FUNC_NAME s_video_driver_name
  char name[MAX_DRIVER_LEN];
  SDL_VideoDriverName (name, MAX_DRIVER_LEN);
  RETURN_0STR (name);
#undef FUNC_NAME
}


PRIMPROC
(list_modes, "list-modes", 0, 2, 0,
 (SCM format, SCM flags),
 doc: /***********
Return a list of available screen dimensions for pixel
@var{format} and @var{flags}.  Format defaults to that for
the current screen.  Flags default to none
(see @code{flagstash:video}).
Return #f if no modes are available, #t if all are available.  */)
{
#define FUNC_NAME s_list_modes
  SDL_PixelFormat *cformat = NULL;
  Uint32 cflags = 0;
  SDL_Rect **modes;
  SCM result;

  UNBOUND_MEANS_FALSE (format);
  if (NOT_FALSEP (format))
    {
      ASSERT_PIXEL_FORMAT (format, 1);
      cformat = UNPACK_PIXEL_FORMAT (format);
    }

  UNBOUND_MEANS_FALSE (flags);
  if (NOT_FALSEP (flags))
    {
      ASSERT_EXACT (flags, 2);
      cflags = GSDL_FLAGS2ULONG (flags, gsdl_video_flags, 2);
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
          result = CONS (rect, result);
        }
    }
  return result;
#undef FUNC_NAME
}


PRIMPROC
(video_mode_ok, "video-mode-ok", 3, 1, 0,
 (SCM width, SCM height, SCM bpp, SCM flags),
 doc: /***********
Check to see if a particular video mode is supported.
Args are @var{width}, @var{height}, @var{bpp} (numbers),
and @var{flags} (see @code{flagstash:video}).
Return #f if the mode is not supported, or a number
indicating the bits-per-pixel of the closest available
mode supporting @var{width} and @var{height}.  */)
{
#define FUNC_NAME s_video_mode_ok
  Uint32 cflags = 0;
  int result;

  ASSERT_EXACT (width,  1);
  ASSERT_EXACT (height, 2);
  ASSERT_EXACT (bpp,    3);

  if (BOUNDP (flags))
    cflags = GSDL_FLAGS2ULONG (flags, gsdl_video_flags, 4);

  result = SDL_VideoModeOK (C_LONG (width), C_LONG (height),
                            C_LONG (bpp), cflags);
  return result ? NUM_LONG (result) : BOOL_FALSE;
#undef FUNC_NAME
}


PRIMPROC
(set_video_mode, "set-video-mode", 3, 1, 0,
 (SCM width, SCM height, SCM bpp, SCM flags),
 doc: /***********
Set the SDL video mode with @var{width},
@var{height} and bits-per-pixel @var{bpp}.  Optional arg
@var{flags} (see @code{flagstash:video}) is supported.
Return a new surface.  */)
{
#define FUNC_NAME s_set_video_mode
  Uint32 cflags = 0;

  ASSERT_EXACT (width,  1);
  ASSERT_EXACT (height, 2);
  ASSERT_EXACT (bpp,    3);

  if (BOUNDP (flags))
    cflags = GSDL_FLAGS2ULONG (flags, gsdl_video_flags, 4);

  RETURN_NEW_SURFACE
    (SDL_SetVideoMode (C_LONG (width), C_LONG (height),
                       C_LONG (bpp), cflags));
#undef FUNC_NAME
}


PRIMPROC
(update_rect, "update-rect", 2, 3, 0,
 (SCM surface, SCM x, SCM y, SCM w, SCM h),
 doc: /***********
Update @var{surface} within a specified rectangle.
The second arg can either be an SDL-Rect object, or
the second through fifth args are numbers specifying
the x, y, width and height of a rectangular area.  */)
{
#define FUNC_NAME s_update_rect
  SDL_Rect *rect;
  Sint32 cx, cy, cw, ch;

  /* First arg is a surface.  */
  ASSERT_SURFACE (surface, 1);

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
      ASSERT_EXACT (x, 2);
      ASSERT_EXACT (y, 3);
      ASSERT_EXACT (w, 4);
      ASSERT_EXACT (h, 5);
      cx = C_LONG (x);
      cy = C_LONG (y);
      cw = C_LONG (w);
      ch = C_LONG (h);
    }

  SDL_UpdateRect (UNPACK_SURFACE (surface), cx, cy, cw, ch);
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


PRIMPROC
(update_rects, "update-rects", 2, 0, 0,
 (SCM surface, SCM ls),
 doc: /***********
On @var{surface}, update the rectangles in @var{ls},
a list of rectangles.  */)
{
#define FUNC_NAME s_update_rects
  SDL_Surface *csurface;
  SDL_Rect *rect;
  SCM p;

  ASSERT_SURFACE (surface, 1);
  ASSERT_LIST (ls, 2);
  for (p = ls; ! NULLP (p); p = CDR (p))
    ASSERT_RECT (CAR (p), 2);

  csurface = UNPACK_SURFACE (surface);
  for (p = ls; ! NULLP (p); p = CDR (p))
    {
      rect = UNPACK_RECT (CAR (p));
      SDL_UpdateRect (csurface, rect->x, rect->y, rect->w, rect->h);
    }
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


PRIMPROC
(flip, "flip", 0, 1, 0,
 (SCM surface),
 doc: /***********
Swap double buffers of the default surface,
or of @var{surface} if specified.  */)
{
#define FUNC_NAME s_flip
  SDL_Surface *csurface;

  if (BOUNDP (surface))
    {
      ASSERT_SURFACE (surface, 1);
      csurface = UNPACK_SURFACE (surface);
    }
  else
    csurface = SDL_GetVideoSurface ();

  SDL_Flip (csurface);
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


PRIMPROC
(set_colors, "set-colors!", 2, 0, 0,
 (SCM surface, SCM colors),
 doc: /***********
Set a portion of the colormap for the 8-bit @var{surface}
using @var{colors}, a vector of SDL-Colors.  */)
{
#define FUNC_NAME s_set_colors
  SDL_Color *ccolors;
  SDL_Color *color;
  int i, length, result = 0;

  ASSERT_SURFACE (surface, 1);
  ASSERT_VECTOR (colors, 2);

  length = VECLENGTH (colors);
  if ((ccolors = (SDL_Color*) scm_must_malloc (length, FUNC_NAME)))
    {
      for (i = 0; i < length; i++)
        {
          color = UNPACK_COLOR (scm_vector_ref (colors, NUM_LONG (i)));
          ccolors[i] = *color;
        }

      result = SDL_SetColors (UNPACK_SURFACE (surface),
                              ccolors, 0, length);
      scm_must_free (ccolors);
    }

  RETURN_BOOL
    (result);
#undef FUNC_NAME
}


PRIMPROC
(set_palette, "set-palette", 3, 0, 0,
 (SCM surface, SCM flags, SCM colors),
 doc: /***********
Set the palette of an 8-bit @var{surface}
using @var{flags} (see @code{flagstash:palette}) and
@var{colors}, a vector of SDL-Colors.  */)
{
#define FUNC_NAME s_set_palette
  SDL_Color *ccolors;
  SDL_Color *color;
  int cflags, i, length, result = 0;

  ASSERT_SURFACE (surface, 1);
  ASSERT_VECTOR (colors, 3);

  cflags   = GSDL_FLAGS2ULONG (flags, gsdl_palette_flags, 2);
  length   = VECLENGTH (colors);
  if ((ccolors  = (SDL_Color*) scm_must_malloc (length, FUNC_NAME)))
    {
      for (i = 0; i < length; i++)
        {
          color = UNPACK_COLOR (scm_vector_ref (colors, NUM_LONG (i)));
          ccolors[i] = *color;
        }

      result = SDL_SetPalette (UNPACK_SURFACE (surface),
                               cflags, ccolors, 0, length);
      scm_must_free (ccolors);
    }

  RETURN_BOOL
    (result);
#undef FUNC_NAME
}


PRIMPROC
(set_gamma, "set-gamma", 3, 0, 0,
 (SCM redgamma, SCM greengamma, SCM bluegamma),
 doc: /***********
Set the color gamma function for the display
using real numbers @var{redgamma}, @var{greengamma}
and @var{bluegamma}.  */)
{
#define FUNC_NAME s_set_gamma
  ASSERT_NUMBER (redgamma,   1);
  ASSERT_NUMBER (greengamma, 2);
  ASSERT_NUMBER (bluegamma,  3);

  RETURN_TRUE_IF_0
    (SDL_SetGamma ((float) SCM_REAL_VALUE (redgamma),
                   (float) SCM_REAL_VALUE (greengamma),
                   (float) SCM_REAL_VALUE (bluegamma)));
#undef FUNC_NAME
}


DECLARE_SIMPLE_SYM (redtable);
DECLARE_SIMPLE_SYM (greentable);
DECLARE_SIMPLE_SYM (bluetable);

#define GAMMAVEC(x)  (gsdl_scm_from_uint16s (x, GAMMA_TABLE_SIZE))

PRIMPROC
(get_gamma_ramp, "get-gamma-ramp", 0, 0, 0,
 (void),
 doc: /***********
Get the gamma translation lookup tables currently used
by the display.  Each table is a vector of 256 integer values.
Return an alist with keys @code{redtable}, @code{greentable}
and @code{bluetable}, and values the corresponding vectors.
Return #f if unsuccessful.  */)
{
#define FUNC_NAME s_get_gamma_ramp
  Uint16 rt[GAMMA_TABLE_SIZE], gt[GAMMA_TABLE_SIZE], bt[GAMMA_TABLE_SIZE];

  if (SDL_GetGammaRamp (rt, gt, bt) == -1)
    RETURN_FALSE;

  RETURN_LIST3 (CONS (SYM (redtable),   GAMMAVEC (rt)),
                CONS (SYM (greentable), GAMMAVEC (gt)),
                CONS (SYM (bluetable),  GAMMAVEC (bt)));
#undef FUNC_NAME
}


#define ASSERT_VSZFIT(v,which)                          \
  ASSERT_VECTOR (v, which);                             \
  SCM_ASSERT (VECLENGTH (v) == GAMMA_TABLE_SIZE,        \
              v, which, FUNC_NAME)

PRIMPROC
(set_gamma_ramp, "set-gamma-ramp", 3, 0, 0,
 (SCM redtable, SCM greentable, SCM bluetable),
 doc: /***********
Set the gamma translation lookup tables currently
used by the display, for @var{redtable}, @var{greentable}
and @var{bluetable}.  Each table is an vector of 256
integer values.  Return #t if successful.  */)
{
#define FUNC_NAME s_get_gamma_ramp
  Uint16 rt[GAMMA_TABLE_SIZE], gt[GAMMA_TABLE_SIZE], bt[GAMMA_TABLE_SIZE];

  ASSERT_VSZFIT (redtable,   1);
  ASSERT_VSZFIT (greentable, 2);
  ASSERT_VSZFIT (bluetable,  3);

  RETURN_TRUE_IF_0
    (SDL_SetGammaRamp (gsdl_scm_to_uint16s (redtable,   rt),
                       gsdl_scm_to_uint16s (greentable, gt),
                       gsdl_scm_to_uint16s (bluetable,  bt)));
#undef FUNC_NAME
}


PRIMPROC
(map_rgb, "map-rgb", 2, 2, 0,
 (SCM format, SCM r, SCM g, SCM b),
 doc: /***********
Map a RGB color value to the pixel @var{format}.
The second arg can be an SDL-Color, otherwise the second
through fourth args are red, green and blue values (numbers).
Return the mapped components as an unsigned integer.  */)
{
#define FUNC_NAME s_map_rgb
  Uint8 cr, cg, cb;

  ASSERT_PIXEL_FORMAT (format, 1);

  if (COLOR_P (r))
    {
      SDL_Color *color = UNPACK_COLOR (r);
      cr = color->r;
      cg = color->g;
      cb = color->b;
    }
  else
    {
      ASSERT_EXACT (r, 2);
      ASSERT_EXACT (g, 3);
      ASSERT_EXACT (b, 4);
      cr = C_ULONG (r);
      cg = C_ULONG (g);
      cb = C_ULONG (b);
    }

  RETURN_UINT (SDL_MapRGB (UNPACK_PIXEL_FORMAT (format),
                           cr, cg, cb));
#undef FUNC_NAME
}


PRIMPROC
(map_rgba, "map-rgba", 3, 2, 0,
 (SCM format, SCM r, SCM g, SCM b, SCM a),
 doc: /***********
Map a RGB color value to the pixel @var{format}.
If the second arg is an SDL-Color, the third is an alpha
value (number).  Otherwise, the second through fifth args
are red, green, blue and alpha values (numbers).
Return the mapped components as an unsigned integer.  */)
{
#define FUNC_NAME s_map_rgba
  Uint8 cr, cg, cb, ca;

  ASSERT_PIXEL_FORMAT (format, 1);

  if (COLOR_P (r))
    {
      SDL_Color *color = UNPACK_COLOR (r);
      cr = color->r;
      cg = color->g;
      cb = color->b;
      ASSERT_EXACT (g, 3);
      ca = C_ULONG (g);
    }
  else
    {
      ASSERT_EXACT (r, 2);
      ASSERT_EXACT (g, 3);
      ASSERT_EXACT (b, 4);
      ASSERT_EXACT (a, 5);
      cr = C_ULONG (r);
      cg = C_ULONG (g);
      cb = C_ULONG (b);
      ca = C_ULONG (a);
    }

  RETURN_UINT (SDL_MapRGBA (UNPACK_PIXEL_FORMAT (format),
                            cr, cg, cb, ca));
#undef FUNC_NAME
}


DECLARE_SIMPLE_SYM (r);
DECLARE_SIMPLE_SYM (g);
DECLARE_SIMPLE_SYM (b);
DECLARE_SIMPLE_SYM (a);

PRIMPROC
(get_rgb, "get-rgb", 2, 0, 0,
 (SCM pixel,
  SCM format),
 doc: /***********
Get RGB values from @var{pixel} in the specified pixel
@var{format}.  Return an alist with keys @code{r}, @code{g}
and @code{b}, with red, green and blue values (numbers),
respectively.  */)
{
#define FUNC_NAME s_get_rgb
  Uint8 r, g, b;

  ASSERT_EXACT (pixel, 1);
  ASSERT_PIXEL_FORMAT (format, 2);

  SDL_GetRGB (C_ULONG (pixel), UNPACK_PIXEL_FORMAT (format),
              &r, &g, &b);

  RETURN_LIST3 (CONS (SYM (r), NUM_ULONG (r)),
                CONS (SYM (g), NUM_ULONG (g)),
                CONS (SYM (b), NUM_ULONG (b)));
#undef FUNC_NAME
}


PRIMPROC
(get_rgba, "get-rgba", 2, 0, 0,
 (SCM pixel, SCM format),
 doc: /***********
Get RGBA values from @var{pixel} in the specified pixel
@var{format}.  Return an alist with keys @code{r}, @code{g},
@code{b} and @code{a}, with red, green, blue and alpha values
(numbers), respectively.  */)
{
#define FUNC_NAME s_get_rgba
  Uint8 r, g, b, a;

  ASSERT_EXACT (pixel, 1);
  ASSERT_PIXEL_FORMAT (format, 2);

  SDL_GetRGBA (C_ULONG (pixel), UNPACK_PIXEL_FORMAT (format),
               &r, &g, &b, &a);

  RETURN_LIST4 (CONS (SYM (r), NUM_ULONG (r)),
                CONS (SYM (g), NUM_ULONG (g)),
                CONS (SYM (b), NUM_ULONG (b)),
                CONS (SYM (a), NUM_ULONG (a)));
#undef FUNC_NAME
}


PRIMPROC
(fill_rect, "fill-rect", 3, 0, 0,
 (SCM surface, SCM rect, SCM color),
 doc: /***********
Fill @var{surface} @var{rect} with @var{color} (a number).
If @var{rect} is #f, fill the entire surface.
Return #t if successful.  */)
{
#define FUNC_NAME s_fill_rect
  SDL_Rect *crect = NULL;

  ASSERT_SURFACE (surface, 1);
  if (! EXACTLY_FALSEP (rect))
    {
      ASSERT_RECT (rect, 2);
      crect = UNPACK_RECT (rect);
    }
  ASSERT_EXACT (color, 3);

  RETURN_TRUE_IF_0
    (SDL_FillRect (UNPACK_SURFACE (surface), crect, C_ULONG (color)));
#undef FUNC_NAME
}


PRIMPROC
(display_format, "display-format", 1, 0, 0,
 (SCM surface),
 doc: /***********
Return a new surface made by converting @var{surface}
to the display format.  Return #f if not successful.  */)
{
#define FUNC_NAME s_display_format
  SDL_Surface *csurface;

  ASSERT_SURFACE (surface, 1);

  csurface = SDL_DisplayFormat (UNPACK_SURFACE (surface));

  if (! csurface)
    RETURN_FALSE;

  RETURN_NEW_SURFACE (csurface);
#undef FUNC_NAME
}


PRIMPROC
(display_format_alpha, "display-format-alpha", 1, 0, 0,
 (SCM surface),
 doc: /***********
Return a new surface made by converting @var{surface}
to the display format, with an alpha channel.  Return #f
if not successful.  */)
{
#define FUNC_NAME s_display_format_alpha
  SDL_Surface *csurface;

  ASSERT_SURFACE (surface, 1);

  csurface = SDL_DisplayFormatAlpha (UNPACK_SURFACE (surface));

  if (! csurface)
    RETURN_FALSE;

  RETURN_NEW_SURFACE (csurface);
#undef FUNC_NAME
}


PRIMPROC
(warp_mouse, "warp-mouse", 2, 0, 0,
 (SCM x, SCM y),
 doc: /***********
Set the position of the mouse cursor to @var{x},@var{y}.  */)
{
#define FUNC_NAME s_warp_mouse
  ASSERT_EXACT (x, 1);
  ASSERT_EXACT (y, 2);

  SDL_WarpMouse (C_ULONG (x), C_ULONG (y));
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


PRIMPROC
(set_cursor, "set-cursor", 1, 0, 0,
 (SCM cursor),
 doc: /***********
Set the current mouse cursor to @var{cursor}.  */)
{
#define FUNC_NAME s_set_cursor
  ASSERT_CURSOR (cursor, 1);
  SDL_SetCursor (UNPACK_CURSOR (cursor)->c);
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


PRIMPROC
(get_cursor, "get-cursor", 0, 0, 0,
 (void),
 doc: /***********
Get the current mouse cursor.  */)
{
#define FUNC_NAME s_get_cursor
  xSDL_Cursor *cursor;

  if ((cursor = MALLOC_XSDL_CURSOR (FUNC_NAME)))
    {
      cursor->freeable = 0;
      cursor->c = SDL_GetCursor ();
    }

  RETURN_NEW_CURSOR (cursor);
#undef FUNC_NAME
}


PRIMPROC
(show_cursor, "show-cursor", 0, 1, 0,
 (SCM query),
 doc: /***********
Toggle the visibility of the mouse cursor.
Return #t if was being displayed before the call,
and #f if not.  Optional arg @var{query} non-#f
means to return the current state without toggling.  */)
{
#define FUNC_NAME s_show_cursor
  UNBOUND_MEANS_FALSE (query);
  RETURN_BOOL (SDL_ShowCursor (EXACTLY_FALSEP (query) - 1));
#undef FUNC_NAME
}


PRIMPROC
(gl_get_attribute, "gl-get-attribute", 1, 0, 0,
 (SCM attribute),
 doc: /***********
Return the value of a special SDL/OpenGL @var{attribute}.  */)
{
#define FUNC_NAME s_gl_get_attribute
  int value;

  ASSERT_EXACT (attribute, 1);

  SDL_GL_GetAttribute ((SDL_GLattr) C_LONG (attribute), &value);
  RETURN_INT (value);
#undef FUNC_NAME
}


PRIMPROC
(gl_set_attribute, "gl-set-attribute", 2, 0, 0,
 (SCM attribute,
  SCM value),
 doc: /***********
Set the special SDL/OpenGL @var{attribute} to @var{value}.
Both args are numbers.  */)
{
#define FUNC_NAME s_gl_set_attribute
  ASSERT_EXACT (attribute, 1);
  ASSERT_EXACT (value, 2);

  SDL_GL_SetAttribute ((SDL_GLattr) C_LONG (attribute),
                       (int) C_LONG (value));
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


PRIMPROC
(gl_swap_buffers, "gl-swap-buffers", 0, 0, 0,
 (void),
 doc: /***********
Swap OpenGL framebuffers/Update Display.  */)
{
#define FUNC_NAME s_gl_swap_buffers
  SDL_GL_SwapBuffers ();
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


PRIMPROC
(lock_yuv_overlay, "lock-yuv-overlay", 1, 0, 0,
 (SCM overlay),
 doc: /***********
Lock the given YUV @var{overlay}.
Return #f if successful.  */)
{
#define FUNC_NAME s_lock_yuv_overlay
  ASSERT_OVERLAY (overlay, 1);

  RETURN_TRUE_IF_0
    (SDL_LockYUVOverlay (UNPACK_OVERLAY (overlay)));
#undef FUNC_NAME
}


PRIMPROC
(unlock_yuv_overlay, "unlock-yuv-overlay", 1, 0, 0,
 (SCM overlay),
 doc: /***********
Unlock the previously locked YUV @var{overlay}.  */)
{
#define FUNC_NAME s_unlock_yuv_overlay
  ASSERT_OVERLAY (overlay, 1);

  SDL_UnlockYUVOverlay (UNPACK_OVERLAY (overlay));
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


PRIMPROC
(display_yuv_overlay, "display-yuv-overlay", 2, 0, 0,
 (SCM overlay, SCM dstrect),
 doc: /***********
Blit the YUV @var{overlay} to the display @var{dstrect}
over which it was created.  Return #t if successful.  */)
{
#define FUNC_NAME s_display_yuv_overlay
  ASSERT_OVERLAY (overlay, 1);
  ASSERT_RECT (dstrect, 2);

  RETURN_TRUE_IF_0
    (SDL_DisplayYUVOverlay (UNPACK_OVERLAY (overlay),
                            UNPACK_RECT (dstrect)));
#undef FUNC_NAME
}


/* window manager functions */

PRIMPROC
(wm_set_caption, "set-caption", 1, 1, 0,
 (SCM title, SCM icon),
 doc: /***********
Set the title-bar and icon name of the display window
to @var{title} and @var{icon} (both strings), respectively.
If @var{icon} is not specified, use @var{title} by default.  */)
{
#define FUNC_NAME s_wm_set_caption
  char *ctitle, *cicon;

  ASSERT_STRING (title, 1);

  ctitle = SCM_CHARS (title);

  if (UNBOUNDP (icon))
    cicon = ctitle;
  else
    {
      ASSERT_STRING (icon, 2);
      cicon = SCM_CHARS (icon);
    }

  SDL_WM_SetCaption (ctitle, cicon);

  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


DECLARE_SIMPLE_SYM (title);
DECLARE_SIMPLE_SYM (icon);

PRIMPROC
(wm_get_caption, "get-caption", 0, 0, 0,
 (void),
 doc: /***********
Return an alist with keys @code{title} and @code{icon}
and values the title-bar and icon name of the display
window, respectively.  */)
{
#define FUNC_NAME s_wm_get_caption
  char *title, *icon;

  SDL_WM_GetCaption (&title, &icon);
  RETURN_LIST2 (CONS (SYM (title), STRING (title)),
                CONS (SYM (icon),  STRING (icon)));
#undef FUNC_NAME
}


PRIMPROC
(wm_set_icon, "set-icon", 1, 0, 0,
 (SCM icon),
 doc: /***********
Set @var{icon} for the display window.  */)
{
#define FUNC_NAME s_wm_set_icon
  ASSERT_SURFACE (icon, 1);

  /* Set w/ a NULL mask for now.  */
  SDL_WM_SetIcon (UNPACK_SURFACE (icon), NULL);
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


PRIMPROC
(wm_iconify_window, "iconify-window", 0, 0, 0,
 (void),
 doc: /***********
Iconify/Minimize the window.
Return #t if successful.  */)
{
#define FUNC_NAME s_wm_iconify_window
  RETURN_BOOL
    (SDL_WM_IconifyWindow ());
#undef FUNC_NAME
}


PRIMPROC
(wm_toggle_full_screen, "toggle-full-screen", 0, 1, 0,
 (SCM surface),
 doc: /***********
Toggle the default video surface between windowed
and fullscreen mode, if supported.  Optional arg
@var{surface} specifies another surface to toggle.
Return #t if successful.  */)
{
#define FUNC_NAME s_wm_toggle_full_screen
  SDL_Surface *csurface;

  if (UNBOUNDP (surface))
    csurface = SDL_GetVideoSurface ();
  else
    {
      ASSERT_SURFACE (surface, 1);
      csurface = UNPACK_SURFACE (surface);
    }

  RETURN_BOOL
    (SDL_WM_ToggleFullScreen (csurface));
#undef FUNC_NAME
}


DECLARE_SIMPLE_SYM (query);
DECLARE_SIMPLE_SYM (off);
DECLARE_SIMPLE_SYM (on);

PRIMPROC
(wm_grab_input, "grab-input", 0, 1, 0,
 (SCM mode),
 doc: /***********
Grab mouse and keyboard input.  Return new grab state.
Optional arg @var{mode} (a symbol) specifies the kind
of grab, one of @code{query} (the default),
@code{off} or @code{on}.

Compatibility Note: Presently, @var{mode} can also be an
integer, one of -1, 0 or 1.  Starting with Guile-SDL 0.5.0
an integer @var{mode} will result in a wrong-type-arg error.  */)
{
#define FUNC_NAME s_wm_grab_input
  if (UNBOUNDP (mode))
    mode = SYM (query);

  if (NOT_FALSEP (scm_exact_p (mode)))
    switch (C_LONG (mode))
      {
      case -1: mode = SYM (query); break;
      case  0: mode = SYM (off);   break;
      case  1: mode = SYM (on);    break;
      default:
        scm_misc_error (FUNC_NAME, "bad mode: ~S", CONS (mode, SCM_EOL));
      }

  ASSERT_SYMBOL (mode, 1);
  if (! (EQ (mode, SYM (query)) ||
         EQ (mode, SYM (off)) ||
         EQ (mode, SYM (on))))
    scm_misc_error (FUNC_NAME, "bad mode: ~S", CONS (mode, SCM_EOL));

  return (SDL_GRAB_ON == SDL_WM_GrabInput (EQ (mode, SYM (query))
                                           ? SDL_GRAB_QUERY
                                           : (EQ (mode, SYM (on))
                                              ? SDL_GRAB_ON
                                              : SDL_GRAB_OFF))
          ? SYM (on)
          : SYM (off));
#undef FUNC_NAME
}



#include "video.c"
#include "palette.c"
#include "overlay.c"

void
gsdl_init_video (void)
{
  cursor_tag = scm_make_smob_type ("SDL-Cursor", sizeof (xSDL_Cursor));
  scm_set_smob_free (cursor_tag, free_cursor);

  pixel_format_tag = scm_make_smob_type ("SDL-Pixel-Format",
                                         sizeof (SDL_PixelFormat));
  scm_set_smob_free (pixel_format_tag, free_pixel_format);
  scm_set_smob_print (pixel_format_tag, print_pixel_format);

  overlay_tag = scm_make_smob_type ("SDL-Overlay", sizeof (SDL_Overlay));
  scm_set_smob_free (overlay_tag, free_yuv_overlay);

  /* alpha constants */
  gsdl_alpha_enums = DEFINE_ENUM ("alpha-enums", alpha_eback);

  /* video flags */
  gsdl_video_flags = gsdl_make_flagstash (&vid_flagstash);

  /* palette flags */
  gsdl_palette_flags = gsdl_make_flagstash (&pal_flagstash);

  /* yuv overlay formats */
  gsdl_overlay_formats = gsdl_make_flagstash (&ov_flagstash);

  /* GL constants */
  gl_enums = DEFINE_ENUM ("gl-enums", gl_eback);

#include "sdlvideo.x"
}

/* sdlvideo.c ends here */
