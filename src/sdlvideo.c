/* sdlvideo.c --- SDL Video functions for Guile
 *
 * Copyright (C) 2003, 2004, 2005, 2007, 2009,
 *   2011, 2012, 2013 Thien-Thi Nguyen
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
#include "SDL_image.h"
#include "snuggle/finangle.h"
#include "b-uv.h"
#include "b-values.h"

#if ! GI_LEVEL_1_8
IMPORT_SRFI4 ();
SELECT_MODULE_VAR (mk_u16v, srfi4, "make-u16vector");
SELECT_MODULE_VAR (u16v_x, srfi4, "u16vector-set!");
SELECT_UVEC_PREDICATE (u8);
SELECT_UVEC_PREDICATE (u16);
#define scm_u8vector_p(obj)           CALL1 (u8v_p, obj)
#define scm_u16vector_p(obj)          CALL1 (u16v_p, obj)
#define scm_make_u16vector(len,fill)  CALL2 (mk_u16v, len, fill)
#endif

DEFINE_STRUCT_AND_COPY_FUNC (u8, Uint8)
#define ASSERT_UVEC_U8(obj,n)   ASSERT_UVEC (u8,  obj, n)
#define U8_STUFF(v)             STUFF (u8, v)
#define GET_U8_PARTICULARS(v)   GET_PARTICULARS (u8, v)
#define HOWDY_U8(v)             HOWDY (u8, Uint8, v)

DEFINE_STRUCT_AND_COPY_FUNC (u16, Uint16)
#define ASSERT_UVEC_U16(obj,n)  ASSERT_UVEC (u16, obj, n)
#define U16_STUFF(v)            STUFF (u16, v)
#define GET_U16_PARTICULARS(v)  GET_PARTICULARS (u16, v)
#define HOWDY_U16(v)            HOWDY (u16, Uint16, v)


#define MAX_DRIVER_LEN    100
#define GAMMA_TABLE_SIZE  256

#define COLOR_P(x) \
  (SCM_SMOB_PREDICATE (color_tag, x))

#define RECT_P(x) \
  (SCM_SMOB_PREDICATE (rect_tag, x))


static SCM palette_flags;
static SCM gsdl_overlay_formats;
static SCM grab_modes;


/* extended SDL_* structures */

#define cursor_nick "SDL-Cursor"

DECLARE_PF (Cursor);


/* tags for SDL smobs */
static smob_tag_t cursor_tag;
static smob_tag_t overlay_tag;
#define overlay_nick "SDL-Overlay"

#define ASSERT_CURSOR(obj,which)   ASSERT_SMOB (obj, cursor, which)
#define ASSERT_OVERLAY(obj,which)  ASSERT_SMOB (obj, overlay, which)

#define UNPACK_PF_CURSOR(smob)    (SMOBGET (smob, PF_Cursor *))
#define UNPACK_CURSOR(smob)       (UNPACK_PF_CURSOR (smob)->object)
#define UNPACK_OVERLAY(smob)      (SMOBGET (smob, SDL_Overlay *))

#define RETURN_PF_CURSOR(x,REFP)                        \
  RETURN_NEW_PF_OR_FALSE (Cursor, cursor, REFP, x)

#define RETURN_NEW_CURSOR(x)    RETURN_PF_CURSOR (x, false)
#define RETURN_INT_CURSOR(x)    RETURN_PF_CURSOR (x, true)
#define RETURN_NEW_OVERLAY(x)   NEWSMOB_OR_FALSE (overlay_tag, x)

/* smob functions */

static
size_t
free_cursor (SCM cursor)
{
  PF_Cursor *pf = UNPACK_PF_CURSOR (cursor);

  if (! pf->internalp)
    SDL_FreeCursor (pf->object);
  GCFREE (pf, cursor_nick);
  return GCRV (pf);
}

static
size_t
free_yuv_overlay (SCM overlay)
{
  SDL_FreeYUVOverlay (UNPACK_OVERLAY (overlay));
  return 0;
}

#define pixel_format_nick "SDL-Pixel-Format"

static
size_t
free_pixel_format (UNUSED SCM pixel_format)
{
  /* Always part of a surface.  */
  return 0;
}

static
int
print_pixel_format (SCM pixel_format, SCM port, UNUSED scm_print_state *pstate)
{
  SDL_PixelFormat *f = UNPACK_PIXEL_FORMAT (pixel_format);
  char buf[80];

  snprintf (buf, 80, "#<%s %d %d %x %d %s%s%s%s>", pixel_format_nick,
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
(both u8 uniform vectors), sized @var{w} by @var{h}
and with hot pixel located at @var{x},@var{y}.  */)
{
#define FUNC_NAME s_create_cursor
  U8_STUFF (data);
  U8_STUFF (mask);
  SDL_Cursor *rv;

  ASSERT_UVEC_U8 (data, 1);
  ASSERT_UVEC_U8 (mask, 2);
  ASSERT_INTEGER (w, 3);
  ASSERT_INTEGER (h, 4);
  ASSERT_INTEGER (x, 5);
  ASSERT_INTEGER (y, 6);

  HOWDY_U8 (data);
  HOWDY_U8 (mask);
  rv = SDL_CreateCursor (VBITS (data),
                         VBITS (mask),
                         C_LONG (w), C_LONG (h),
                         C_LONG (x), C_LONG (y));
  LATER (data);
  LATER (mask);
  RETURN_NEW_CURSOR (rv);
#undef FUNC_NAME
}


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
  DECLINIT_SYM2NUM_CC (3, gsdl_overlay_formats);
  Uint32 cformat;
  SDL_Surface *cdisplay;

  ASSERT_INTEGER (width, 1);
  ASSERT_INTEGER (height, 2);

  if (SYMBOLP (format))
    cformat = FLAGS2ULONG (3, format);
  else
    ASSERT_ULONG_COPY (format, 3);

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
  RETURN_INT_SURFACE (SDL_GetVideoSurface ());
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

PRIMPROC
(video_cmf, "video-cmf", 0, 0, 0,
 (void),
 doc: /***********
Return information about the video hardware as three values:
@code{capabilities} (list of symbols), @code{memory} (integer),
and @code{format} (pixel format object).  The @code{capabilities} are:

@example
hw-available
wm-available
blit-hw   blit-hw-CC   blit-hw-A
blit-sw   blit-sw-CC   blit-sw-A
blit-fill
@end example  */)
{
#define FUNC_NAME s_video_cmf
  SCM cap = SCM_EOL;
  const SDL_VideoInfo *info = SDL_GetVideoInfo ();
  SCM format;

  SCM_NEWSMOB (format, pixel_format_tag, info->vfmt);

#define TRY(bit)  if (info->bit) cap = CONS (SYM (bit), cap)

  TRY (blit_fill);
  TRY (blit_sw_A);
  TRY (blit_sw_CC);
  TRY (blit_sw);
  TRY (blit_hw_A);
  TRY (blit_hw_CC);
  TRY (blit_hw);
  TRY (wm_available);
  TRY (hw_available);

#undef TRY

  RETURN_VALUES3
    (cap,
     NUM_ULONG (info->video_mem),
     format);
#undef FUNC_NAME
}


DECLARE_SYM (video_mem,    "video-mem");
DECLARE_SIMPLE_SYM (vfmt);

PRIMPROC
(get_video_info, "get-video-info", 0, 0, 0,
 (void),
 doc: /***********
NB: This procedure is obsoleted by @code{video-cmf}
and @strong{will be removed} after 2013-12-31.

Return information about the video hardware as an alist.
Keys are: @code{hw-available}, @code{wm-available},
@code{bit-hw}, @code{blit-hw-CC}, @code{blit-hw-A},
@code{blit-sw}, @code{blit-sw-CC}, @code{blit-sw-A},
@code{blit-fill}, @code{video-mem} and @code{vfmt}.  */)
{
#define FUNC_NAME s_get_video_info
  const SDL_VideoInfo *info = SDL_GetVideoInfo ();
  SCM format;
  SCM rv = SCM_EOL;

  SCM_NEWSMOB (format, pixel_format_tag, info->vfmt);

#define PUSHPAIR(a,b)  rv = CONS (CONS (SYM (a), b), rv)

  PUSHPAIR (vfmt,         format);
  PUSHPAIR (video_mem,    NUM_ULONG (info->video_mem));
  PUSHPAIR (blit_fill,    BOOLEAN (info->blit_fill));
  PUSHPAIR (blit_sw_A,    BOOLEAN (info->blit_sw_A));
  PUSHPAIR (blit_sw_CC,   BOOLEAN (info->blit_sw_CC));
  PUSHPAIR (blit_sw,      BOOLEAN (info->blit_sw));
  PUSHPAIR (blit_hw_A,    BOOLEAN (info->blit_hw_A));
  PUSHPAIR (blit_hw_CC,   BOOLEAN (info->blit_hw_CC));
  PUSHPAIR (blit_hw,      BOOLEAN (info->blit_hw));
  PUSHPAIR (wm_available, BOOLEAN (info->wm_available));
  PUSHPAIR (hw_available, BOOLEAN (info->hw_available));

#undef PUSHPAIR

  return rv;
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
@var{format} and @var{flags} (@pxref{video flags}).
Format defaults to that for
the current screen.  Flags default to none.
Return @code{#f} if no modes are available, @code{#t} if all are available.  */)
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
      DECLINIT_SYM2NUM_CC (2, btw->video_flags);

      ASSERT_INTEGER (flags, 2);
      cflags = FLAGS2ULONG (2, flags);
    }

  modes = SDL_ListModes (cformat, cflags);

  if (modes == (SDL_Rect**)0)
    /* Return ‘#f’ to signify no resolutions are available.  */
    result = BOOL_FALSE;
  else if (modes == (SDL_Rect**)-1)
    /* Return ‘#t’ to signify all resolutions are available.  */
    result = BOOL_TRUE;
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
and @var{flags} (@pxref{video flags}).
Return @code{#f} if the mode is not supported, or a number
indicating the bits-per-pixel of the closest available
mode supporting @var{width} and @var{height}.  */)
{
#define FUNC_NAME s_video_mode_ok
  DECLINIT_SYM2NUM_CC (4, btw->video_flags);
  Uint32 cflags = 0;
  int result;

  ASSERT_INTEGER (width,  1);
  ASSERT_INTEGER (height, 2);
  ASSERT_INTEGER (bpp,    3);

  if (BOUNDP (flags))
    cflags = FLAGS2ULONG (4, flags);

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
@var{flags} (@pxref{video flags}) is supported.
Return a new surface.  */)
{
#define FUNC_NAME s_set_video_mode
  DECLINIT_SYM2NUM_CC (4, btw->video_flags);
  Uint32 cflags = 0;

  ASSERT_INTEGER (width,  1);
  ASSERT_INTEGER (height, 2);
  ASSERT_INTEGER (bpp,    3);

  if (BOUNDP (flags))
    cflags = FLAGS2ULONG (4, flags);

  RETURN_INT_SURFACE
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
      ASSERT_LONG_COPY (x, 2);
      ASSERT_LONG_COPY (y, 3);
      ASSERT_LONG_COPY (w, 4);
      ASSERT_LONG_COPY (h, 5);
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


static void
assert_colormap_copy (const char *FUNC_NAME, SCM v, int pos,
                      int *len, SDL_Color *cv)
{
  if (! VECTORP (v)
      || 256 < (*len = VECTOR_LEN (v)))
    *len = -1;
  else
    {
      int i;

      for (i = 0; i < *len; i++)
        {
          SCM elem = VECTOR_REF (v, i);

          if (! COLOR_P (elem))
            {
              *len = -1;
              break;
            }
          *cv++ = *UNPACK_COLOR (elem);
        }
    }
  ASSERT_TYPE (-1 < *len, v, pos,
               "vector (length 256 or less) of SDL-Color");
}

#define ASSERT_COLORMAP_COPY(v,pos,len) \
  assert_colormap_copy (FUNC_NAME, v, pos, &len, c ## v)


PRIMPROC
(set_colors, "set-colors!", 2, 1, 0,
 (SCM surface, SCM colors, SCM start),
 doc: /***********
Set a portion of the colormap for the 8-bit @var{surface}
using @var{colors}, a vector of SDL-Colors.
Optional arg @var{start} (an integer in the range [0,255])
specifies the portion to be modified.  It defaults to 0.  */)
{
#define FUNC_NAME s_set_colors
  SDL_Color ccolors[256];
  int length, cstart = 0;

  ASSERT_SURFACE (surface, 1);
  ASSERT_COLORMAP_COPY (colors, 2, length);
  if (BOUNDP (start))
    {
      ASSERT_ULONG_COPY (start, 3);
      SCM_ASSERT_RANGE (4, start, cstart < 256);
    }

  RETURN_BOOL
    (SDL_SetColors (UNPACK_SURFACE (surface),
                    ccolors, cstart, length));
#undef FUNC_NAME
}


PRIMPROC
(set_palette, "set-palette", 3, 1, 0,
 (SCM surface, SCM flags, SCM colors, SCM start),
 doc: /***********
Set the palette of an 8-bit @var{surface}
using @var{flags} (@pxref{palette flags}) and
@var{colors}, a vector of SDL-Colors.
Optional arg @var{start} (an integer in the range [0,255])
specifies the portion to be modified.  It defaults to 0.  */)
{
#define FUNC_NAME s_set_palette
  DECLINIT_SYM2NUM_CC (2, palette_flags);
  SDL_Color ccolors[256];
  int cflags, length, cstart = 0;

  ASSERT_SURFACE (surface, 1);
  ASSERT_COLORMAP_COPY (colors, 3, length);
  if (BOUNDP (start))
    {
      ASSERT_ULONG_COPY (start, 4);
      SCM_ASSERT_RANGE (4, start, cstart < 256);
    }

  cflags   = FLAGS2ULONG (2, flags);
  RETURN_BOOL
    (SDL_SetPalette (UNPACK_SURFACE (surface),
                     cflags, ccolors, cstart, length));
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


PRIMPROC
(get_gamma_ramp, "get-gamma-ramp", 0, 0, 0,
 (void),
 doc: /***********
Return the gamma translation lookup tables currently used by
the display as a list of three tables, for red, green and blue.
Each table is a u16 uniform vector of length 256.
Return @code{#f} if unsuccessful.  */)
{
#define FUNC_NAME s_get_gamma_ramp
  DECLARE_UV_STRUCT (/* not const */, u16, Uint16);

  U16_STUFF (r); SCM r;
  U16_STUFF (g); SCM g;
  U16_STUFF (b); SCM b;
  SCM table_size = NUM_ULONG (GAMMA_TABLE_SIZE);
  int rv;

#define GREET(v)                                        \
  v = scm_make_u16vector (table_size, SCM_UNDEFINED);   \
  GET_WRITABLE_PARTICULARS (u16, v)
#define WRITABLE_VBITS(v)  ST (v, elt)

  GREET (r);
  GREET (g);
  GREET (b);
  rv = SDL_GetGammaRamp (WRITABLE_VBITS (r),
                         WRITABLE_VBITS (g),
                         WRITABLE_VBITS (b));
  LATER (r);
  LATER (g);
  LATER (b);
  if (0 > rv)
    return BOOL_FALSE;
  else
    return LIST3 (r, g, b);

#undef WRITABLE_VBITS
#undef GREET
#undef FUNC_NAME
}


PRIMPROC
(set_gamma_ramp, "set-gamma-ramp", 3, 0, 0,
 (SCM r, SCM g, SCM b),
 doc: /***********
Set the gamma translation lookup tables currently
used by the display to tables @var{r}, @var{g} and @var{b},
each a u16 uniform vector of length 256, or @code{#f},
in which case that particular component is unchanged.
Return @code{#t} if successful.  */)
{
#define FUNC_NAME s_set_gamma_ramp
#define STUFF_GIVEN(v)                          \
  U16_STUFF (v);                                \
  int v ## _given = NOT_FALSEP (v)
#define GREET(v)                                \
  if (v ##_given)                               \
    HOWDY_U16 (v);                              \
  else                                          \
    VBITS (v) = NULL

  STUFF_GIVEN (r);
  STUFF_GIVEN (g);
  STUFF_GIVEN (b);
  int rv;

  if (r_given) ASSERT_UVEC_U16 (r, 1);
  if (g_given) ASSERT_UVEC_U16 (g, 2);
  if (b_given) ASSERT_UVEC_U16 (b, 3);

  GREET (r);
  GREET (g);
  GREET (b);
  rv = SDL_SetGammaRamp (VBITS (r), VBITS (g), VBITS (b));
  LATER (r);
  LATER (g);
  LATER (b);
  RETURN_TRUE_IF_0 (rv);

#undef GREET
#undef STUFF_GIVEN
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
      ASSERT_ULONG_COPY (r, 2);
      ASSERT_ULONG_COPY (g, 3);
      ASSERT_ULONG_COPY (b, 4);
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
      ASSERT_INTEGER (g, 3);
      ca = C_ULONG (g);
    }
  else
    {
      ASSERT_ULONG_COPY (r, 2);
      ASSERT_ULONG_COPY (g, 3);
      ASSERT_ULONG_COPY (b, 4);
      ASSERT_ULONG_COPY (a, 5);
    }

  RETURN_UINT (SDL_MapRGBA (UNPACK_PIXEL_FORMAT (format),
                            cr, cg, cb, ca));
#undef FUNC_NAME
}


PRIMPROC
(pixel_rgb, "pixel-rgb", 2, 0, 0,
 (SCM pixel,
  SCM format),
 doc: /***********
Return RGB info from @var{pixel} in the specified pixel @var{format}
as three values: @code{r}, @code{g} and @code{b} (all integers).  */)
{
#define FUNC_NAME s_pixel_rgb
  Uint8 r, g, b;

  ASSERT_INTEGER (pixel, 1);
  ASSERT_PIXEL_FORMAT (format, 2);

  SDL_GetRGB (C_ULONG (pixel), UNPACK_PIXEL_FORMAT (format),
              &r, &g, &b);

  RETURN_VALUES3
    (NUM_ULONG (r),
     NUM_ULONG (g),
     NUM_ULONG (b));
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
NB: This procedure is obsoleted by @code{pixel-rgb}
and @strong{will be removed} after 2013-12-31.

Get RGB values from @var{pixel} in the specified pixel
@var{format}.  Return an alist with keys @code{r}, @code{g}
and @code{b}, with red, green and blue values (numbers),
respectively.  */)
{
#define FUNC_NAME s_get_rgb
  Uint8 r, g, b;

  ASSERT_INTEGER (pixel, 1);
  ASSERT_PIXEL_FORMAT (format, 2);

  SDL_GetRGB (C_ULONG (pixel), UNPACK_PIXEL_FORMAT (format),
              &r, &g, &b);

  return LIST3 (CONS (SYM (r), NUM_ULONG (r)),
                CONS (SYM (g), NUM_ULONG (g)),
                CONS (SYM (b), NUM_ULONG (b)));
#undef FUNC_NAME
}


PRIMPROC
(pixel_rgba, "pixel-rgba", 2, 0, 0,
 (SCM pixel, SCM format),
 doc: /***********
Return RGBA info from @var{pixel} in the specified pixel @var{format} as
four values: @code{r}, @code{g}, @code{b} and @code{a} (all integers).  */)
{
#define FUNC_NAME s_pixel_rgba
  Uint8 r, g, b, a;

  ASSERT_INTEGER (pixel, 1);
  ASSERT_PIXEL_FORMAT (format, 2);

  SDL_GetRGBA (C_ULONG (pixel), UNPACK_PIXEL_FORMAT (format),
               &r, &g, &b, &a);

  RETURN_VALUES4
    (NUM_ULONG (r),
     NUM_ULONG (g),
     NUM_ULONG (b),
     NUM_ULONG (a));
#undef FUNC_NAME
}


PRIMPROC
(get_rgba, "get-rgba", 2, 0, 0,
 (SCM pixel, SCM format),
 doc: /***********
NB: This procedure is obsoleted by @code{pixel-rgba}
and @strong{will be removed} after 2013-12-31.

Get RGBA values from @var{pixel} in the specified pixel
@var{format}.  Return an alist with keys @code{r}, @code{g},
@code{b} and @code{a}, with red, green, blue and alpha values
(numbers), respectively.  */)
{
#define FUNC_NAME s_get_rgba
  Uint8 r, g, b, a;

  ASSERT_INTEGER (pixel, 1);
  ASSERT_PIXEL_FORMAT (format, 2);

  SDL_GetRGBA (C_ULONG (pixel), UNPACK_PIXEL_FORMAT (format),
               &r, &g, &b, &a);

  return LIST4 (CONS (SYM (r), NUM_ULONG (r)),
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
If @var{rect} is @code{#f}, fill the entire surface.
Return @code{#t} if successful.  */)
{
#define FUNC_NAME s_fill_rect
  SDL_Rect *crect = NULL;

  ASSERT_SURFACE (surface, 1);
  if (! EXACTLY_FALSEP (rect))
    {
      ASSERT_RECT (rect, 2);
      crect = UNPACK_RECT (rect);
    }
  ASSERT_INTEGER (color, 3);

  RETURN_TRUE_IF_0
    (SDL_FillRect (UNPACK_SURFACE (surface), crect, C_ULONG (color)));
#undef FUNC_NAME
}


PRIMPROC
(display_format, "display-format", 1, 0, 0,
 (SCM surface),
 doc: /***********
Return a new surface made by converting @var{surface}
to the display format.  Return @code{#f} if not successful.  */)
{
#define FUNC_NAME s_display_format
  SDL_Surface *csurface;

  ASSERT_SURFACE (surface, 1);

  csurface = SDL_DisplayFormat (UNPACK_SURFACE (surface));

  if (! csurface)
    return BOOL_FALSE;

  RETURN_NEW_SURFACE (csurface);
#undef FUNC_NAME
}


PRIMPROC
(display_format_alpha, "display-format-alpha", 1, 0, 0,
 (SCM surface),
 doc: /***********
Return a new surface made by converting @var{surface}
to the display format, with an alpha channel.  Return @code{#f}
if not successful.  */)
{
#define FUNC_NAME s_display_format_alpha
  SDL_Surface *csurface;

  ASSERT_SURFACE (surface, 1);

  csurface = SDL_DisplayFormatAlpha (UNPACK_SURFACE (surface));

  if (! csurface)
    return BOOL_FALSE;

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
  ASSERT_INTEGER (x, 1);
  ASSERT_INTEGER (y, 2);

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
  SDL_SetCursor (UNPACK_CURSOR (cursor));
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
  RETURN_INT_CURSOR (SDL_GetCursor ());
#undef FUNC_NAME
}


PRIMPROC
(show_cursor, "show-cursor", 0, 1, 0,
 (SCM setting),
 doc: /***********
Return the current visibility of the pointer (aka ``mouse cursor'')
as a boolean.  If arg @var{setting} (a boolean) is specified, set
the visibility to @var{setting} (the returned visibility corresponds
to that before the call, regardless).  */)
{
#define FUNC_NAME s_show_cursor
  RETURN_BOOL (SDL_ShowCursor (BOUNDP (setting)
                               ? NOT_FALSEP (setting)
                               : -1));
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

  ASSERT_INTEGER (attribute, 1);

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
  ASSERT_INTEGER (attribute, 1);
  ASSERT_INTEGER (value, 2);

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
Return @code{#f} if successful.  */)
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
over which it was created.  Return @code{#t} if successful.  */)
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
  range_t ctitle, cicon;

  ASSERT_STRING (title, 1);
  if (UNBOUNDP (icon))
    icon = title;
  ASSERT_STRING (icon, 2);

  FINANGLE (title);
  FINANGLE (icon);
  SDL_WM_SetCaption (RS (title), RS (icon));
  UNFINANGLE (icon);
  UNFINANGLE (title);
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


PRIMPROC
(caption_ti, "caption-ti", 0, 0, 0,
 (void),
 doc: /***********
Return display-window caption as two values: @code{title}
and @code{icon} (both strings, or @code{#f} if not set).  */)
{
#define FUNC_NAME s_caption_ti
  char *title, *icon;

  SDL_WM_GetCaption (&title, &icon);
  RETURN_VALUES2
    (title ? STRING (title) : BOOL_FALSE,
     icon  ? STRING (icon)  : BOOL_FALSE);
#undef FUNC_NAME
}


DECLARE_SIMPLE_SYM (title);
DECLARE_SIMPLE_SYM (icon);

PRIMPROC
(wm_get_caption, "get-caption", 0, 0, 0,
 (void),
 doc: /***********
NB: This procedure is obsoleted by @code{caption-ti}
and @strong{will be removed} after 2013-12-31.

Return an alist with keys @code{title} and @code{icon}
and values the title-bar and icon name (or @code{#f}) of the display
window, respectively.  */)
{
#define FUNC_NAME s_wm_get_caption
  char *title, *icon;

  SDL_WM_GetCaption (&title, &icon);
  return LIST2 (CONS (SYM (title), title ? STRING (title) : BOOL_FALSE),
                CONS (SYM (icon),  icon  ? STRING (icon)  : BOOL_FALSE));
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
Return @code{#t} if successful.  */)
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
Return @code{#t} if successful.  */)
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


PRIMPROC
(wm_grab_input, "grab-input", 0, 1, 0,
 (SCM mode),
 doc: /***********
Grab mouse and keyboard input.  Return new grab state.
Optional arg @var{mode} (a symbol) specifies the kind
of grab, one of @code{query} (the default),
@code{off} or @code{on}.  */)
{
#define FUNC_NAME s_wm_grab_input
  DECLINIT_SYM2NUM_CC (1, grab_modes);
  int cmode = BOUNDP (mode)
    ? ENUM2LONG (1, mode)
    : SDL_GRAB_QUERY;

  return btw->long2enum (SDL_WM_GrabInput (cmode), grab_modes);
#undef FUNC_NAME
}



#include "k/video.c"
#include "k/palette.c"
#include "k/overlay.c"
#include "k/grabmode.c"

void
gsdl_init_video (void)
{
  DEFSMOB (cursor_tag, cursor_nick,
           NULL,
           free_cursor,
           NULL);

  DEFSMOB (pixel_format_tag, pixel_format_nick,
           NULL,
           free_pixel_format,
           print_pixel_format);

  DEFSMOB (overlay_tag, overlay_nick,
           NULL,
           free_yuv_overlay,
           NULL);

  {
    kf_init_t allf[] = {
      { &btw->video_flags, &vid_flagstash },
      { &palette_flags, &pal_flagstash },
      { &gsdl_overlay_formats, &ov_flagstash }
    };

    REGISTER_KF_V (allf);
  }

  {
    kp_init_t allp[] = {
      { &grab_modes, &grabmode_kp }
    };

    REGISTER_KP_V (allp);
  }

#include "sdlvideo.x"
}

/* sdlvideo.c ends here */
