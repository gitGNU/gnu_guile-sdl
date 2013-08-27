/* sdlsurface.c --- SDL Surface functions
 *
 * Copyright (C) 2003, 2004, 2005, 2007, 2008, 2009,
 *   2011, 2013 Thien-Thi Nguyen
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

#if ! GI_LEVEL_1_8
IMPORT_SRFI4 ();
SELECT_MODULE_VAR (mk_u8v,  srfi4, "make-u8vector");
SELECT_MODULE_VAR (mk_u16v, srfi4, "make-u16vector");
SELECT_MODULE_VAR (mk_u32v, srfi4, "make-u32vector");
#define scm_make_u8vector(len,fill)   CALL2 (mk_u8v,  len, fill)
#define scm_make_u16vector(len,fill)  CALL2 (mk_u16v, len, fill)
#define scm_make_u32vector(len,fill)  CALL2 (mk_u32v, len, fill)
#endif

static SCM kp_alpha;
static SCM alpha_enums;


/* smob functions */

#define SURFACE_P(x) \
  (SCM_SMOB_PREDICATE (surface_tag, x))

static
size_t
free_surface (SCM surface)
{
  PF_Surface *pf = UNPACK_PF_SURFACE (surface);

  if (! pf->internalp)
    SDL_FreeSurface (pf->object);
  GCFREE (pf, surface_nick);
  return GCRV (pf);
}

static
int
print_surface (SCM surface_smob, SCM port, UNUSED scm_print_state *pstate)
{
  SDL_Surface *surface = UNPACK_SURFACE (surface_smob);
  char buf[64], sbuf[64];

  if (surface)
    snprintf (sbuf, 64, "%dx%d %d bpp",
              surface->w, surface->h,
              surface->format->BitsPerPixel);

  snprintf (buf, 64, "#<%s %s>", surface_nick, surface ? sbuf : "NULL");
  scm_puts (buf, port);
  return 1;
}


/* constructors */

PRIMPROC
(make_surface, "make-surface", 2, 1, 0,
 (SCM width,
  SCM height,
  SCM flags),
 doc: /***********
Return a new surface of dimensions @var{width} by @var{height}.
Optional third arg @var{flags} (@pxref{video flags})
further specifies the surface.  Color depth and masks
are those for the current video surface.  */)
{
#define FUNC_NAME s_make_surface
  DECLINIT_SYM2NUM_CC (3, btw->video_flags);
  Uint32 cflags;
  const SDL_PixelFormat *fmt;
  const SDL_Surface *cur;

  ASSERT_INTEGER (width,  1);
  ASSERT_INTEGER (height, 2);

  cur = SDL_GetVideoSurface ();
  fmt = cur ? cur->format : SDL_GetVideoInfo ()->vfmt;
  cflags = (UNBOUNDP (flags)
            ? (cur ? cur->flags : 0)
            : FLAGS2ULONG (3, flags));

  /* Return a newly allocated surface smob.  */
  RETURN_NEW_SURFACE
    (SDL_CreateRGBSurface (cflags, C_LONG (width), C_LONG (height),
                           /* Defaults from current video info.  */
                           fmt->BitsPerPixel,
                           fmt->Rmask,
                           fmt->Gmask,
                           fmt->Bmask,
                           fmt->Amask));
#undef FUNC_NAME
}


PRIMPROC
(create_rgb_surface, "create-rgb-surface", 8, 0, 0,
 (SCM flags,
  SCM width, SCM height, SCM depth,
  SCM rmask, SCM gmask, SCM bmask, SCM amask),
 doc: /***********
Return an empty surface.
The eight arguments, directly analagous to those
for SDL_CreateRGBSurface, are: @var{flags}
(list of symbols, @pxref{video flags}),
@var{width}, @var{height}, @var{depth}, @var{rmask},
@var{gmask}, @var{bmask}, @var{amask}
(all numbers).  */)
{
#define FUNC_NAME s_create_rgb_surface

  DECLINIT_SYM2NUM_CC (1, btw->video_flags);
  Uint32 cflags;

  ASSERT_INTEGER (width,  2);
  ASSERT_INTEGER (height, 3);
  ASSERT_INTEGER (depth,  4);
  ASSERT_INTEGER (rmask,  5);
  ASSERT_INTEGER (gmask,  6);
  ASSERT_INTEGER (bmask,  7);
  ASSERT_INTEGER (amask,  8);

  cflags = FLAGS2ULONG (1, flags);

  /* Return a newly allocated surface smob.  */
  RETURN_NEW_SURFACE
    (SDL_CreateRGBSurface (cflags,
                           C_LONG (width), C_LONG (height), C_LONG (depth),
                           C_ULONG (rmask), C_ULONG (gmask),
                           C_ULONG (bmask), C_ULONG (amask)));
#undef FUNC_NAME
}

/* accessors */

#define NUMBER_GETTER(f,backend)                \
  GSDL_PF_NUMBER_GETTER ("surface:" #f,         \
                         surface_get_ ## f,     \
                         surface, Surface,      \
                         backend)

NUMBER_GETTER (w, w)
NUMBER_GETTER (h, h)
NUMBER_GETTER (depth, format->BitsPerPixel)

GSDL_FLAG_GETTER ("surface:flags", surface_get_flags,
                  surface, PF_Surface *,
                  object->flags, flags, btw->video_flags)


PRIMPROC
(surface_get_format, "surface-get-format", 1, 0, 0,
 (SCM surface),
 doc: /***********
Return a new pixel format, the same used by @var{surface}.  */)
{
#define FUNC_NAME s_surface_get_format
  ASSERT_SURFACE (surface, 1);

  RETURN_NEW_PIXEL_FORMAT (UNPACK_SURFACE (surface)->format);
#undef FUNC_NAME
}


/* utilities */

PRIMPROC
(surface_p, "surface?", 1, 0, 0,
 (SCM obj),
 doc: /***********
Return true iff @var{obj} is a surface.  */)
{
#define FUNC_NAME s_surface_p
  RETURN_BOOL
    (SURFACE_P (obj));
#undef FUNC_NAME
}


PRIMPROC
(lock_surface, "lock-surface", 1, 0, 0,
 (SCM surface),
 doc: /***********
Lock @var{surface} for direct access.
Return @code{#t} if successful.  */)
{
#define FUNC_NAME s_lock_surface
  ASSERT_SURFACE (surface, 1);

  RETURN_TRUE_IF_0
    (SDL_LockSurface (UNPACK_SURFACE (surface)));
#undef FUNC_NAME
}


PRIMPROC
(unlock_surface, "unlock-surface", 1, 0, 0,
 (SCM surface),
 doc: /***********
Unlock previously locked @var{surface}.  */)
{
#define FUNC_NAME s_unlock_surface
  ASSERT_SURFACE (surface, 1);

  SDL_UnlockSurface (UNPACK_SURFACE (surface));
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


PRIMPROC
(load_bmp, "load-bmp", 1, 0, 0,
 (SCM filename),
 doc: /***********
Load bitmap data from @var{filename}.
Return a new surface if successful, otherwise @code{#f}.  */)
{
#define FUNC_NAME s_load_bmp
  range_t cfilename;
  SDL_Surface *rv;

  ASSERT_STRING (filename, 1);

  FINANGLE (filename);
  rv = SDL_LoadBMP (RS (filename));
  UNFINANGLE (filename);
  RETURN_NEW_SURFACE (rv);
#undef FUNC_NAME
}


/* Load an image in one of many formats.  */
PRIMPROC
(load_image, "load-image", 1, 0, 0,
 (SCM filename),
 doc: /***********
Load image data from @var{filename}.
Return a new surface if successful, otherwise @code{#f}.  */)
{
#define FUNC_NAME s_load_image
  range_t cfilename;
  SDL_Surface *rv;

  ASSERT_STRING (filename, 1);

  FINANGLE (filename);
  rv = IMG_Load (RS (filename));
  UNFINANGLE (filename);
  RETURN_NEW_SURFACE (rv);
#undef FUNC_NAME
}


PRIMPROC
(save_bmp, "save-bmp", 2, 0, 0,
 (SCM surface,
  SCM filename),
 doc: /***********
Save @var{surface} to @var{filename} in Windows BMP format.
Return @code{#t} if successful.  */)
{
#define FUNC_NAME s_save_bmp
  range_t cfilename;
  int rv;

  ASSERT_SURFACE (surface,  1);
  ASSERT_STRING (filename, 2);

  FINANGLE (filename);
  rv = SDL_SaveBMP (UNPACK_SURFACE (surface), RS (filename));
  UNFINANGLE (filename);
  RETURN_TRUE_IF_0 (rv);
#undef FUNC_NAME
}


PRIMPROC
(surface_color_key_x, "surface-color-key!", 2, 1, 0,
 (SCM surface, SCM pixel, SCM rle),
 doc: /***********
Set the color key for @var{surface} to @var{pixel}.
If @var{pixel} is @code{#f}, clear the current color key.
Otherwise, it should be an integer of the appropriate depth
for @var{surface} (e.g., in the range [0,65535] for 16 bpp).
If color key processing is enabled, optional arg @var{rle} is a
boolean that enables (true) or disables (false, the default)
RLE acceleration.
Return @code{#t} if successful.  */)
{
#define FUNC_NAME s_surface_color_key_x
  Uint32 cpixel;
  Uint32 cflags;

  ASSERT_SURFACE (surface, 1);
  if (EXACTLY_FALSEP (pixel))
    cflags = cpixel = 0;                /* disable */
  else
    {
      ASSERT_ULONG_COPY (pixel, 2);
      UNBOUND_MEANS_FALSE (rle);
      cflags = SDL_SRCCOLORKEY | (NOT_FALSEP (rle)
                                  ? SDL_RLEACCEL
                                  : 0);
    }

  RETURN_TRUE_IF_0
    (SDL_SetColorKey (UNPACK_SURFACE (surface),
                      cflags, cpixel));
#undef FUNC_NAME
}


PRIMPROC
(set_color_key, "set-color-key!", 3, 0, 0,
 (SCM surface,
  SCM flag,
  SCM key),
 doc: /***********
NB: This procedure is obsoleted by @code{surface-color-key!}
and @strong{will be removed} after 2013-12-31.

Set @var{surface} color key as specified by @var{flag}
(@pxref{video flags}) and @var{key}.  */)
{
#define FUNC_NAME s_set_color_key
  DECLINIT_SYM2NUM_CC (2, btw->video_flags);
  Uint32 cflag;

  ASSERT_SURFACE (surface, 1);
  ASSERT_INTEGER (key, 3);

  cflag = FLAGS2ULONG (2, flag);

  RETURN_TRUE_IF_0
    (SDL_SetColorKey (UNPACK_SURFACE (surface), cflag, C_LONG (key)));
#undef FUNC_NAME
}


PRIMPROC
(surface_alpha_x, "surface-alpha!", 2, 1, 0,
 (SCM surface, SCM alpha, SCM rle),
 doc: /***********
Set alpha blending for the entire @var{surface} to @var{alpha}.
If @var{alpha} is @code{#f}, disable alpha blending.
Otherwise it should be an integer in the range [0,255]
or one of the symbols @code{transparent} or @code{opaque}.
If alpha blending is enabled, optional arg @var{rle} is a
boolean that enables (true) or disables (false, the default)
RLE acceleration.
Return @code{#t} if successful.  */)
{
#define FUNC_NAME s_surface_alpha_x
  DECLINIT_SYM2NUM_CC (2, kp_alpha);
  Uint32 cflags;
  Uint8 calpha;

  ASSERT_SURFACE (surface, 1);
  if (EXACTLY_FALSEP (alpha))
    cflags = calpha = 0;                /* disable */
  else
    {
      UNBOUND_MEANS_FALSE (rle);
      cflags = SDL_SRCALPHA | (NOT_FALSEP (rle)
                               ? SDL_RLEACCEL
                               : 0);
      calpha = ENUM2LONG (2, alpha);
    }

  RETURN_TRUE_IF_0
    (SDL_SetAlpha (UNPACK_SURFACE (surface),
                   cflags, calpha));
#undef FUNC_NAME
}


PRIMPROC
(set_alpha, "set-alpha!", 2, 1, 0,
 (SCM surface,
  SCM flag,
  SCM alpha),
 doc: /***********
NB: This procedure is obsoleted by @code{surface-alpha!}
and @strong{will be removed} after 2013-12-31.

Adjust whole-@var{surface} alpha as specified by
@var{flag} (@pxref{video flags}) and @var{alpha}
(@pxref{alpha-enum enums}, or a number 0-255).
If @var{flag} is @code{#f}, ignore @var{alpha} completely.  */)
{
#define FUNC_NAME s_set_alpha
  DECLINIT_SYM2NUM_CC (2, btw->video_flags);
  DECLINIT_SYM2NUM_CC (3, alpha_enums);
  Uint32 cflag;
  Uint8 calpha;

  ASSERT_SURFACE (surface, 1);
  if (EXACTLY_FALSEP (flag) || NULLP (flag))
    {
      flag = SCM_BOOL_F;
      alpha = SCM_INUM0;
    }
  if (UNBOUNDP (alpha))
    alpha = SCM_INUM0;
  else
    ASSERT_INTEGER (alpha, 3);

  cflag = (EXACTLY_FALSEP (flag)
           ? 0
           : FLAGS2ULONG (2, flag));
  calpha = (Uint8) ENUM2LONG (3, alpha);

  RETURN_TRUE_IF_0
    (SDL_SetAlpha (UNPACK_SURFACE (surface),
                   cflag,
                   calpha));
#undef FUNC_NAME
}


PRIMPROC
(set_clip_rect, "set-clip-rect!", 1, 1, 0,
 (SCM surface,
  SCM rect),
 doc: /***********
Set @var{surface} clipping rectangle to the whole surface.
Optional arg @var{rect}, if non-@code{#f}, specifies a particular
rectangle instead of using the whole surface.  */)
{
#define FUNC_NAME s_set_clip_rect
  SDL_Rect *crect = NULL;

  ASSERT_SURFACE (surface, 1);

  if (BOUNDP (rect) && !EXACTLY_FALSEP (rect))
    {
      /* Rect defaults to NULL (the whole surface).  */
      ASSERT_RECT (rect, 2);
      crect = UNPACK_RECT (rect);
    }

  SDL_SetClipRect (UNPACK_SURFACE (surface), crect);
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


PRIMPROC
(get_clip_rect, "get-clip-rect", 1, 0, 0,
 (SCM surface),
 doc: /***********
Return the clipping rectangle for @var{surface}.  */)
{
#define FUNC_NAME s_get_clip_rect
  SDL_Rect *rect = NULL;

  ASSERT_SURFACE (surface, 1);

  if ((rect = GCMALLOC_RECT ()))
    SDL_GetClipRect (UNPACK_SURFACE (surface), rect);
  RETURN_NEW_RECT (rect);
#undef FUNC_NAME
}


PRIMPROC
(convert_surface, "convert-surface", 2, 1, 0,
 (SCM surface,
  SCM format,
  SCM flags),
 doc: /***********
Convert @var{surface} to the same @var{format} as another
surface.  Optional third arg @var{flags} is a list of flags
(@pxref{video flags}).  */)
{
#define FUNC_NAME s_convert_surface
  DECLINIT_SYM2NUM_CC (3, btw->video_flags);
  Uint32 cflags = 0;

  ASSERT_SURFACE (surface, 1);
  ASSERT_PIXEL_FORMAT (format, 2);

  if (BOUNDP (flags))
    cflags = FLAGS2ULONG (3, flags);

  RETURN_NEW_SURFACE
    (SDL_ConvertSurface (UNPACK_SURFACE (surface),
                         UNPACK_PIXEL_FORMAT (format),
                         cflags));
#undef FUNC_NAME
}


PRIMPROC
(blit_surface, "blit-surface", 1, 3, 0,
 (SCM src,
  SCM srcrect,
  SCM dst,
  SCM dstrect),
 doc: /***********
Perform a fast blit from the
@var{src} surface @var{srcrect} to the
@var{dst} surface @var{dstrect}.
@var{srcrect} defaults to x=0, y=0, @var{src} surface
dimensions.  If unspecified @var{dst} is taken as
the default video surface.  @var{dstrect} likewise defaults
to x=0, y=0, @var{dst} surface dimensions.  */)
{
#define FUNC_NAME s_blit_surface
  SDL_Surface *csrc;
  SDL_Surface *cdst;
  SDL_Rect *csrcrect;
  SDL_Rect *cdstrect;
  SDL_Rect default_rect;

  /* 1st arg, source surface.  */
  ASSERT_SURFACE (src, 1);
  csrc = UNPACK_SURFACE (src);

  /* 2nd arg, source rect, default (0,0) by source dimensions.  */
  UNBOUND_MEANS_FALSE (srcrect);
  if (NOT_FALSEP (srcrect))
    {
      ASSERT_RECT (srcrect, 2);
      csrcrect = UNPACK_RECT (srcrect);
    }
  else
    {
      default_rect.x = 0;
      default_rect.y = 0;
      default_rect.w = csrc->w;
      default_rect.h = csrc->h;
      csrcrect = &default_rect;
    }

  /* 3rd arg, dest surface, default video surface.  */
  UNBOUND_MEANS_FALSE (dst);
  if (NOT_FALSEP (dst))
    {
      ASSERT_SURFACE (dst, 3);
      cdst = UNPACK_SURFACE (dst);
    }
  else
    cdst = SDL_GetVideoSurface ();

  /* 4th arg, dest rect, default src rect.  */
  UNBOUND_MEANS_FALSE (dstrect);
  if (NOT_FALSEP (dstrect))
    {
      ASSERT_RECT (dstrect, 4);
      cdstrect = UNPACK_RECT (dstrect);
    }
  else
    cdstrect = csrcrect;

  RETURN_INT (SDL_BlitSurface (csrc, csrcrect, cdst, cdstrect));
#undef FUNC_NAME
}


/* flipping */

#define MAKE_DEST_SURFACE(src)                          \
  SDL_CreateRGBSurface (src->flags, src->w, src->h,     \
                        src->format->BitsPerPixel,      \
                        0, 0, 0, 0)

PRIMPROC
(vertical_flip_surface, "vertical-flip-surface", 1, 0, 0,
 (SCM surface),
 doc: /***********
Return a new surface created by flipping @var{surface} vertically.  */)
{
#define FUNC_NAME s_vertical_flip_surface
  int i, w, h;
  SDL_Surface *src, *dst;
  SDL_Rect srcrect, dstrect;

  /* Verify args.  */
  ASSERT_SURFACE (surface, 1);

  /* Get source and dimensions.  */
  src = UNPACK_SURFACE (surface);
  w = src->w;
  h = src->h;

  /* Create a new surface.  */
  dst = MAKE_DEST_SURFACE (src);

  /* Initialize the rects.  */
  srcrect.x = 0;  srcrect.y = 0;    srcrect.w = w;  srcrect.h = 1;
  dstrect.x = 0;  dstrect.y = h-1;  dstrect.w = w;  dstrect.h = 1;

  /* Loop through, copying lines from top to bottom.  */
  for (i = h; i >= 0; i--)
    {
      SDL_BlitSurface (src, &srcrect, dst, &dstrect);
      srcrect.y++;
      dstrect.y--;
    }

  /* Return the surface.  */
  RETURN_NEW_SURFACE (dst);
#undef FUNC_NAME
}


PRIMPROC
(horizontal_flip_surface, "horizontal-flip-surface", 1, 0, 0,
 (SCM surface),
 doc: /***********
Return a new surface created by flipping @var{surface} horizontally.  */)
{
#define FUNC_NAME s_horizontal_flip_surface
  int i, w, h;
  SDL_Surface *src, *dst;
  SDL_Rect srcrect, dstrect;

  /* Verify args.  */
  ASSERT_SURFACE (surface, 1);

  /* Get source and dimensions.  */
  src = UNPACK_SURFACE (surface);
  w = src->w;
  h = src->h;

  /* Create a new surface.  */
  dst = MAKE_DEST_SURFACE (src);

  /* Initialize the rects.  */
  srcrect.x = 0;    srcrect.y = 0;  srcrect.w = 1;  srcrect.h = h;
  dstrect.x = w-1;  dstrect.y = 0;  dstrect.w = 1;  dstrect.h = h;

  /* Loop through, copying lines from left to right.  */
  for (i = w; i >= 0; i--)
    {
      SDL_BlitSurface (src, &srcrect, dst, &dstrect);
      srcrect.x++;
      dstrect.x--;
    }

  /* Return the surface.  */
  RETURN_NEW_SURFACE (dst);
#undef FUNC_NAME
}


PRIMPROC
(vh_flip_surface, "vh-flip-surface", 1, 0, 0,
 (SCM surface),
 doc: /***********
Return a new surface created by flipping @var{surface}
both vertically and horizontally.  */)
{
#define FUNC_NAME s_vh_flip_surface
  SCM temp = vertical_flip_surface (surface);
  return horizontal_flip_surface (temp);
#undef FUNC_NAME
}


PRIMPROC
(surface_pixels, "surface-pixels", 1, 1, 0,
 (SCM surface, SCM squash),
 doc: /***********
Return pixel data of @var{surface} as a new uniform vector.
The uvec has type @code{u8}, @code{u16} or @code{u32}, corresponding
to the @var{surface} depth, with @var{height} x @var{width} elements.
A 24bpp surface --- @var{depth-in-bytes} of 3 --- is expanded (per pixel)
to @code{u32}, leaving the high nybble clear.

Optional arg @var{squash} non-@code{#f} means to
return a u8vector regardless of @var{surface} depth,
with @var{height} x @var{width} x @var{depth-in-bytes} elements.  */)
{
#define FUNC_NAME s_surface_pixels
  DECLARE_UV_STRUCT (/* not const */, u8,  Uint8);
  DECLARE_UV_STRUCT (/* not const */, u16, Uint16);
  DECLARE_UV_STRUCT (/* not const */, u32, Uint32);

  SDL_Surface *src;
  size_t len, bypp, sz;
  SCM rv = SCM_EOL;

  ASSERT_SURFACE (surface, 1);
  src = UNPACK_SURFACE (surface);
  len = src->w * src->h;
  bypp = src->format->BytesPerPixel;
  sz = len * bypp;
  if (! (1 <= bypp && bypp <= 4))
    SCM_MISC_ERROR ("invalid depth: ~S", LIST1 (surface));

#define PREP(TT,COUNT)                          \
  STUFF (TT, rv);                               \
  rv = scm_make_ ## TT ## vector                \
    (NUM_ULONG (COUNT), SCM_UNDEFINED);         \
  GET_WRITABLE_PARTICULARS (TT, rv)

#define DIRECT(TT,COUNT,ALLOC)  do                      \
    {                                                   \
      PREP (TT, COUNT);                                 \
      memcpy (ST (rv, elt), src->pixels, ALLOC);        \
      LATER (rv);                                       \
    }                                                   \
  while (0)

#define SNARF(TT)  DIRECT (TT, len, sz)

  /* Do it!  */
  if (BOUNDP (squash) && NOT_FALSEP (squash))
    DIRECT (u8, sz, sz);
  else
    switch (bypp)
      {
      case 4: SNARF (u32); break;
      case 2: SNARF (u16); break;
      case 1: SNARF (u8);  break;
      case 3:
        {
          size_t i, lsbidx = SDL_LIL_ENDIAN == SDL_BYTEORDER ? 0 : 2;
          const uint8_t *rp;
          uint32_t *wp;

          PREP (u32, len);
          for (i = 0, rp = src->pixels, wp = ST (rv, elt);
               i < len;
               i++, rp += bypp, wp++)
            *wp = ((rp[2 - lsbidx] << 16)
                   | (rp[1]        << 8)
                   | (rp[lsbidx]   << 0));
          LATER (rv);
        }
      }

  return rv;

#undef SNARF
#undef DIRECT
#undef PREP
#undef FUNC_NAME
}



#include "k/alphalim.c"
#include "k/alphalimold.c"

void
gsdl_init_surface (void)
{
  DEFSMOB (surface_tag, surface_nick,
           NULL,
           free_surface,
           print_surface);

#include "sdlsurface.x"

  /* alpha constants */
  {
    kp_init_t allp[] = {
      { &alpha_enums, &alphalimold_kp },
      { &kp_alpha, &alphalim_kp }
    };

    REGISTER_KP_V (allp);
  }
}

/* sdlsurface.c ends here */
