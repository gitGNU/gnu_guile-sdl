/* sdlsurface.c --- SDL Surface functions
 *
 * Copyright (C) 2003, 2004, 2005, 2007, 2008, 2009, 2011 Thien-Thi Nguyen
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
#include <SDL/SDL_image.h>

static SCM alpha_enums;
static valaka_t alpha_eback[] = {
  VALAKA (SDL_ALPHA_OPAQUE),
  VALAKA (SDL_ALPHA_TRANSPARENT)
};


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
print_surface (SCM surface_smob, SCM port, scm_print_state *pstate)
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
Optional third arg @var{flags} (see @code{flagstash:video})
further specifies the surface.  Color depth and masks
are those for the current video surface.  */)
{
#define FUNC_NAME s_make_surface
  Uint32 cflags;
  const SDL_PixelFormat *fmt;
  const SDL_Surface *cur;

  ASSERT_EXACT (width,  1);
  ASSERT_EXACT (height, 2);

  cur = SDL_GetVideoSurface ();
  fmt = cur ? cur->format : SDL_GetVideoInfo ()->vfmt;
  cflags = (UNBOUNDP (flags)
            ? (cur ? cur->flags : 0)
            : GSDL_FLAGS2ULONG (flags, gsdl_video_flags, 3));

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
(list of symbols, see @code{flagstash:video}),
@var{width}, @var{height}, @var{depth}, @var{rmask},
@var{gmask}, @var{bmask}, @var{amask}
(all numbers).  */)
{
#define FUNC_NAME s_create_rgb_surface
  Uint32 cflags;

  ASSERT_EXACT (width,  2);
  ASSERT_EXACT (height, 3);
  ASSERT_EXACT (depth,  4);
  ASSERT_EXACT (rmask,  5);
  ASSERT_EXACT (gmask,  6);
  ASSERT_EXACT (bmask,  7);
  ASSERT_EXACT (amask,  8);

  cflags = GSDL_FLAGS2ULONG (flags, gsdl_video_flags, 1);

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
                      surface_get_ ## f,        \
                      surface_tag, Surface,     \
                      backend)

NUMBER_GETTER (w, w)
NUMBER_GETTER (h, h)
NUMBER_GETTER (depth, format->BitsPerPixel)

GSDL_FLAG_GETTER ("surface:flags", surface_get_flags,
                  surface_tag, PF_Surface *,
                  object->flags, gsdl_video_flags)


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
 (SCM file),
 doc: /***********
Return a surface made by loading the bitmap @var{file}.  */)
{
#define FUNC_NAME s_load_bmp
  range_t cfile;
  SDL_Surface *rv;

  ASSERT_STRING (file, 1);

  FINANGLE (file);
  rv = SDL_LoadBMP (RS (file));
  UNFINANGLE (file);
  RETURN_NEW_SURFACE (rv);
#undef FUNC_NAME
}


/* Load an image in one of many formats.  */
PRIMPROC
(load_image, "load-image", 1, 0, 0,
 (SCM file),
 doc: /***********
Return a surface made by loading the image @var{file}.
If there are problems, return @code{#f}.  */)
{
#define FUNC_NAME s_load_image
  range_t cfile;
  SDL_Surface *rv;

  ASSERT_STRING (file, 1);

  FINANGLE (file);
  rv = IMG_Load (RS (file));
  UNFINANGLE (file);
  RETURN_NEW_SURFACE (rv);
#undef FUNC_NAME
}


/* Experimental: Load an image in one of many formats from a string.  */
PRIMPROC
(string_to_image, "string->image", 1, 0, 0,
 (SCM s),
 doc: /***********
Return a surface made by loading image data from string
@var{s}.  [WARNING: This procedure is experimental!]  */)
{
#define FUNC_NAME s_string_to_image
  range_t cs;
  SDL_Surface *rv;

  ASSERT_STRING (s, 1);

  FINANGLE_RAW (s);
  rv = IMG_Load_RW (SDL_RWFromConstMem (RS (s), RLEN (s)), 0);
  UNFINANGLE (s);
  RETURN_NEW_SURFACE (rv);
#undef FUNC_NAME
}


PRIMPROC
(save_bmp, "save-bmp", 2, 0, 0,
 (SCM surface,
  SCM file),
 doc: /***********
Save @var{surface} to @var{file} in Windows BMP format.
Return @code{#t} if successful.  */)
{
#define FUNC_NAME s_save_bmp
  range_t cfile;
  int rv;

  ASSERT_SURFACE (surface,  1);
  ASSERT_STRING (file, 2);

  FINANGLE (file);
  rv = SDL_SaveBMP (UNPACK_SURFACE (surface), RS (file));
  UNFINANGLE (file);
  RETURN_TRUE_IF_0 (rv);
#undef FUNC_NAME
}


PRIMPROC
(set_color_key, "set-color-key!", 3, 0, 0,
 (SCM surface,
  SCM flag,
  SCM key),
 doc: /***********
Set @var{surface} color key as specified by @var{flag}
(see @code{flagstash:video}) and @var{key}.  */)
{
#define FUNC_NAME s_set_color_key
  Uint32 cflag;

  ASSERT_SURFACE (surface, 1);
  ASSERT_EXACT (key, 3);

  cflag = GSDL_FLAGS2ULONG (flag, gsdl_video_flags, 2);

  RETURN_TRUE_IF_0
    (SDL_SetColorKey (UNPACK_SURFACE (surface), cflag, C_LONG (key)));
#undef FUNC_NAME
}


PRIMPROC
(set_alpha, "set-alpha!", 2, 1, 0,
 (SCM surface,
  SCM flag,
  SCM alpha),
 doc: /***********
Adjust whole-@var{surface} alpha as specified by
@var{flag} (see @code{flagstash:video}) and @var{alpha}
(one of the @code{alpha-enums}, or a number 0-255).
@xref{Enums and Constants}.
If @var{flag} is @code{#f}, ignore @var{alpha} completely.  */)
{
#define FUNC_NAME s_set_alpha
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
    ASSERT_EXACT (alpha, 3);

  cflag = (EXACTLY_FALSEP (flag)
           ? 0
           : GSDL_FLAGS2ULONG (flag, gsdl_video_flags, 2));
  calpha = (Uint8) GSDL_ENUM2LONG (alpha, alpha_enums, 3);

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
(symbols) from the set returned by @code{flagstash:video}.  */)
{
#define FUNC_NAME s_convert_surface
  Uint32 cflags = 0;

  ASSERT_SURFACE (surface, 1);
  ASSERT_PIXEL_FORMAT (format, 2);

  if (BOUNDP (flags))
    cflags = GSDL_FLAGS2ULONG (flags, gsdl_video_flags, 3);

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
  dst = SDL_CreateRGBSurface (src->flags, w, h, 16, 0, 0, 0, 0);

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
  dst = SDL_CreateRGBSurface (src->flags, w, h, 16, 0, 0, 0, 0);

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



void
gsdl_init_surface (void)
{
  DEFSMOB (surface_tag, surface_nick,
           NULL,
           free_surface,
           print_surface);

#include "sdlsurface.x"

  /* alpha constants */
  alpha_enums = DEFINE_ENUM ("alpha-enums", alpha_eback);
}

/* sdlsurface.c ends here */
