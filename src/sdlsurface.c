/* sdlsurface.c --- SDL Surface functions
 *
 * 	Copyright (C) 2003,2004 Thien-Thi Nguyen
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
#include "wholefns.h"

#include "sdlenums.h"
#include "sdlsmobs.h"
#include "sdlvideo.h"
#include "retval.h"
#include "bool.h"


/* smob functions */

#define SURFACE_P(x) \
  (SCM_SMOB_PREDICATE (surface_tag, x))

static
SCM
mark_surface (SCM surface)
{
  return surface;
}

static
size_t
free_surface (SCM surface)
{
  SDL_FreeSurface (UNPACK_SURFACE (surface));
  /* return sizeof (SDL_Surface); */
  return 0;
}

static
int
print_surface (SCM surface_smob, SCM port, scm_print_state *pstate)
{
  SDL_Surface *surface = UNPACK_SURFACE (surface_smob);

  scm_puts                                   ("#<SDL-Surface ", port);

  if (surface)
    {
      scm_display                    (gh_long2scm (surface->w), port);
      scm_puts                                            ("x", port);
      scm_display                    (gh_long2scm (surface->h), port);
      scm_puts                                            (" ", port);
      scm_display (gh_long2scm (surface->format->BitsPerPixel), port);
      scm_puts                                         (" bpp", port);
    }
  else
    scm_puts                                           ("NULL", port);

  scm_puts                                                (">", port);

  /* Non-zero means success.  */
  return 1;
}


/* constructors */

GH_DEFPROC (make_surface, "make-surface", 2, 1, 0,
            (SCM width,
             SCM height,
             SCM flags),
            "Return a new surface of dimensions @var{width} by @var{height}.\n"
            "Optional third arg @var{flags} (see @code{flagstash:video})\n"
            "further specifies the surface.  Color depth and masks\n"
            "are those for the current video surface.")
{
#define FUNC_NAME s_make_surface
  Uint32 cflags;
  const SDL_PixelFormat *fmt;

  ASSERT_EXACT (width,  ARGH1);
  ASSERT_EXACT (height, ARGH2);

  cflags = (UNBOUNDP (flags)
            ? SDL_GetVideoSurface ()->flags
            : GSDL_FLAGS2ULONG (flags, gsdl_video_flags, ARGH3));

  fmt = SDL_GetVideoInfo ()->vfmt;

  /* Return a newly allocated surface smob.  */
  RETURN_NEW_SURFACE
    (SDL_CreateRGBSurface (cflags,
                           gh_scm2long (width), gh_scm2long (height),
                           /* Defaults from current video info.  */
                           (Uint8)  fmt->BitsPerPixel,
                           (Uint32) fmt->Rmask,
                           (Uint32) fmt->Gmask,
                           (Uint32) fmt->Bmask,
                           (Uint32) fmt->Amask));
#undef FUNC_NAME
}


GH_DEFPROC (create_rgb_surface, "create-rgb-surface", 8, 0, 0,
            (SCM flags,
             SCM width, SCM height, SCM depth,
             SCM rmask, SCM gmask, SCM bmask, SCM amask),
            "Return an empty surface.\n"
            "The eight arguments, directly analagous to those\n"
            "for SDL_CreateRGBSurface, are: @var{flags}\n"
            "(list of symbols, see @code{flagstash:video}),\n"
            "@var{width}, @var{height}, @var{depth}, @var{rmask},\n"
            "@var{gmask}, @var{bmask}, @var{amask}\n"
            "(all numbers).")
{
#define FUNC_NAME s_create_rgb_surface
  Uint32 cflags;

  ASSERT_EXACT (width,  ARGH2);
  ASSERT_EXACT (height, ARGH3);
  ASSERT_EXACT (depth,  ARGH4);
  ASSERT_EXACT (rmask,  ARGH5);
  ASSERT_EXACT (gmask,  ARGH6);
  ASSERT_EXACT (bmask,  ARGH7);
  ASSERT_EXACT (amask,  ARGHn);

  cflags = (Uint32) GSDL_FLAGS2ULONG (flags, gsdl_video_flags, ARGH1);

  /* Return a newly allocated surface smob.  */
  RETURN_NEW_SURFACE
    (SDL_CreateRGBSurface (cflags,
                           gh_scm2long (width), gh_scm2long (height),
                           (Uint8)  gh_scm2long (depth),
                           (Uint32) gh_scm2long (rmask),
                           (Uint32) gh_scm2long (gmask),
                           (Uint32) gh_scm2long (bmask),
                           (Uint32) gh_scm2long (amask)));
#undef FUNC_NAME
}

/* accessors */

#define NUMBER_GETTER(f,backend)                        \
  GSDL_NUMBER_GETTER ("surface:" #f,                    \
                      surface_get_ ## f,                \
                      surface_tag, SDL_Surface *,       \
                      backend)

NUMBER_GETTER (w, w)
NUMBER_GETTER (h, h)
NUMBER_GETTER (depth, format->BitsPerPixel)

GSDL_FLAG_GETTER ("surface:flags", surface_get_flags,
                  surface_tag, SDL_Surface *,
                  flags, gsdl_video_flags)


GH_DEFPROC (surface_get_format, "surface-get-format", 1, 0, 0,
            (SCM surface),
            "Return a new pixel format, the same used by @var{surface}.")
{
#define FUNC_NAME s_surface_get_format
  ASSERT_SURFACE (surface, ARGH1);

  RETURN_NEW_PIXEL_FORMAT (UNPACK_SURFACE (surface)->format);
#undef FUNC_NAME
}


/* utilities */

GH_DEFPROC (surface_p, "surface?", 1, 0, 0,
            (SCM obj),
            "Return true iff @var{obj} is a surface.")
{
#define FUNC_NAME s_surface_p
  RETURN_BOOL
    (SURFACE_P (obj));
#undef FUNC_NAME
}


GH_DEFPROC (lock_surface, "lock-surface", 1, 0, 0,
            (SCM surface),
            "Lock @var{surface} for direct access.\n"
            "Return #t if successful.")
{
#define FUNC_NAME s_lock_surface
  ASSERT_SURFACE (surface, ARGH1);

  RETURN_TRUE_IF_0
    (SDL_LockSurface (UNPACK_SURFACE (surface)));
#undef FUNC_NAME
}


GH_DEFPROC (unlock_surface, "unlock-surface", 1, 0, 0,
            (SCM surface),
            "Unlock previously locked @var{surface}.\n"
            "The return value is unspecified.")
{
#define FUNC_NAME s_unlock_surface
  ASSERT_SURFACE (surface, ARGH1);

  SDL_UnlockSurface (UNPACK_SURFACE (surface));
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


GH_DEFPROC (load_bmp, "load-bmp", 1, 0, 0,
            (SCM file),
            "Return a surface made by loading the bitmap @var{file}.")
{
#define FUNC_NAME s_load_bmp
  ASSERT_STRING (file, ARGH1);

  RETURN_NEW_SURFACE (SDL_LoadBMP (SCM_CHARS (file)));
#undef FUNC_NAME
}


/* Load an image in one of many formats.  */
GH_DEFPROC (load_image, "load-image", 1, 0, 0,
            (SCM file),
            "Return a surface made by loading the image @var{file}.")
{
#define FUNC_NAME s_load_bmp
  ASSERT_STRING (file, ARGH1);

  RETURN_NEW_SURFACE (IMG_Load (SCM_CHARS (file)));
#undef FUNC_NAME
}


/* Experimental: Load an image in one of many formats from a string.  */
GH_DEFPROC (string_to_image, "string->image", 1, 0, 0,
            (SCM s),
            "Return a surface made by loading image data from string\n"
            "@var{s}.  [WARNING: This procedure is experimental!]")
{
#define FUNC_NAME s_string_to_image
  void *mem; int size;

  ASSERT_STRING (s, ARGH1);
  mem = SCM_CHARS (s);
  size = SCM_ROLENGTH (s);

  RETURN_NEW_SURFACE (IMG_Load_RW (SDL_RWFromMem (mem, size), 0));
#undef FUNC_NAME
}


GH_DEFPROC (save_bmp, "save-bmp", 2, 0, 0,
            (SCM surface,
             SCM file),
            "Save @var{surface} to @var{file} in Windows BMP format.\n"
            "Return #t if successful.")
{
#define FUNC_NAME s_save_bmp
  ASSERT_SURFACE (surface,  ARGH1);
  ASSERT_STRING (file, ARGH2);

  RETURN_TRUE_IF_0
    (SDL_SaveBMP (UNPACK_SURFACE (surface),
                  SCM_CHARS (file)));
#undef FUNC_NAME
}


GH_DEFPROC (set_color_key, "set-color-key!", 3, 0, 0,
            (SCM surface,
             SCM flag,
             SCM key),
            "Set @var{surface} color key as specified by @var{flag}\n"
            "(see @code{flagstash:video}) and @var{key}.")
{
#define FUNC_NAME s_set_color_key
  Uint32 cflag;

  ASSERT_SURFACE (surface, ARGH1);
  ASSERT_EXACT (key, ARGH3);

  cflag = (Uint32) GSDL_FLAGS2ULONG (flag, gsdl_video_flags, ARGH2);

  RETURN_TRUE_IF_0
    (SDL_SetColorKey (UNPACK_SURFACE (surface),
                      cflag,
                      gh_scm2long (key)));
#undef FUNC_NAME
}


GH_DEFPROC (set_alpha, "set-alpha!", 3, 0, 0,
            (SCM surface,
             SCM flag,
             SCM alpha),
            "Adjust @var{surface} alpha properties as specified by\n"
            "@var{flag} (see @code{flagstash:video}) and @var{alpha}.")
{
#define FUNC_NAME s_set_alpha
  Uint32 cflag;
  Uint8 calpha;

  ASSERT_SURFACE (surface, ARGH1);
  /* hmmm, why was this here? --ttn */
  /* ASSERT_EXACT (flag, ARGH2); */
  ASSERT_EXACT (alpha, ARGH3);

  cflag  = (Uint32) GSDL_FLAGS2ULONG (flag, gsdl_video_flags, ARGH2);
  calpha = (Uint8) gsdl_enum2long (alpha, gsdl_alpha_enums, ARGH3, FUNC_NAME);

  RETURN_TRUE_IF_0
    (SDL_SetAlpha (UNPACK_SURFACE (surface),
                   cflag,
                   calpha));
#undef FUNC_NAME
}


GH_DEFPROC (set_clip_rect, "set-clip-rect!", 1, 1, 0,
            (SCM surface,
             SCM rect),
            "Set @var{surface} clipping rectangle to the whole surface.\n"
            "Optional arg @var{rect} specifies a particular rectangle\n"
            "instead of using the whole surface.")
{
#define FUNC_NAME s_set_clip_rect
  SDL_Rect *crect = NULL;

  ASSERT_SURFACE (surface, ARGH1);

  if (BOUNDP (rect))
    {
      /* Rect defaults to NULL (the whole surface).  */
      ASSERT_RECT (rect, ARGH2);
      crect = UNPACK_RECT (rect);
    }

  SDL_SetClipRect (UNPACK_SURFACE (surface), crect);
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


GH_DEFPROC (get_clip_rect, "get-clip-rect", 1, 0, 0,
            (SCM surface),
            "Return the clipping rectangle for @var{surface}.")
{
#define FUNC_NAME s_get_clip_rect
  SDL_Rect *rect = NULL;

  ASSERT_SURFACE (surface, ARGH1);

  SDL_GetClipRect (UNPACK_SURFACE (surface), rect);
  RETURN_NEW_RECT (rect);
#undef FUNC_NAME
}


GH_DEFPROC (convert_surface, "convert-surface", 2, 1, 0,
            (SCM surface,
             SCM format,
             SCM flags),
            "Convert @var{surface} to the same @var{format} as another\n"
            "surface.  Optional third arg @var{flags} is a list of flags\n"
            "(symbols) from the set returned by @code{flagstash:video}.")
{
#define FUNC_NAME s_convert_surface
  Uint32 cflags = 0;

  ASSERT_SURFACE (surface, ARGH1);
  ASSERT_PIXEL_FORMAT (format, ARGH2);

  if (BOUNDP (flags))
    cflags = (Uint32) GSDL_FLAGS2ULONG (flags, gsdl_video_flags, ARGH3);

  RETURN_NEW_SURFACE
    (SDL_ConvertSurface (UNPACK_SURFACE (surface),
                         UNPACK_PIXEL_FORMAT (format),
                         cflags));
#undef FUNC_NAME
}


GH_DEFPROC (blit_surface, "blit-surface", 1, 3, 0,
            (SCM src,
             SCM srcrect,
             SCM dst,
             SCM dstrect),
            "Perform a fast blit from the\n"
            "@var{src} surface @var{srcrect} to the\n"
            "@var{dst} surface @var{dstrect}.\n"
            "@var{srcrect} defaults to x=0, y=0, @var{src} surface\n"
            "dimensions.  If unspecified @var{dst} is taken as\n"
            "the default video surface.  @var{dstrect} likewise defaults\n"
            "to x=0, y=0, @var{dst} surface dimensions.")
{
#define FUNC_NAME s_blit_surface
  SDL_Surface *csrc;
  SDL_Surface *cdst;
  SDL_Rect *csrcrect;
  SDL_Rect *cdstrect;
  SDL_Rect default_rect;

  /* 1st arg, source surface.  */
  ASSERT_SURFACE (src, ARGH1);
  csrc = UNPACK_SURFACE (src);

  /* 2nd arg, source rect, default (0,0) by source dimensions.  */
  UNBOUND_MEANS_FALSE (srcrect);
  if (NOT_FALSEP (srcrect))
    {
      ASSERT_RECT (srcrect, ARGH2);
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
      ASSERT_SURFACE (dst, ARGH3);
      cdst = UNPACK_SURFACE (dst);
    }
  else
    cdst = SDL_GetVideoSurface ();

  /* 4th arg, dest rect, default src rect.  */
  UNBOUND_MEANS_FALSE (dstrect);
  if (NOT_FALSEP (dstrect))
    {
      ASSERT_RECT (dstrect, ARGH4);
      cdstrect = UNPACK_RECT (dstrect);
    }
  else
    cdstrect = csrcrect;

  RETURN_INT (SDL_BlitSurface (csrc, csrcrect, cdst, cdstrect));
#undef FUNC_NAME
}


void
gsdl_init_surface (void)
{
  surface_tag = scm_make_smob_type ("SDL-Surface", sizeof (SDL_Surface));
  scm_set_smob_mark  (surface_tag, mark_surface);
  scm_set_smob_free  (surface_tag, free_surface);
  scm_set_smob_print (surface_tag, print_surface);

#include "sdlsurface.x"
}

/* sdlsurface.c ends here */
