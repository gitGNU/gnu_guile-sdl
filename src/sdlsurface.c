/* sdlsurface.c --- SDL Surface functions
 *
 * 	Copyright (C) 2003,2004 Thien-Thi Nguyen
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
#include "wholefns.h"

#include "sdlenums.h"
#include "sdlsmobs.h"
#include "sdlvideo.h"
#include "retval.h"


/* smob functions */

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

  scm_puts                                 ("#<SDL-Surface ", port);

  if (surface) {
    scm_display                    (gh_long2scm (surface->w), port);
    scm_puts                                            ("x", port);
    scm_display                    (gh_long2scm (surface->h), port);
    scm_puts                                            (" ", port);
    scm_display (gh_long2scm (surface->format->BitsPerPixel), port);
    scm_puts                                         (" bpp", port);
  }
  else {
    scm_puts                                         ("NULL", port);
  }
  scm_puts                                              (">", port);


  /* non-zero means success */
  return 1;
}


/* constructors */

GH_DEFPROC (make_surface, "make-surface", 2, 1, 0,
            (SCM s_width,
             SCM s_height,
             SCM s_flags),
            "Return an empty surface of width @var{w} and height @var{h}.\n"
            "Optional third arg @var{flags} (see @code{flagstash:video})\n"
            "further specifies the surface.  Color depth and masks\n"
            "are those for the current video surface.")
#define FUNC_NAME s_make_surface
{
  Uint32 flags;
  const SDL_PixelFormat *fmt;

  ASSERT_EXACT (s_width,  ARGH1);
  ASSERT_EXACT (s_height, ARGH2);

  flags = (UNBOUNDP (s_flags)
           ? SDL_GetVideoSurface ()->flags
           : GSDL_FLAGS2ULONG (s_flags, gsdl_video_flags, ARGH3));

  fmt = SDL_GetVideoInfo ()->vfmt;

  /* return a newly allocated surface smob */
  RETURN_NEW_SURFACE
    (SDL_CreateRGBSurface (SDL_HWSURFACE,
                           gh_scm2long (s_width), gh_scm2long (s_height),
                           /* defaults from current video info */
                           (Uint8)  fmt->BitsPerPixel,
                           (Uint32) fmt->Rmask,
                           (Uint32) fmt->Gmask,
                           (Uint32) fmt->Bmask,
                           (Uint32) fmt->Amask));
}
#undef FUNC_NAME


GH_DEFPROC (create_rgb_surface, "create-rgb-surface", 8, 0, 0,
            (SCM s_flags,
             SCM s_width, SCM s_height, SCM s_depth,
             SCM s_rmask, SCM s_gmask, SCM s_bmask, SCM s_amask),
            "Return an empty surface.\n"
            "The eight arguments, directly analagous to those\n"
            "for SDL_CreateRGBSurface, are: @var{flags}\n"
            "(list of symbols, see @code{flagstash:video}),\n"
            "@var{width}, @var{height}, @var{depth}, @var{rmask},\n"
            "@var{gmask}, @var{bmask}, @var{amask}\n"
            "(all numbers).")
#define FUNC_NAME s_create_rgb_surface
{
  Uint32 flags;

  ASSERT_EXACT (s_width,  ARGH2);
  ASSERT_EXACT (s_height, ARGH3);
  ASSERT_EXACT (s_depth,  ARGH4);
  ASSERT_EXACT (s_rmask,  ARGH5);
  ASSERT_EXACT (s_gmask,  ARGH6);
  ASSERT_EXACT (s_bmask,  ARGH7);
  ASSERT_EXACT (s_amask,  ARGHn);

  flags = (Uint32) GSDL_FLAGS2ULONG (s_flags, gsdl_video_flags, ARGH1);

  /* return a newly allocated surface smob */
  RETURN_NEW_SURFACE
    (SDL_CreateRGBSurface (SDL_HWSURFACE,
                           gh_scm2long (s_width), gh_scm2long (s_height),
                           (Uint8)  gh_scm2long (s_depth),
                           (Uint32) gh_scm2long (s_rmask),
                           (Uint32) gh_scm2long (s_gmask),
                           (Uint32) gh_scm2long (s_bmask),
                           (Uint32) gh_scm2long (s_amask)));
}
#undef FUNC_NAME

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
            (SCM s_surface),
            "")
#define FUNC_NAME s_surface_get_format
{
  ASSERT_SURFACE (s_surface, ARGH1);

  RETURN_NEW_PIXEL_FORMAT (UNPACK_SURFACE (s_surface)->format);
}
#undef FUNC_NAME


/* utilities */

GH_DEFPROC (surface_p, "surface?", 1, 0, 0,
            (SCM obj),
            "Return true iff @var{obj} is a surface.")
#define FUNC_NAME s_surface_p
{
  RETURN_BOOL
    (SCM_SMOB_PREDICATE (surface_tag, obj));
}
#undef FUNC_NAME


GH_DEFPROC (lock_surface, "lock-surface", 1, 0, 0,
            (SCM s_surface),
            "Lock a @var{surface} for direct access.\n"
            "Return #t if successful.")
#define FUNC_NAME s_lock_surface
{
  ASSERT_SURFACE (s_surface, ARGH1);

  RETURN_TRUE_IF_0
    (SDL_LockSurface (UNPACK_SURFACE (s_surface)));
}
#undef FUNC_NAME


GH_DEFPROC (unlock_surface, "unlock-surface", 1, 0, 0,
            (SCM s_surface),
            "Unlock a previously locked @var{surface}.\n"
            "The return value is unspecified.")
#define FUNC_NAME s_unlock_surface
{
  ASSERT_SURFACE (s_surface, ARGH1);

  SDL_UnlockSurface (UNPACK_SURFACE (s_surface));
  RETURN_UNSPECIFIED;
}
#undef FUNC_NAME


GH_DEFPROC (load_bmp, "load-bmp", 1, 0, 0,
            (SCM s_file),
            "Return a surface made by loading the bitmap @var{file}.")
#define FUNC_NAME s_load_bmp
{
  ASSERT_STRING (s_file, ARGH1);

  RETURN_NEW_SURFACE (SDL_LoadBMP (SCM_CHARS (s_file)));
}
#undef FUNC_NAME


/* Load an image in one of many formats */
GH_DEFPROC (load_image, "load-image", 1, 0, 0,
            (SCM s_file),
            "Return a surface made by loading the image @var{file}.")
#define FUNC_NAME s_load_bmp
{
  ASSERT_STRING (s_file, ARGH1);

  RETURN_NEW_SURFACE (IMG_Load (SCM_CHARS (s_file)));
}
#undef FUNC_NAME


GH_DEFPROC (save_bmp, "save-bmp", 2, 0, 0,
            (SCM s_surface,
             SCM s_file),
            "Save @var{surface} to @var{file} in Windows BMP format.\n"
            "Return #t if successful.")
#define FUNC_NAME s_save_bmp
{
  ASSERT_SURFACE (s_surface,  ARGH1);
  ASSERT_STRING (s_file, ARGH2);

  RETURN_TRUE_IF_0
    (SDL_SaveBMP (UNPACK_SURFACE (s_surface),
                  SCM_CHARS (s_file)));
}
#undef FUNC_NAME


GH_DEFPROC (set_color_key, "set-color-key!", 3, 0, 0,
            (SCM s_surface,
             SCM s_flag,
             SCM s_key),
            "Set @var{surface} color key as specified by @var{flag}\n"
            "(see @code{flagstash:video}) and @var{key}.")
#define FUNC_NAME s_set_color_key
{
  Uint32 flag;

  ASSERT_SURFACE (s_surface, ARGH1);
  ASSERT_EXACT (s_key, ARGH3);

  flag = (Uint32) GSDL_FLAGS2ULONG (s_flag, gsdl_video_flags, ARGH2);

  RETURN_TRUE_IF_0
    (SDL_SetColorKey (UNPACK_SURFACE (s_surface),
                      flag,
                      gh_scm2long (s_key)));
}
#undef FUNC_NAME


GH_DEFPROC (set_alpha, "set-alpha!", 3, 0, 0,
            (SCM s_surface,
             SCM s_flag,
             SCM s_alpha),
            "Adjust @var{surface} alpha properties as specified by\n"
            "@var{flag} (see @code{flagstash:video}) and @var{alpha}.")
#define FUNC_NAME s_set_alpha
{
  Uint32 flag;
  Uint8 alpha;

  ASSERT_SURFACE (s_surface, ARGH1);
  /* hmmm, why was this here? --ttn */
  /* ASSERT_EXACT (s_flag, ARGH2); */
  ASSERT_EXACT (s_alpha, ARGH3);

  flag  = (Uint32) GSDL_FLAGS2ULONG (s_flag, gsdl_video_flags, ARGH2);
  alpha = (Uint8) gsdl_enum2long (s_alpha, gsdl_alpha_enums, ARGH3, FUNC_NAME);

  RETURN_TRUE_IF_0
    (SDL_SetAlpha (UNPACK_SURFACE (s_surface),
                   flag,
                   alpha));
}
#undef FUNC_NAME


GH_DEFPROC (set_clip_rect, "set-clip-rect!", 1, 1, 0,
            (SCM s_surface,
             SCM s_rect),
            "Set @var{surface} clipping rectangle to the whole surface.\n"
            "Optional arg @var{rect} specifies a particular rectangle\n"
            "instead of using the whole surface.")
#define FUNC_NAME s_set_clip_rect
{
  SDL_Rect *rect = NULL;

  ASSERT_SURFACE (s_surface, ARGH1);

  if (BOUNDP (s_rect)) {
    /* rect defaults to NULL (the whole surface) */
    ASSERT_RECT (s_rect, ARGH2);
    rect = UNPACK_RECT (s_rect);
  }

  SDL_SetClipRect (UNPACK_SURFACE (s_surface), rect);
  RETURN_UNSPECIFIED;
}
#undef FUNC_NAME


GH_DEFPROC (get_clip_rect, "get-clip-rect", 1, 0, 0,
            (SCM s_surface),
            "Return the clipping rectangle for @var{surface}.")
#define FUNC_NAME s_get_clip_rect
{
  SDL_Rect *rect = NULL;

  ASSERT_SURFACE (s_surface, ARGH1);

  SDL_GetClipRect (UNPACK_SURFACE (s_surface), rect);
  RETURN_NEW_RECT (rect);
}
#undef FUNC_NAME


GH_DEFPROC (convert_surface, "convert-surface", 2, 1, 0,
            (SCM s_src,
             SCM s_fmt,
             SCM s_flags),
            "Convert @var{surface} to the same @var{format} as another\n"
            "surface.  Optional third arg @var{flags} is a list of flags\n"
            "(symbols) from the set returned by @code{flagstash:video}.")
#define FUNC_NAME s_convert_surface
{
  Uint32 flags = 0;

  ASSERT_SURFACE (s_src, ARGH1);
  ASSERT_PIXEL_FORMAT (s_fmt, ARGH2);

  if (BOUNDP (s_flags)) {
    flags = (Uint32) GSDL_FLAGS2ULONG (s_flags, gsdl_video_flags, ARGH3);
  }

  RETURN_NEW_SURFACE
    (SDL_ConvertSurface (UNPACK_SURFACE (s_src),
                         UNPACK_PIXEL_FORMAT (s_fmt),
                         flags));
}
#undef FUNC_NAME


GH_DEFPROC (blit_surface, "blit-surface", 1, 3, 0,
            (SCM s_src,
             SCM s_srcrect,
             SCM s_dst,
             SCM s_dstrect),
            "Perform a fast blit from the\n"
            "@var{src_surface}/@var{src_rect} to the\n"
            "@var{dst_surface}/@var{dst_rect}.\n"
            "@var{src_rect} defaults to x=0, y=0, @var{src_surface}\n"
            "dimensions.  If unspecified @var{dst_surface} is taken as\n"
            "the default video surface.  @var{dst_rect} likewise defaults\n"
            "to x=0, y=0, @var{dst_surface} dimensions.")
#define FUNC_NAME s_blit_surface
{
  SDL_Surface *src;
  SDL_Surface *dst;
  SDL_Rect *srcrect;
  SDL_Rect *dstrect;
  SDL_Rect default_rect;

  /* 1st arg, source surface */
  ASSERT_SURFACE (s_src, ARGH1);
  src = UNPACK_SURFACE (s_src);

  /* 2nd arg, source rect, default (0,0) by source dimensions */
  UNBOUND_MEANS_FALSE (s_srcrect);
  if (SCM_NFALSEP (s_srcrect)) {
    ASSERT_RECT (s_srcrect, ARGH2);
    srcrect = UNPACK_RECT (s_srcrect);
  } else {
    default_rect.x = 0;
    default_rect.y = 0;
    default_rect.w = src->w;
    default_rect.h = src->h;
    srcrect = &default_rect;
  }

  /* 3rd arg, dest surface, default video surface */
  UNBOUND_MEANS_FALSE (s_dst);
  if (SCM_NFALSEP (s_dst)) {
    ASSERT_SURFACE (s_dst, ARGH3);
    dst = UNPACK_SURFACE (s_dst);
  } else {
    dst = SDL_GetVideoSurface ();
  }

  /* 4th arg, dest rect, default src rect */
  UNBOUND_MEANS_FALSE (s_dstrect);
  if (SCM_NFALSEP (s_dstrect)) {
    ASSERT_RECT (s_dstrect, ARGH4);
    dstrect = UNPACK_RECT (s_dstrect);
  } else {
    dstrect = srcrect;
  }

  RETURN_INT (SDL_BlitSurface (src, srcrect, dst, dstrect));
}
#undef FUNC_NAME


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
