/*******************************************************************
 *  sdlsurface.c -- SDL Surface functions                          *
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
#include "sdlvideo.h"
#include "sdlsurface.h"

/* tags for SDL smobs */
long surface_tag;


/* smob functions */

size_t
free_surface (SCM surface)
{
  /* printf ("free_surface(%p)\n", (SDL_Surface*) SCM_SMOB_DATA (surface)); */
  SDL_FreeSurface ((SDL_Surface*) SCM_SMOB_DATA (surface));
  return sizeof (SDL_Surface);
}

int
print_surface (SCM surface_smob, SCM port, scm_print_state *pstate)
{
  SDL_Surface *surface = (SDL_Surface *) SCM_SMOB_DATA (surface_smob);
  SCM w,h, bpp;

  if (surface != NULL) {
    
    w = scm_long2num (surface->w);
    h = scm_long2num (surface->h);
    bpp = scm_long2num (surface->format->BitsPerPixel);

    scm_puts ("#<SDL-Surface ", port);
    scm_display(w, port);
    scm_puts ("x", port);
    scm_display(h, port);
    scm_puts (" ", port);
    
    scm_display(bpp, port);
    scm_puts (" bpp", port);
    scm_puts (">", port);
  }
  else {
    scm_puts ("#<SDL-Surface NULL>", port);
  }
  
  /* non-zero means success */
  return 1;
}


/* constructors */

SCM_DEFINE( sdl_make_surface, "sdl-make-surface", 2, 1, 0,
            (SCM s_width,
             SCM s_height,
             SCM s_flags),
"Create an empty SDL-Surface
Takes 2 arguments, width and height, plus an optional third argument
of video flags.  Color depth and masks will be those for the current
video surface.")
#define FUNC_NAME s_sdl_make_surface
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

  width  = scm_num2long (s_width, SCM_ARG1, "sdl-make-surface");
  height = scm_num2long (s_height, SCM_ARG2, "sdl-make-surface");

  /* flags are optional, defaulting to current screen flags */
  if (s_flags == SCM_UNDEFINED) {
    flags = SDL_GetVideoSurface()->flags;
  } else {
    flags = (Uint32) scm_flags2ulong (s_flags, sdl_video_flags,
                                      SCM_ARG3, "sdl-make-surface");
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
#undef FUNC_NAME


SCM_DEFINE( sdl_create_rgb_surface, "sdl-create-rgb-surface", 8, 0, 0,
            (SCM s_flags,
             SCM s_width,
             SCM s_height,
             SCM s_depth,
             SCM s_rmask,
             SCM s_gmask,
             SCM s_bmask,
             SCM s_amask),
"Create an empty sdl-surface.
Takes 8 arguments, directly analagous to those for SDL_CreateRGBSurface.")
#define FUNC_NAME s_sdl_create_rgb_surface
{
  /* surface to make */
  SDL_Surface *surface;
  /* params */
  int width, height;
  Uint32 flags;
  Uint8 depth;
  Uint32 rmask, gmask, bmask, amask;

  /* validate params */
  SCM_ASSERT (scm_exact_p (s_width),  s_width,  SCM_ARG2, "sdl-create-rgb-surface");
  SCM_ASSERT (scm_exact_p (s_height), s_height, SCM_ARG3, "sdl-create-rgb-surface");
  SCM_ASSERT (scm_exact_p (s_depth),  s_depth,  SCM_ARG4, "sdl-create-rgb-surface");
  SCM_ASSERT (scm_exact_p (s_rmask),  s_rmask,  SCM_ARG5, "sdl-create-rgb-surface");
  SCM_ASSERT (scm_exact_p (s_gmask),  s_gmask,  SCM_ARG6, "sdl-create-rgb-surface");
  SCM_ASSERT (scm_exact_p (s_bmask),  s_bmask,  SCM_ARG7, "sdl-create-rgb-surface");
  SCM_ASSERT (scm_exact_p (s_amask),  s_amask,  SCM_ARGn, "sdl-create-rgb-surface");

  flags  = (Uint32) scm_flags2ulong (s_flags, sdl_video_flags,
                                     SCM_ARG1, "sdl-create-rgb-surface");
  width  = scm_num2long (s_width, SCM_ARG2, "sdl-create-rgb-surface");
  height = scm_num2long (s_height, SCM_ARG3, "sdl-create-rgb-surface");
  depth  = (Uint8)  scm_num2long (s_depth, SCM_ARG4, "sdl-create-rgb-surface");
  rmask  = (Uint32) scm_num2long (s_rmask, SCM_ARG5, "sdl-create-rgb-surface");
  gmask  = (Uint32) scm_num2long (s_gmask, SCM_ARG6, "sdl-create-rgb-surface");
  bmask  = (Uint32) scm_num2long (s_bmask, SCM_ARG7, "sdl-create-rgb-surface");
  amask  = (Uint32) scm_num2long (s_amask, SCM_ARGn, "sdl-create-rgb-surface");

  /* create the surface */
  surface = SDL_CreateRGBSurface (SDL_HWSURFACE, width, height, depth,
                                  rmask, gmask, bmask, amask);

  /* return a newly allocated smob */
  SCM_RETURN_NEWSMOB (surface_tag, surface);
}
#undef FUNC_NAME


/* SCM */
/* create_rgb_surface_from (SCM s_pixels, SCM s_width, SCM s_height, */
/*                          SCM s_depth, SCM s_pitch, SCM s_rmask, */
/*                          SCM s_gmask, SCM s_bmask, SCM s_amask) */
/* { */
/*    return SCM_UNSPECIFIED; */
/* } */

/* accessors */

SCM_DEFINE_NUMBER_GETTER ("sdl-surface:w", surface_get_w, surface_tag, SDL_Surface*, w)
SCM_DEFINE_NUMBER_GETTER ("sdl-surface:h", surface_get_h, surface_tag, SDL_Surface*, h)
SCM_DEFINE_FLAG_GETTER ("sdl-surface:flags", surface_get_flags, surface_tag,
                        SDL_Surface*, flags, sdl_video_flags)
SCM_DEFINE_NUMBER_GETTER ("sdl-surface:depth", surface_get_depth, surface_tag,
                        SDL_Surface*, format->BitsPerPixel)

SCM
surface_get_format (SCM s_surface)
{
  SDL_Surface *surface;
  SDL_PixelFormat *format;

  SCM_ASSERT_SMOB (s_surface, surface_tag, SCM_ARG1, "sdl-surface:format");
  surface = (SDL_Surface*) SCM_SMOB_DATA (s_surface);

  format = surface->format;
  SCM_RETURN_NEWSMOB (pixel_format_tag, format);
}


/* utilities */

SCM_DEFINE( sdl_surface_p, "sdl-surface?", 1, 0, 0,
            (SCM s_surface),
"Returns true iff argument is a surface.")
#define FUNC_NAME s_sdl_surface_p
{
  return SMOB_SURFACEP (s_surface) ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE( sdl_lock_surface, "sdl-lock-surface", 1, 0, 0,
            (SCM s_surface),
"Lock a surface for direct access.")
#define FUNC_NAME s_sdl_lock_surface
{
  SDL_Surface *surface;

  SCM_ASSERT_SMOB (s_surface, surface_tag, SCM_ARG1, "sdl-lock-surface");
  surface = (SDL_Surface*) SCM_SMOB_DATA (s_surface);

  SCM_RETURN_TRUE_IF_0 (SDL_LockSurface (surface));
}
#undef FUNC_NAME


SCM_DEFINE( sdl_unlock_surface, "sdl-unlock-surface", 1, 0, 0,
            (SCM s_surface),
"Unlocks a previously locked surface.")
#define FUNC_NAME s_sdl_unlock_surface
{
  SDL_Surface *surface;

  SCM_ASSERT_SMOB (s_surface, surface_tag, SCM_ARG1, "sdl-unlock-surface");
  surface = (SDL_Surface*) SCM_SMOB_DATA (s_surface);

  SDL_UnlockSurface (surface);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE( sdl_load_bmp, "sdl-load-bmp", 1, 0, 0,
            (SCM s_file),
"Loads a simple bitmap from a file.")
#define FUNC_NAME s_sdl_load_bmp
{
  SDL_Surface *surface;
  const char *file;

  SCM_ASSERT (SCM_STRINGP (s_file), s_file, SCM_ARG1, "sdl-load-bmp");
  file = SCM_STRING_CHARS (s_file);

  surface = SDL_LoadBMP (file);

  SCM_RETURN_NEWSMOB (surface_tag, surface);
}
#undef FUNC_NAME


/* Load an image in one of many formats */
SCM_DEFINE( sdl_load_image, "sdl-load-image", 1, 0, 0,
            (SCM s_file),
"Loads an image from a file.")
#define FUNC_NAME s_sdl_load_bmp
{
  SDL_Surface *image;

  SCM_ASSERT ((SCM_NIMP (s_file) && SCM_STRINGP (s_file)),
              s_file, SCM_ARG1, "sdl-load-image");

  image = IMG_Load (SCM_STRING_CHARS (s_file));
  SCM_RETURN_NEWSMOB (surface_tag, image);
}
#undef FUNC_NAME


SCM_DEFINE( sdl_save_bmp, "sdl-save-bmp", 2, 0, 0,
            (SCM s_surface,
             SCM s_file),
"Save an SDL_Surface as a Windows BMP file.")
#define FUNC_NAME s_sdl_save_bmp
{
  SDL_Surface *surface;
  const char *file;

  SCM_ASSERT_SMOB (s_surface, surface_tag,  SCM_ARG1, "sdl-save-bmp");
  SCM_ASSERT (SCM_STRINGP (s_file), s_file, SCM_ARG2, "sdl-save-bmp");

  surface = (SDL_Surface*) SCM_SMOB_DATA (s_surface);
  file = SCM_STRING_CHARS (s_file);

  SCM_RETURN_TRUE_IF_0 (SDL_SaveBMP (surface, file));
}
#undef FUNC_NAME


SCM_DEFINE( sdl_set_color_key, "sdl-set-color-key!", 3, 0, 0,
            (SCM s_surface,
             SCM s_flag,
             SCM s_key),
"Sets the color key in a blittable surface and RLE acceleration.")
#define FUNC_NAME s_sdl_set_color_key
{
  SDL_Surface *surface;
  Uint32 flag, key;

  SCM_ASSERT_SMOB (s_surface, surface_tag, SCM_ARG1, "sdl-set-color-key!");
  SCM_ASSERT (scm_exact_p (s_key),  s_key,   SCM_ARG3, "sdl-set-color-key!");

  surface = (SDL_Surface*) SCM_SMOB_DATA (s_surface);
  flag = (Uint32) scm_flags2ulong (s_flag, sdl_video_flags,
                                   SCM_ARG2, "sdl-set-color-key!");
  key  = (Uint32) scm_num2long (s_key, SCM_ARG3, "sdl-set-color-key!");

  SCM_RETURN_TRUE_IF_0 (SDL_SetColorKey (surface, flag, key));
}
#undef FUNC_NAME


SCM_DEFINE( sdl_set_alpha, "sdl-set-alpha!", 3, 0, 0,
            (SCM s_surface,
             SCM s_flag,
             SCM s_alpha),
"Adjust the alpha properties of a surface.")
{
  SDL_Surface *surface;
  Uint32 flag;
  Uint8 alpha;

  SCM_ASSERT_SMOB (s_surface, surface_tag,  SCM_ARG1, "sdl-set-alpha!");
  SCM_ASSERT (scm_exact_p (s_flag), s_flag,   SCM_ARG2, "sdl-set-alpha!");
  SCM_ASSERT (scm_exact_p (s_alpha), s_alpha, SCM_ARG3, "sdl-set-alpha!");

  surface = (SDL_Surface*) SCM_SMOB_DATA (s_surface);
  flag  = (Uint32) scm_flags2ulong (s_flag, sdl_video_flags,
                                    SCM_ARG2, "sdl-set-alpha!");
  alpha = (Uint8) scm_enum2long (s_alpha, sdl_alpha_enums,
                                 SCM_ARG3, "sdl-set-alpha!");

  SCM_RETURN_TRUE_IF_0 (SDL_SetAlpha (surface, flag, alpha));
}
#undef FUNC_NAME


SCM_DEFINE( sdl_set_clip_rect, "sdl-set-clip-rect!", 2, 0, 0,
            (SCM s_surface,
             SCM s_rect),
"Sets the clipping rectangle for a surface.")
#define FUNC_NAME s_sdl_set_clip_rect
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
#undef FUNC_NAME


SCM_DEFINE( sdl_get_clip_rect, "sdl-get-clip-rect", 1, 0, 0,
            (SCM s_surface),
"Gets the clipping rectangle for a surface.")
#define FUNC_NAME s_sdl_get_clip_rect
{
  SDL_Surface *surface;
  SDL_Rect *rect;

  SCM_ASSERT_SMOB (s_surface, surface_tag,  SCM_ARG1, "sdl-get-clip-rect");

  surface = (SDL_Surface*) SCM_SMOB_DATA (s_surface);

  SDL_GetClipRect (surface, rect);

  SCM_RETURN_NEWSMOB (rect_tag, rect);
}
#undef FUNC_NAME


SCM_DEFINE( sdl_convert_surface, "sdl-convert-surface", 2, 1, 0,
            (SCM s_src,
             SCM s_fmt,
             SCM s_flags),
"Converts a surface to the same format as another surface.")
#define FUNC_NAME s_sdl_convert_surface
{
  SDL_Surface *src, *result;
  SDL_PixelFormat *fmt;
  Uint32 flags=0;

  SCM_ASSERT_SMOB (s_src, surface_tag,  SCM_ARG1, "sdl-convert-surface");
  SCM_ASSERT_SMOB (s_fmt, pixel_format_tag, SCM_ARG2, "sdl-convert-surface");

  src = (SDL_Surface*) SCM_SMOB_DATA (s_src);
  fmt = (SDL_PixelFormat*) SCM_SMOB_DATA (s_fmt);

  if (s_flags != SCM_UNDEFINED) {
    flags = (Uint32) scm_flags2ulong (s_flags, sdl_video_flags,
                                      SCM_ARG3, "sdl-convert-surface");
  }

  result = SDL_ConvertSurface (src, fmt, flags);

  SCM_RETURN_NEWSMOB (surface_tag, result);
}
#undef FUNC_NAME


SCM_DEFINE( sdl_blit_surface, "sdl-blit-surface", 1, 3, 0,
	    (SCM s_src,
             SCM s_srcrect,
	     SCM s_dst,
             SCM s_dstrect),
"Performs a fast blit from the source surface to the destination surface.")
#define FUNC_NAME s_sdl_blit_surface
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
    dstrect = srcrect;
  }

  return scm_long2num (SDL_BlitSurface (src, srcrect, dst, dstrect));
}
#undef FUNC_NAME


void
sdl_init_surface (void)
{
  /* smobs */
  surface_tag = scm_make_smob_type ("SDL-Surface", sizeof(SDL_Surface));
  scm_set_smob_free (surface_tag, free_surface);
  scm_set_smob_print (surface_tag, print_surface);

  /* surfaces */
  scm_c_define_gsubr ("sdl-surface:w",          1, 0, 0, surface_get_w);
  scm_c_define_gsubr ("sdl-surface:h",          1, 0, 0, surface_get_h);
  scm_c_define_gsubr ("sdl-surface:depth",      1, 0, 0, surface_get_depth);
  scm_c_define_gsubr ("sdl-surface:flags",      1, 0, 0, surface_get_flags);
  scm_c_define_gsubr ("sdl-surface:format",     1, 0, 0, surface_get_format);

  /* exported symbols */
  scm_c_export (
    "sdl-surface:w",          "sdl-surface:h",
    "sdl-surface:depth",      "sdl-surface:flags",
    "sdl-surface:format",     "sdl-make-surface",
    "sdl-create-rgb-surface", "sdl-lock-surface",
    "sdl-unlock-surface",     "sdl-set-clip-rect!",
    "sdl-get-clip-rect",      "sdl-set-color-key!",
    "sdl-set-alpha!",         "sdl-convert-surface",
    "sdl-load-bmp",           "sdl-save-bmp",
    "sdl-blit-surface",       "sdl-load-image",
    NULL);

#ifndef SCM_MAGIC_SNARFER
#include "sdlsurface.x"
#endif

}

