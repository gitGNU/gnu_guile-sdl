/* gfx.c --- Additional Graphics functions for Guile SDL
 *
 * 	Copyright (C) 2003 Thien-Thi Nguyen
 * 	Copyright (C) 2001 Alex Shinn
 *
 *  This program is free software; you can redistribute it and/or
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
#include <SDL/SDL_image.h>

#include "config.h"
#include "argcheck.h"
#include "sdlsmobs.h"


MDEFLOCEXP (vertical_flip_surface, "sdl-vertical-flip-surface", 1, 0, 0,
            (SCM s_surface),
            "Return a new surface created by flipping @var{surface} vertically.")
#define FUNC_NAME s_vertical_flip_surface
{
  int i, w, h;
  SDL_Surface *src, *dst;
  SDL_Rect srcrect, dstrect;

  /* verify args */
  ASSERT_SURFACE (s_surface, ARGH1);

  /* get source and dimensions */
  src = SMOBGET (s_surface, SDL_Surface *);
  w = src->w;
  h = src->h;

  /* create a new surface */
  dst = SDL_CreateRGBSurface (src->flags, w, h, 16, 0, 0, 0, 0);

  /* initialize the rects */
  srcrect.x = 0;  srcrect.y = 0;    srcrect.w = w;  srcrect.h = 1;
  dstrect.x = 0;  dstrect.y = h-1;  dstrect.w = w;  dstrect.h = 1;

  /* loop through, copying lines from top to bottom */
  for (i=h; i>=0; i--) {
    SDL_BlitSurface (src, &srcrect, dst, &dstrect);
    srcrect.y++;
    dstrect.y--;
  }

  /* return the surface */
  SCM_RETURN_NEWSMOB (surface_tag, dst);
}
#undef FUNC_NAME

MDEFLOCEXP (horizontal_flip_surface, "sdl-horiztonal-flip-surface", 1, 0, 0,
            (SCM s_surface),
            "Return a new surface created by flipping @var{surface} horizontally.")
#define FUNC_NAME s_horizontal_flip_surface
{
  int i, w, h;
  SDL_Surface *src, *dst;
  SDL_Rect srcrect, dstrect;

  /* verify args */
  ASSERT_SURFACE (s_surface, ARGH1);

  /* get source and dimensions */
  src = SMOBGET (s_surface, SDL_Surface *);
  w = src->w;
  h = src->h;

  /* create a new surface */
  dst = SDL_CreateRGBSurface (src->flags, w, h, 16, 0, 0, 0, 0);

  /* initialize the rects */
  srcrect.x = 0;    srcrect.y = 0;  srcrect.w = 1;  srcrect.h = h;
  dstrect.x = w-1;  dstrect.y = 0;  dstrect.w = 1;  dstrect.h = h;

  /* loop through, copying lines from left to right */
  for (i=w; i>=0; i--) {
    SDL_BlitSurface (src, &srcrect, dst, &dstrect);
    srcrect.x++;
    dstrect.x--;
  }

  /* return the surface */
  SCM_RETURN_NEWSMOB (surface_tag, dst);
}
#undef FUNC_NAME

MDEFLOCEXP (vh_flip_surface, "sdl-vh-flip-surface", 1, 0, 0,
            (SCM s_surface),
            "Return a new surface created by flipping @var{surface}\n"
            "both vertically and horizontally.")
#define FUNC_NAME s_vh_flip_surface
{
  SCM temp = vertical_flip_surface (s_surface);
  return horizontal_flip_surface (temp);
}
#undef FUNC_NAME

MDEFLOCEXP (scale_surface, "sdl-scale-surface", 3, 0, 0,
            (SCM surface, SCM width, SCM height),
            "Scale @var{surface} by @var{width} and @var{height}.")
#define FUNC_NAME s_scale_surface
{
  scm_misc_error (FUNC_NAME, "not yet implemented (sorry)", SCM_EOL);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



void
gsdl_gfx_init (void)
{
#include "sdlgfx.x"
}

/* sdlgfx.c ends here */
