/* sdl.c --- SDL Roto functions for Guile
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

#include "SDL_rotozoom.h"

#include "config.h"
#include "argcheck.h"
#include "sdlsmobs.h"


MDEFLOCEXP (sdl_roto_zoom_surface, "sdl-roto-zoom-surface", 2, 2, 0,
            (SCM surface_smob,
             SCM s_angle,
             SCM s_zoom,
             SCM s_smooth),
            "Returns a new rotated and zoomed copy of a surface.\n"
            "Zoom defaults to 1.0, and smooth defaults to #f.")
#define FUNC_NAME s_sdl_roto_zoom_surface
{
  SDL_Surface *surface, *new_surface;
  double angle=0.0, zoom=1.0;

  ASSERT_SURFACE (surface_smob, ARGH1);
  surface = SMOBGET (surface_smob, SDL_Surface *);

  ASSERT_NUMBER (s_angle, ARGH2);
  angle = gh_scm2double (s_angle);

  if (BOUNDP (s_zoom)) {
    ASSERT_NUMBER (s_zoom, ARGH3);
    angle = gh_scm2double (s_angle);
  }

  UNBOUND_MEANS_FALSE (s_smooth);

  new_surface = rotozoomSurface (surface, angle, zoom, SCM_NFALSEP (s_smooth));

  RETURN_NEW_SURFACE (new_surface);
}
#undef FUNC_NAME


MDEFLOCEXP (sdl_zoom_surface, "sdl-zoom-surface", 2, 2, 0,
            (SCM surface_smob,
             SCM s_zoomx,
             SCM s_zoomy,
             SCM s_smooth),
            "Return a new scaled copy of @var{surface}.\n"
            "@var{zoomx} and @var{zoomy} specify the scaling factor.\n"
            "If omitted, @var{zoomy} defaults to @var{zoomx}.\n"
            "Optional fourth arg @var{smooth} turns on anti-aliasing.")
#define FUNC_NAME s_sdl_zoom_surface
{
  SDL_Surface *surface, *new_surface;
  double zoomx=1.0, zoomy=1.0;

  ASSERT_SURFACE (surface_smob, ARGH1);
  surface = SMOBGET (surface_smob, SDL_Surface *);

  ASSERT_NUMBER (s_zoomx, ARGH2);
  zoomx = gh_scm2double (s_zoomx);

  if (BOUNDP (s_zoomy)) {
    ASSERT_NUMBER (s_zoomy, ARGH3);
    zoomy = gh_scm2double (s_zoomy);
  } else {
    zoomy = zoomx;
  }

  UNBOUND_MEANS_FALSE (s_smooth);

  new_surface = zoomSurface (surface, zoomx, zoomy, SCM_NFALSEP (s_smooth));

  RETURN_NEW_SURFACE (new_surface);
}
#undef FUNC_NAME


MDEFLOCEXP (vertical_flip_surface, "sdl-vertical-flip-surface", 1, 0, 0,
            (SCM s_surface),
            "Return a new vertically flipped copy of a @var{surface}.")
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
  RETURN_NEW_SURFACE (dst);
}
#undef FUNC_NAME


MDEFLOCEXP (horiztonal_flip_surface, "sdl-horizontal-flip-surface", 1, 0, 0,
            (SCM s_surface),
            "Return a new horizontally flipped copy of @var{surface}.")
#define FUNC_NAME s_horiztonal_flip_surface
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
  RETURN_NEW_SURFACE (dst);
}
#undef FUNC_NAME


MDEFLOCEXP (vh_flip_surface, "sdl-vh-flip-surface", 1, 0, 0,
            (SCM s_surface),
            "Return a new surface created by flipping @var{surface}\n"
            "both vertically and horizontally.")
#define FUNC_NAME s_vh_flip_surface
{
  SCM temp = vertical_flip_surface (s_surface);
  return horiztonal_flip_surface (temp);
}
#undef FUNC_NAME


void
gsdl_init_rotozoom (void)
{
#include "sdlroto.x"
}

/* sdlroto.c ends here */
