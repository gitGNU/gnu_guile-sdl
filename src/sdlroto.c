/* sdl.c --- SDL Roto functions for Guile
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

#include "SDL_rotozoom.h"

#include "config.h"
#include "argcheck.h"
#include "sdlsmobs.h"
#include "bool.h"


GH_DEFPROC (roto_zoom_surface, "roto-zoom-surface", 2, 2, 0,
            (SCM surface,
             SCM angle,
             SCM zoom,
             SCM smooth),
            "Return a new surface made from rotating @var{surface}\n"
            "by @var{angle} degrees.  Optional third arg @var{zoom}\n"
            "(default value 1.0) changes the size as well.  Optional\n"
            "fourth arg @var{smooth} turns on anti-aliasing.")
{
#define FUNC_NAME s_roto_zoom_surface
  SDL_Surface *csurface, *new_surface;
  double cangle = 0.0, czoom = 1.0;

  ASSERT_SURFACE (surface, ARGH1);
  csurface = SMOBGET (surface, SDL_Surface *);

  ASSERT_NUMBER (angle, ARGH2);
  cangle = gh_scm2double (angle);

  UNBOUND_MEANS_FALSE (zoom);
  if (NOT_FALSEP (zoom))
    {
      ASSERT_NUMBER (zoom, ARGH3);
      czoom = gh_scm2double (zoom);
    }

  UNBOUND_MEANS_FALSE (smooth);

  new_surface = rotozoomSurface (csurface, cangle, czoom, NOT_FALSEP (smooth));

  RETURN_NEW_SURFACE (new_surface);
#undef FUNC_NAME
}


GH_DEFPROC (zoom_surface, "zoom-surface", 2, 2, 0,
            (SCM surface,
             SCM zoomx,
             SCM zoomy,
             SCM smooth),
            "Return a new scaled copy of @var{surface}.\n"
            "@var{zoomx} and @var{zoomy} specify the scaling factor.\n"
            "If omitted, @var{zoomy} defaults to @var{zoomx}.\n"
            "Optional fourth arg @var{smooth} turns on anti-aliasing.")
{
#define FUNC_NAME s_zoom_surface
  SDL_Surface *csurface, *new_surface;
  double czoomx = 1.0, czoomy = 1.0;

  ASSERT_SURFACE (surface, ARGH1);
  csurface = SMOBGET (surface, SDL_Surface *);

  ASSERT_NUMBER (zoomx, ARGH2);
  czoomx = gh_scm2double (zoomx);

  UNBOUND_MEANS_FALSE (zoomy);
  if (NOT_FALSEP (zoomy))
    {
      ASSERT_NUMBER (zoomy, ARGH3);
      czoomy = gh_scm2double (zoomy);
    }
  else
    czoomy = czoomx;

  UNBOUND_MEANS_FALSE (smooth);

  new_surface = zoomSurface (csurface, czoomx, czoomy, NOT_FALSEP (smooth));

  RETURN_NEW_SURFACE (new_surface);
#undef FUNC_NAME
}


void
gsdl_init_rotozoom (void)
{
#include "sdlroto.x"
}

/* sdlroto.c ends here */
