/*******************************************************************
 *  gfx.c -- Additional Graphics functions for Guile SDL           *
 *                                                                 *
 *  Created:    <2001-06-03 02:00:32 foof>                         *
 *  Time-stamp: <2001-06-03 16:41:33 foof>                         *
 *  Author:     Alex Shinn <foof@debian.org>                       *
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

#include "gfx.h"

SCM
vertical_flip_surface (SCM s_surface)
{
   int i, w, h;
   SDL_Surface *src, *dst;
   SDL_Rect srcrect, dstrect;

   /* verify args */
   SCM_ASSERT ((SCM_NIMP (s_surface)
                && (long) SCM_CAR (s_surface) == surface_tag),
               s_surface, SCM_ARG1, "vertical-flip-surface");

   /* get source and dimensions */
   src = (SDL_Surface *) SCM_CDR (s_surface);
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

SCM
horiztonal_flip_surface (SCM s_surface)
{
   int i, w, h;
   SDL_Surface *src, *dst;
   SDL_Rect srcrect, dstrect;

   /* verify args */
   SCM_ASSERT ((SCM_NIMP (s_surface)
                && (long) SCM_CAR (s_surface) == surface_tag),
               s_surface, SCM_ARG1, "horiztonal-flip-surface");

   /* get source and dimensions */
   src = (SDL_Surface *) SCM_CDR (s_surface);
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

SCM
vh_flip_surface (SCM s_surface)
{
   SCM temp = vertical_flip_surface (s_surface);
   return horiztonal_flip_surface (temp);
}

SCM
scale_surface (SCM surface, SCM width, SCM height)
{
   return SCM_UNSPECIFIED;
}


void
sdl_gfx_init (void)
{
   scm_make_gsubr ("vertical-flip-surface",    1, 0, 0, vertical_flip_surface);
   scm_make_gsubr ("horizontal-flip-surface",  1, 0, 0, horiztonal_flip_surface);
   scm_make_gsubr ("vh-flip-surface",          1, 0, 0, vh_flip_surface);
   scm_make_gsubr ("scale-surface",            3, 0, 0, scale_surface);

   scm_c_export ("vertical-flip-surface",
                 "horizontal-flip-surface",
                 "vh-flip-surface",
                 "scale-surface",
                 NULL);
}

