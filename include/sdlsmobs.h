/* sdlsmobs.h --- Smob helper definitions
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

#ifndef GUILE_SDL_SMOBS_H
#define GUILE_SDL_SMOBS_H

/* useful type-checking for smobs */
#define ASSERT_SMOB(arg, tag, error)                    \
  SCM_ASSERT ((SCM_NIMP (arg)                           \
                 && (long) SCM_CAR (arg) == tag),       \
                arg, error, FUNC_NAME)

#define SMOBGET(smob,c_type)       ((c_type) SCM_SMOB_DATA (smob))
#define SMOBSET(smob,val)          (SCM_SET_SMOB_DATA (smob, val))

#define SMOBFIELD(c_type,c_field)  (SMOBGET (smob, c_type)->c_field)


/* Most smob tags are static, but a few are required to be global
   because they are used in more than one compilation unit.  To minimize
   linker symbol footprint we gather them into one big array, manually
   indexed.  The convenience foo_tag macros are for consistency w/ the
   static variants.  "GSTX" stands for "Guile-sdl (and/or Global) Smob
   Tag indeX".  */

#define GSTX_COLOR              0
#define GSTX_RECT               1
#define GSTX_SURFACE            2
#define GSTX_PIXEL_FORMAT       3
#define GSTX_RESERVED4          4
#define GSTX_RESERVED5          5
#define GSTX_RESERVED6          6
#define GSTX_RESERVED7          7
#define GSTX_TOO_MUCH           8

extern long gsdl_smob_tags[GSTX_TOO_MUCH];

#define color_tag           (gsdl_smob_tags[GSTX_COLOR])
#define rect_tag            (gsdl_smob_tags[GSTX_RECT])
#define surface_tag         (gsdl_smob_tags[GSTX_SURFACE])
#define pixel_format_tag    (gsdl_smob_tags[GSTX_PIXEL_FORMAT])

#define ASSERT_COLOR(obj,n)         ASSERT_SMOB (obj, color_tag, n)
#define ASSERT_RECT(obj,n)          ASSERT_SMOB (obj, rect_tag, n)
#define ASSERT_SURFACE(obj,n)       ASSERT_SMOB (obj, surface_tag, n)
#define ASSERT_PIXEL_FORMAT(obj,n)  ASSERT_SMOB (obj, pixel_format_tag, n)

#define UNPACK_COLOR(smob)         (SMOBGET (smob, SDL_Color *))
#define UNPACK_RECT(smob)          (SMOBGET (smob, SDL_Rect *))
#define UNPACK_SURFACE(smob)       (SMOBGET (smob, SDL_Surface *))
#define UNPACK_PIXEL_FORMAT(smob)  (SMOBGET (smob, SDL_PixelFormat *))

#define RETURN_NEW_COLOR(x)         SCM_RETURN_NEWSMOB (color_tag, x)
#define RETURN_NEW_RECT(x)          SCM_RETURN_NEWSMOB (rect_tag, x)
#define RETURN_NEW_SURFACE(x)       SCM_RETURN_NEWSMOB (surface_tag, x)
#define RETURN_NEW_PIXEL_FORMAT(x)  SCM_RETURN_NEWSMOB (pixel_format_tag, x)

#endif /* ! defined (GUILE_SDL_SMOBS_H) */

/* sdlsmobs.h ends here */
