/*******************************************************************
 *  image.c -- SDL Image functions for Guile                       *
 *                                                                 *
 *  Created:    <2001-05-01 23:39:14 foof>                         *
 *  Time-stamp: <2001-06-18 01:07:18 foof>                         *
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

#include <image.h>

/* Load an image in one of many formats */
SCM
img_load (SCM file)
{
   SDL_Surface *image;

   SCM_ASSERT ((SCM_NIMP (file) && SCM_STRINGP (file)),
               file, SCM_ARG1, "sdl-load-image");

   image = IMG_Load (SCM_CHARS (file));
   SCM_RETURN_NEWSMOB (surface_tag, image);
}


/* Initialize glue */
void
sdl_image_init (void)
{
   scm_c_define_gsubr ("sdl-load-image",           1, 0, 0, img_load);
   scm_c_export ("sdl-load-image", NULL);
}
