/*******************************************************************
 *  image.c -- SDL Image functions for Guile                       *
 *                                                                 *
 *  Created:    <2001-05-01 23:39:14 foof>                         *
 *  Time-stamp: <2001-05-16 00:19:48 foof>                         *
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

/* Primitive Load */
SCM
img_load_typed_rw (SCM src, SCM freesrc, SCM type)
{
   return SCM_UNSPECIFIED;
}

/* Convenience functions */
SCM
img_load (SCM file)
{
   SDL_Surface *image;

   SCM_ASSERT ((SCM_NIMP (file) && SCM_STRINGP (file)),
               file, SCM_ARG1, "sdl-load-image");

   image = IMG_Load (SCM_CHARS (file));
   SCM_RETURN_NEWSMOB (surface_tag, image);
}

SCM
img_load_rw (SCM src, SCM freesrc)
{
   return SCM_UNSPECIFIED;
}

/* Functions to detect a file type, given a seekable source */
SCM
img_bmp_p (SCM src)
{
   return SCM_UNSPECIFIED;
}

SCM
img_pnm_p (SCM src)
{
   return SCM_UNSPECIFIED;
}

SCM
img_xpm_p (SCM src)
{
   return SCM_UNSPECIFIED;
}

SCM
img_xcf_p (SCM src)
{
   return SCM_UNSPECIFIED;
}

SCM
img_pcx_p (SCM src)
{
   return SCM_UNSPECIFIED;
}

SCM
img_gif_p (SCM src)
{
   return SCM_UNSPECIFIED;
}

SCM
img_jpg_p (SCM src)
{
   return SCM_UNSPECIFIED;
}

SCM
img_tif_p (SCM src)
{
   return SCM_UNSPECIFIED;
}

SCM
img_png_p (SCM src)
{
   return SCM_UNSPECIFIED;
}

/* Individual loading functions */
SCM
img_load_bmp_rw (SCM src)
{
   return SCM_UNSPECIFIED;
}

SCM
img_load_pnm_rw (SCM src)
{
   return SCM_UNSPECIFIED;
}

SCM
img_load_xpm_rw (SCM src)
{
   return SCM_UNSPECIFIED;
}

SCM
img_load_xcf_rw (SCM src)
{
   return SCM_UNSPECIFIED;
}

SCM
img_load_pcx_rw (SCM src)
{
   return SCM_UNSPECIFIED;
}

SCM
img_load_gif_rw (SCM src)
{
   return SCM_UNSPECIFIED;
}

SCM
img_load_jpg_rw (SCM src)
{
   return SCM_UNSPECIFIED;
}

SCM
img_load_tif_rw (SCM src)
{
   return SCM_UNSPECIFIED;
}

SCM
img_load_png_rw (SCM src)
{
   return SCM_UNSPECIFIED;
}

SCM
img_load_tga_rw (SCM src)
{
   return SCM_UNSPECIFIED;
}

/* Initialize glue */
void
sdl_image_init (void)
{
/*    scm_make_gsubr ("sdl-load-image-typed-rw",  3, 0, 0, img_load_typed_rw); */
   scm_make_gsubr ("sdl-load-image",           1, 0, 0, img_load);
/*    scm_make_gsubr ("sdl-load-rw-image",        2, 0, 0, img_load_rw); */

/*    scm_make_gsubr ("sdl-image/bmp?",         1, 0, 0, img_bmp_p); */
/*    scm_make_gsubr ("sdl-image/pnm?",         1, 0, 0, img_pnm_p); */
/*    scm_make_gsubr ("sdl-image/xpm?",         1, 0, 0, img_xpm_p); */
/*    scm_make_gsubr ("sdl-image/xcf?",         1, 0, 0, img_xcf_p); */
/*    scm_make_gsubr ("sdl-image/pcx?",         1, 0, 0, img_pcx_p); */
/*    scm_make_gsubr ("sdl-image/gif?",         1, 0, 0, img_gif_p); */
/*    scm_make_gsubr ("sdl-image/jpg?",         1, 0, 0, img_jpg_p); */
/*    scm_make_gsubr ("sdl-image/tif?",         1, 0, 0, img_tif_p); */
/*    scm_make_gsubr ("sdl-image/png?",         1, 0, 0, img_png_p); */

/*    scm_make_gsubr ("sdl-load-bmp-rw",        1, 0, 0, img_load_bmp_rw); */
/*    scm_make_gsubr ("sdl-load-pnm-rw",        1, 0, 0, img_load_pnm_rw); */
/*    scm_make_gsubr ("sdl-load-xpm-rw",        1, 0, 0, img_load_xpm_rw); */
/*    scm_make_gsubr ("sdl-load-xcf-rw",        1, 0, 0, img_load_xcf_rw); */
/*    scm_make_gsubr ("sdl-load-pcx-rw",        1, 0, 0, img_load_pcx_rw); */
/*    scm_make_gsubr ("sdl-load-gif-rw",        1, 0, 0, img_load_gif_rw); */
/*    scm_make_gsubr ("sdl-load-jpg-rw",        1, 0, 0, img_load_jpg_rw); */
/*    scm_make_gsubr ("sdl-load-tif-rw",        1, 0, 0, img_load_tif_rw); */
/*    scm_make_gsubr ("sdl-load-png-rw",        1, 0, 0, img_load_png_rw); */
}
