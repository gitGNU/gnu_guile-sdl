/*******************************************************************
 *  image.h -- SDL Image functions for Guile                       *
 *                                                                 *
 *  Created:    <2001-05-01 23:39:14 foof>                         *
 *  Time-stamp: <2001-05-16 00:19:34 foof>                         *
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

#ifndef _GUILE_SDL_IMAGE_H

/* guile headers */
#include <libguile.h>
/* sdl headers */
#include <SDL/SDL.h>
#include <SDL/SDL_image.h>
/* guile-sdl headers */
#include "video.h"

/* Primitive Load */
SCM img_load_typed_rw (SCM src, SCM freesrc, SCM type);

/* Convenience functions */
SCM img_load (SCM file);
SCM img_load_rw (SCM src, SCM freesrc);

/* Functions to detect a file type, given a seekable source */
SCM img_bmp_p (SCM src);
SCM img_pnm_p (SCM src);
SCM img_xpm_p (SCM src);
SCM img_xcf_p (SCM src);
SCM img_pcx_p (SCM src);
SCM img_gif_p (SCM src);
SCM img_jpg_p (SCM src);
SCM img_tif_p (SCM src);
SCM img_png_p (SCM src);

/* Individual loading functions */
SCM img_load_bmp_rw (SCM src);
SCM img_load_pnm_rw (SCM src);
SCM img_load_xpm_rw (SCM src);
SCM img_load_xcf_rw (SCM src);
SCM img_load_pcx_rw (SCM src);
SCM img_load_gif_rw (SCM src);
SCM img_load_jpg_rw (SCM src);
SCM img_load_tif_rw (SCM src);
SCM img_load_png_rw (SCM src);
SCM img_load_tga_rw (SCM src);

/* Initialize glue */
void sdl_image_init (void);

#endif /* ! _GUILE_SDL_IMAGE_H */

