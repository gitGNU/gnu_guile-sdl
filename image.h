/*******************************************************************
 *  image.h -- SDL Image functions for Guile                       *
 *                                                                 *
 *  Created:    <2001-05-01 23:39:14 foof>                         *
 *  Time-stamp: <2001-06-18 01:07:29 foof>                         *
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
#define _GUILE_SDL_IMAGE_H

/* guile headers */
#include <libguile.h>
/* sdl headers */
#include <SDL/SDL.h>
#include <SDL/SDL_image.h>
/* guile-sdl headers */
#include "video.h"

/* Convenience functions */
SCM img_load (SCM file);

/* Initialize glue */
void sdl_image_init (void);

#endif /* ! _GUILE_SDL_IMAGE_H */

