/*******************************************************************
 *  guile-sdl.c -- SDL Video Wrappers for Guile                    *
 *                                                                 *
 *  Created:    <2001-04-08 13:48:18 foof>                         *
 *  Time-stamp: <2001-05-16 00:24:39 foof>                         *
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

/* guile headers */
#include <libguile.h>
/* sdl headers */
#include <SDL/SDL.h>
/* wrapper headers */
#include "video.h"
#include "image.h"

/* Initialization */
static SCM
sdl_init (SCM s_subsystems)
{
   int subsystems;

   SCM_ASSERT(SCM_INUMP(s_subsystems), s_subsystems, SCM_ARG1, "sdl-init");
   subsystems = SCM_INUM(s_subsystems);

   return SCM_MAKINUM(SDL_Init(subsystems));
}

/* Termination */
static SCM
sdl_quit (void)
{
   SDL_Quit();
   return SCM_UNSPECIFIED;
}

void
guile_sdl_init (void)
{
   /* general initializations */
   scm_make_gsubr ("sdl-init", 1, 0, 0, sdl_init);
   scm_make_gsubr ("sdl-quit", 0, 0, 0, sdl_quit);

   /* video initializations */
   sdl_video_init();

   /* image initializations */
/*    sdl_image_init(); */
}

/* static void */
/* inner_main (void *closure, int argc, char **argv) */
/* { */
/*    /\* module initializations would go here *\/ */
/*    guile_sdl_init(); */
/*    scm_shell (argc, argv); */
/* } */

/* int */
/* main (int argc, char **argv) */
/* { */
/*    scm_boot_guile (argc, argv, inner_main, 0); */
/*    return 0; /\* never reached *\/ */
/* } */

