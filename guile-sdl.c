/*******************************************************************
 *  guile-sdl.c -- SDL Video Wrappers for Guile                    *
 *                                                                 *
 *  Created:    <2001-04-08 13:48:18 foof>                         *
 *  Time-stamp: <2001-06-01 21:38:41 foof>                         *
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

/* Initialization */
static SCM
sdl_init (SCM s_subsystems)
{
   int subsystems;

   SCM_ASSERT (SCM_INUMP (s_subsystems), s_subsystems, SCM_ARG1, "init");
   subsystems = SCM_INUM (s_subsystems);

   return SCM_MAKINUM (SDL_Init (subsystems));
}

static SCM
sdl_init_subsystem (SCM s_subsystems)
{
   int subsystems;

   SCM_ASSERT (SCM_INUMP (s_subsystems), s_subsystems, SCM_ARG1, "init-subsystem");
   subsystems = SCM_INUM (s_subsystems);

   return SCM_MAKINUM (SDL_InitSubSystem (subsystems));
}

/* Termination */
static SCM
sdl_quit (void)
{
   SDL_Quit();
   return SCM_UNSPECIFIED;
}

static SCM
sdl_quit_subsystem (SCM s_subsystems)
{
   int subsystems;

   SCM_ASSERT (SCM_INUMP (s_subsystems), s_subsystems, SCM_ARG1, "quit-subsystem");
   subsystems = SCM_INUM (s_subsystems);

   SDL_QuitSubSystem (subsystems);
   return SCM_UNSPECIFIED;
}

/* Information */
static SCM
sdl_was_init (SCM s_subsystems)
{
   int subsystems;

   SCM_ASSERT (SCM_INUMP (s_subsystems), s_subsystems, SCM_ARG1, "was-init");
   subsystems = SCM_INUM (s_subsystems);

   return SCM_MAKINUM (SDL_WasInit (subsystems));
}

void
guile_sdl_init (void)
{
   /* general initializations */
   scm_make_gsubr ("init",           1, 0, 0, sdl_init);
   scm_make_gsubr ("init-subsystem", 1, 0, 0, sdl_init_subsystem);
   scm_make_gsubr ("quit-sdl",       0, 0, 0, sdl_quit);
   scm_make_gsubr ("quit-subsystem", 1, 0, 0, sdl_quit_subsystem);
   scm_make_gsubr ("was-init",       1, 0, 0, sdl_was_init);

   /* constants */
   scm_c_define ("init-timer",       SCM_MAKINUM (SDL_INIT_TIMER));
   scm_c_define ("init-audio",       SCM_MAKINUM (SDL_INIT_AUDIO));
   scm_c_define ("init-video",       SCM_MAKINUM (SDL_INIT_VIDEO));
   scm_c_define ("init-cdrom",       SCM_MAKINUM (SDL_INIT_CDROM));
   scm_c_define ("init-joystick",    SCM_MAKINUM (SDL_INIT_JOYSTICK));
   scm_c_define ("init-everything",  SCM_MAKINUM (SDL_INIT_EVERYTHING));
   scm_c_define ("init-noparachute", SCM_MAKINUM (SDL_INIT_NOPARACHUTE));
   scm_c_define ("init-eventthread", SCM_MAKINUM (SDL_INIT_EVENTTHREAD));

   /* exported symbols */
   scm_c_export ("init", "quit-sdl", "init-subsystem",
                 "quit-subsystem", "was-init",
                 /* constants */
                 "init-timer", "init-audio", "init-video",
                 "init-cdrom", "init-joystick", "init-everything",
                 "init-noparachute", "init-eventthread",
                 NULL);

   /* video initializations */
   sdl_video_init();

   /* input initializations */
}

