/*******************************************************************
 *  guile-sdl.c -- SDL Video Wrappers for Guile                    *
 *                                                                 *
 *  Created:    <2001-04-08 13:48:18 foof>                         *
 *  Time-stamp: <2001-06-11 19:17:20 foof>                         *
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
#include "scm.h"
#include "guile-sdl.h"
#include "video.h"
#include "image.h"
#include "gfx.h"
#include "event.h"
#include "mixer.h"
#include "ttf.h"
#include "wm.h"

/* Initialization */
SCM
sdl_init (SCM s_subsystems)
{
   int subsystems;

   SCM_ASSERT (SCM_INUMP (s_subsystems), s_subsystems, SCM_ARG1, "init");
   subsystems = SCM_INUM (s_subsystems);

   return SCM_MAKINUM (SDL_Init (subsystems));
}

SCM
sdl_init_subsystem (SCM s_subsystems)
{
   int subsystems;

   SCM_ASSERT (SCM_INUMP (s_subsystems), s_subsystems, SCM_ARG1, "init-subsystem");
   subsystems = SCM_INUM (s_subsystems);

   return SCM_MAKINUM (SDL_InitSubSystem (subsystems));
}

/* Termination */
SCM
sdl_quit (void)
{
   SDL_Quit();
   return SCM_UNSPECIFIED;
}

SCM
sdl_quit_subsystem (SCM s_subsystems)
{
   int subsystems;

   SCM_ASSERT (SCM_INUMP (s_subsystems), s_subsystems, SCM_ARG1, "quit-subsystem");
   subsystems = SCM_INUM (s_subsystems);

   SDL_QuitSubSystem (subsystems);
   return SCM_UNSPECIFIED;
}

/* Information */
SCM
sdl_was_init (SCM s_subsystems)
{
   int subsystems;

   SCM_ASSERT (SCM_INUMP (s_subsystems), s_subsystems, SCM_ARG1, "was-init");
   subsystems = SCM_INUM (s_subsystems);

   return SCM_MAKINUM (SDL_WasInit (subsystems));
}

/* time functions */

SCM
sdl_get_ticks (void)
{
   return SCM_MAKINUM (SDL_GetTicks ());
}

SCM
sdl_delay (SCM ms)
{
   SCM_ASSERT (SCM_INUMP (ms),  ms,  SCM_ARG1, "delay");
   SDL_Delay (SCM_INUM (ms));
   return SCM_UNSPECIFIED;
}

void
guile_sdl_init (void)
{
   /* scm util definition */
   scm_c_define_gsubr ("enum->number",   2, 0, 0, scm_enum_to_number);
   scm_c_define_gsubr ("number->enum",   2, 0, 0, scm_number_to_enum);

   /* general initializations */
   scm_c_define_gsubr ("init",           1, 0, 0, sdl_init);
   scm_c_define_gsubr ("init-subsystem", 1, 0, 0, sdl_init_subsystem);
   scm_c_define_gsubr ("quit-all",       0, 0, 0, sdl_quit);
   scm_c_define_gsubr ("quit-subsystem", 1, 0, 0, sdl_quit_subsystem);
   scm_c_define_gsubr ("was-init",       1, 0, 0, sdl_was_init);
   scm_c_define_gsubr ("get-ticks",      0, 0, 0, sdl_get_ticks);
   scm_c_define_gsubr ("delay",          1, 0, 0, sdl_delay);

   /* constants */
   scm_c_define ("init/timer",       SCM_MAKINUM (SDL_INIT_TIMER));
   scm_c_define ("init/audio",       SCM_MAKINUM (SDL_INIT_AUDIO));
   scm_c_define ("init/video",       SCM_MAKINUM (SDL_INIT_VIDEO));
   scm_c_define ("init/cdrom",       SCM_MAKINUM (SDL_INIT_CDROM));
   scm_c_define ("init/joystick",    SCM_MAKINUM (SDL_INIT_JOYSTICK)); 
   scm_c_define ("init/everything",  SCM_MAKINUM (SDL_INIT_EVERYTHING)); 
   scm_c_define ("init/noparachute", SCM_MAKINUM (SDL_INIT_NOPARACHUTE)); 
   scm_c_define ("init/eventthread", SCM_MAKINUM (SDL_INIT_EVENTTHREAD)); 

   /* exported symbols */
   scm_c_export (
      /* utils */
      "enum->number", "number->enum",
      /* time */
      "get-ticks", "delay",
      /* sdl initializations */
      "subsystems", "init", "quit-all", "init-subsystem",
      "quit-subsystem", "was-init",
      /* constants */
      "init/timer", "init/audio", "init/video",
      "init/cdrom", "init/joystick", "init/everything",
      "init/noparachute", "init/eventthread",
      NULL);

   /* wm initializations */
   sdl_wm_init();

   /* video initializations */
   sdl_video_init();

   /* image initializations */
   sdl_image_init();

   /* extra gfx initializations */
   sdl_gfx_init();

   /* event initializations */
   sdl_event_init();

   /* mixer initializations */
   sdl_mixer_init();

   /* ttf initializations */
   sdl_ttf_init();
}

