/*******************************************************************
 *  sdl.c -- SDL Wrappers for Guile                                *
 *                                                                 *
 *  Created:    <2001-04-08 13:48:18 foof>                         *
 *  Time-stamp: <2001-06-25 01:23:41 foof>                         *
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
#include "sdl.h"
#include "sdlenums.h"
#include "sdlsmobs.h"
#include "sdlvideo.h"
/* #include "sdlimage.h" */
#include "sdlgfx.h"
#include "sdlevent.h"
#include "sdlcdrom.h"
#include "sdljoystick.h"
/* #include "mixer.h" */
/* #include "ttf.h" */


/* Initialization */
SCM
sdl_init (SCM s_subsystems)
{
   int subsystems;

   SCM_ASSERT (scm_exact_p (s_subsystems), s_subsystems, SCM_ARG1, "sdl-init");
   subsystems = scm_num2long (s_subsystems, SCM_ARG1, "scm_num2long");

   return scm_long2num (SDL_Init (subsystems));
}

SCM
sdl_init_subsystem (SCM s_subsystems)
{
   int subsystems;

   SCM_ASSERT (scm_exact_p (s_subsystems), s_subsystems, SCM_ARG1, "sdl-init-subsystem");
   subsystems = scm_num2long (s_subsystems, SCM_ARG1, "scm_num2long");

   return scm_long2num (SDL_InitSubSystem (subsystems));
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

   SCM_ASSERT (scm_exact_p (s_subsystems), s_subsystems, SCM_ARG1, "sdl-quit-subsystem");
   subsystems = scm_num2long (s_subsystems, SCM_ARG1, "scm_num2long");

   SDL_QuitSubSystem (subsystems);
   return SCM_UNSPECIFIED;
}

/* Information */
SCM
sdl_was_init (SCM s_subsystems)
{
   int subsystems;

   SCM_ASSERT (scm_exact_p (s_subsystems), s_subsystems, SCM_ARG1, "sdl-was-init");
   subsystems = scm_num2long (s_subsystems, SCM_ARG1, "scm_num2long");

   return scm_long2num (SDL_WasInit (subsystems));
}

/* time functions */

SCM
sdl_get_ticks (void)
{
   return scm_long2num (SDL_GetTicks ());
}

SCM
sdl_delay (SCM ms)
{
   SCM_ASSERT (scm_exact_p (ms),  ms,  SCM_ARG1, "sdl-delay");
   SDL_Delay (scm_num2long (ms, SCM_ARG1, "scm_num2long"));
   return SCM_UNSPECIFIED;
}

/* error functions */

/* SCM */
/* sdl_get_error (void) */
/* { */
/*    char *error = SDL_GetError(); */
/*    return scm_makfrom0str (error); */
/* } */

void
guile_sdl_init (void)
{
   /* scm util definition */
   scm_c_define_gsubr ("enum->number",   2, 0, 0, scm_enum_to_number);
   scm_c_define_gsubr ("number->enum",   2, 0, 0, scm_number_to_enum);

   /* general initializations */
   scm_c_define_gsubr ("sdl-init",           1, 0, 0, sdl_init);
   scm_c_define_gsubr ("sdl-init-subsystem", 1, 0, 0, sdl_init_subsystem);
   scm_c_define_gsubr ("sdl-quit",           0, 0, 0, sdl_quit);
   scm_c_define_gsubr ("sdl-quit-subsystem", 1, 0, 0, sdl_quit_subsystem);
   scm_c_define_gsubr ("sdl-was-init",       1, 0, 0, sdl_was_init);
   scm_c_define_gsubr ("sdl-get-ticks",      0, 0, 0, sdl_get_ticks);
   scm_c_define_gsubr ("sdl-delay",          1, 0, 0, sdl_delay);
/*    scm_c_define_gsubr ("sdl-get-error",      0, 0, 0, sdl_get_error); */

   /* constants */
   scm_c_define ("sdl-init/timer",       scm_long2num (SDL_INIT_TIMER));
   scm_c_define ("sdl-init/audio",       scm_long2num (SDL_INIT_AUDIO));
   scm_c_define ("sdl-init/video",       scm_long2num (SDL_INIT_VIDEO));
   scm_c_define ("sdl-init/cdrom",       scm_long2num (SDL_INIT_CDROM));
   scm_c_define ("sdl-init/joystick",    scm_long2num (SDL_INIT_JOYSTICK));
   scm_c_define ("sdl-init/everything",  scm_long2num (SDL_INIT_EVERYTHING));
   scm_c_define ("sdl-init/noparachute", scm_long2num (SDL_INIT_NOPARACHUTE));
   scm_c_define ("sdl-init/eventthread", scm_long2num (SDL_INIT_EVENTTHREAD));

   /* exported symbols */
   scm_c_export (
      /* utils */
      "enum->number", "number->enum",
      /* time */
      "sdl-get-ticks", "sdl-delay",
      /* errors */
      /* "sdl-get-error", */
      /* sdl initializations */
      "sdl-subsystems", "sdl-init", "sdl-quit", "sdl-init-subsystem",
      "sdl-quit-subsystem", "sdl-was-init",
      /* constants */
      "sdl-init/timer", "sdl-init/audio", "sdl-init/video",
      "sdl-init/cdrom", "sdl-init/joystick", "sdl-init/everything",
      "sdl-init/noparachute", "sdl-init/eventthread",
      NULL);

   /* initialize subsystems */
   sdl_video_init();
   sdl_gfx_init();
   sdl_event_init();
   sdl_init_joystick();
   sdl_init_cdrom();
}

