/*******************************************************************
 *  sdl.c -- SDL Wrappers for Guile                                *
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
#include "sdlrect.h"
#include "sdlcolor.h"
#include "sdlsurface.h"
#include "sdlvideo.h"
#include "sdlevent.h"
#include "sdlcdrom.h"
#include "sdljoystick.h"
#include "sdlroto.h"

SCM sdl_init_flags;

/* Initialization */
SCM_DEFINE( sdl_init, "sdl-init", 1, 0, 0,
            (SCM s_subsystems),
"Initializes SDL.")
#define FUNC_NAME s_sdl_init
{
  unsigned long subsystems;

  subsystems = scm_flags2ulong (s_subsystems, sdl_init_flags,
                                SCM_ARG1, "sdl-init");

  return scm_long2num (SDL_Init (subsystems));
}
#undef FUNC_NAME


SCM_DEFINE( sdl_init_subsystem, "sdl-init-subsystem", 1, 0, 0,
            (SCM s_subsystems),
"Initializes the given SDL subsystems.")
#define FUNC_NAME s_sdl_init_subsystem
{
  unsigned long subsystems;

  subsystems = scm_flags2ulong (s_subsystems, sdl_init_flags,
                                SCM_ARG1, "sdl-init-subsystems");

  return scm_long2num (SDL_InitSubSystem (subsystems));
}
#undef FUNC_NAME


/* Termination */
SCM_DEFINE( sdl_quit, "sdl-quit", 0, 0, 0,
            (void),
"Shuts down all SDL subsystems.")
#define FUNC_NAME s_sdl_quit
{
  SDL_Quit();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE( sdl_quit_subsystem, "sdl-quit-subsystem", 1, 0, 0,
            (SCM s_subsystems),
"Shuts down the given SDL subsystems.")
#define FUNC_NAME s_sdl_quit_subsystem
{
  unsigned long subsystems;

  subsystems = scm_flags2ulong (s_subsystems, sdl_init_flags,
                                SCM_ARG1, "scm_num2long");

  SDL_QuitSubSystem (subsystems);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


/* Information */
SCM_DEFINE( sdl_was_init, "sdl-was-init", 1, 0, 0,
            (SCM s_subsystems),
"Check which SDL subsystems have been initialized.")
#define FUNC_NAME s_sdl_was_init
{
  unsigned long subsystems;

  subsystems = scm_flags2ulong (s_subsystems, sdl_init_flags,
                                SCM_ARG1, "scm_num2long");

  return scm_ulong2flags (SDL_WasInit (subsystems), sdl_init_flags);
}
#undef FUNC_NAME


/* time functions */

SCM_DEFINE( sdl_get_ticks, "sdl-get-ticks", 0, 0, 0,
            (void),
"Get the number of milliseconds since the SDL library initialization.")
#define FUNC_NAME s_sdl_get_ticks
{
  return scm_long2num (SDL_GetTicks ());
}
#undef FUNC_NAME


SCM_DEFINE( sdl_delay, "sdl-delay", 1, 0, 0,
            (SCM ms),
"Wait a specified number of milliseconds before returning.")
#define FUNC_NAME s_sdl_delay
{
  SCM_ASSERT (scm_exact_p (ms),  ms,  SCM_ARG1, "sdl-delay");
  SDL_Delay (scm_num2long (ms, SCM_ARG1, "scm_num2long"));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


/* error handling */

SCM_DEFINE( sdl_get_error, "sdl-get-error", 0, 0, 0,
            (void),
"Return the current SDL error string.")
#define FUNC_NAME s_sdl_get_error
{
  char *error = SDL_GetError();
  return scm_makfrom0str (error);
}
#undef FUNC_NAME


void
guile_sdl_init (void)
{
  /* initialize enums first, so we can use them */
  sdl_init_enums();

  /* general initializations */
/*   scm_c_define_gsubr ("sdl-init",           1, 0, 0, sdl_init); */
/*   scm_c_define_gsubr ("sdl-init-subsystem", 1, 0, 0, sdl_init_subsystem); */
/*   scm_c_define_gsubr ("sdl-quit",           0, 0, 0, sdl_quit); */
/*   scm_c_define_gsubr ("sdl-quit-subsystem", 1, 0, 0, sdl_quit_subsystem); */
/*   scm_c_define_gsubr ("sdl-was-init",       1, 0, 0, sdl_was_init); */
/*   scm_c_define_gsubr ("sdl-get-ticks",      0, 0, 0, sdl_get_ticks); */
/*   scm_c_define_gsubr ("sdl-delay",          1, 0, 0, sdl_delay); */
/*   scm_c_define_gsubr ("sdl-get-error",      0, 0, 0, sdl_get_error); */

  /* init flags */
  sdl_init_flags = scm_c_define_flag (
    "sdl-init-flags",
    "SDL_INIT_TIMER",        SDL_INIT_TIMER,
    "SDL_INIT_AUDIO",        SDL_INIT_AUDIO,
    "SDL_INIT_VIDEO",        SDL_INIT_VIDEO,
    "SDL_INIT_CDROM",        SDL_INIT_CDROM,
    "SDL_INIT_JOYSTICK",     SDL_INIT_JOYSTICK,
    "SDL_INIT_EVERYTHING",   SDL_INIT_EVERYTHING,
    "SDL_INIT_NOPARACHUTE",  SDL_INIT_NOPARACHUTE,
    "SDL_INIT_EVENTTHREAD",  SDL_INIT_EVENTTHREAD,
    NULL);

  /* exported symbols */
  scm_c_export (
    /* time */
    "sdl-get-ticks", "sdl-delay",
    /* errors */
    "sdl-get-error",
    /* sdl initializations */
    "sdl-subsystems", "sdl-init", "sdl-quit", "sdl-init-subsystem",
    "sdl-quit-subsystem", "sdl-was-init",
    "sdl-init-flags",
    NULL);

#ifndef SCM_MAGIC_SNARFER
#include "sdl.x"
#endif

  /* initialize subsystems */
  sdl_init_rect();
  sdl_init_color();
  sdl_init_video();
  sdl_init_surface();
  sdl_init_rotozoom();
  sdl_init_event();
  sdl_init_joystick();
  sdl_init_cdrom();
}

