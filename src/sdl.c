/* sdl.c --- SDL Wrappers for Guile
 *
 * 	Copyright (C) 2003 Thien-Thi Nguyen
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

#include <guile/gh.h>
#include <SDL/SDL.h>

#include "config.h"
#include "argcheck.h"
#include "sdlenums.h"
#include "sdlsmobs.h"


/* Forward declarations on this page.  */

extern void gsdl_init_enums (void);
extern void gsdl_init_rect (void);
extern void gsdl_init_color (void);
extern void gsdl_init_video (void);
extern void gsdl_init_surface (void);
extern void gsdl_init_rotozoom (void);
extern void gsdl_init_event (void);
extern void gsdl_init_joystick (void);
extern void gsdl_init_cdrom (void);


/* See ../include/sdlsmobs.h for discussion.  */

long gsdl_smob_tags[GSTX_TOO_MUCH];


static SCM sdl_init_flags;

MDEFLOCEXP (sdl_get_init_flags, "flagstash:init", 0, 0, 0, (),
            "Return the flagstash object for @code{sdl-init} flags.")
{
  return sdl_init_flags;
}


/* Initialization */
MDEFLOCEXP (sdl_init, "sdl-init", 1, 0, 0,
            (SCM sel),
            "Initialize SDL based on configuration flags @var{sel}.\n"
            "@var{sel} is a list of symbols whose names all begin\n"
            "with @code{SCM_INIT_}.")
#define FUNC_NAME s_sdl_init
{
  return gh_long2scm (SDL_Init
                      (GSDL_FLAGS2ULONG
                       (sel, sdl_init_flags, SCM_ARG1)));
}
#undef FUNC_NAME


MDEFLOCEXP (sdl_init_subsystem, "sdl-init-subsystem", 1, 0, 0,
            (SCM sel),
            "Initialize the SDL subsystems represented by @var{sel}.\n"
            "@var{sel} is a list of flags (symbols)\n"
            "from the same set useful for @code{sdl-init}.")
#define FUNC_NAME s_sdl_init_subsystem
{
  return gh_long2scm (SDL_InitSubSystem
                      (GSDL_FLAGS2ULONG
                       (sel, sdl_init_flags, SCM_ARG1)));
}
#undef FUNC_NAME


/* Termination */
MDEFLOCEXP (sdl_quit, "sdl-quit", 0, 0, 0,
            (void),
            "Shut down all SDL subsystems.")
#define FUNC_NAME s_sdl_quit
{
  SDL_Quit();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


MDEFLOCEXP (sdl_quit_subsystem, "sdl-quit-subsystem", 1, 0, 0,
            (SCM sel),
            "Shut down the SDL subsystems represented by @var{sel}.\n"
            "@var{sel} is a list of flags (symbols)\n"
            "from the same set useful for @code{sdl-init}.")
#define FUNC_NAME s_sdl_quit_subsystem
{
  SDL_QuitSubSystem (GSDL_FLAGS2ULONG (sel, sdl_init_flags, SCM_ARG1));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


/* Information */
MDEFLOCEXP (sdl_was_init, "sdl-was-init", 1, 0, 0,
            (SCM sel),
            "Check if the SDL subsystems represented by @var{sel}\n"
            "have been initialized.  @var{sel} is a list of flags (symbols)\n"
            "from the same set useful for @code{sdl-init}.")
#define FUNC_NAME s_sdl_was_init
{
  return gsdl_ulong2flags (SDL_WasInit (GSDL_FLAGS2ULONG
                                        (sel, sdl_init_flags, SCM_ARG1)),
                           sdl_init_flags);
}
#undef FUNC_NAME


/* time functions */

MDEFLOCEXP (sdl_get_ticks, "sdl-get-ticks", 0, 0, 0,
            (void),
            "Return the number of milliseconds since\n"
            "the SDL library initialization.")
#define FUNC_NAME s_sdl_get_ticks
{
  return gh_long2scm (SDL_GetTicks ());
}
#undef FUNC_NAME


MDEFLOCEXP (sdl_delay, "sdl-delay", 1, 0, 0,
            (SCM ms),
            "Wait @var{ms} milliseconds.")
#define FUNC_NAME s_sdl_delay
{
  ASSERT_EXACT (ms, SCM_ARG1);
  SDL_Delay (gh_scm2ulong (ms));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


/* error handling */

MDEFLOCEXP (sdl_get_error, "sdl-get-error", 0, 0, 0,
            (void),
            "Return the current SDL error string.")
#define FUNC_NAME s_sdl_get_error
{
  char *error = SDL_GetError();
  return gh_str02scm (error);
}
#undef FUNC_NAME


extern flagstash_t gsdl_init_flagstash;

static
void
init_module (void)
{
  /* Initialize enums first, so we can use them.  */
  gsdl_init_enums ();

  /* Init flags.  */
  sdl_init_flags = gsdl_make_flagstash (&gsdl_init_flagstash);

#include "sdl.x"

  /* Initialize subsystems.  */
  gsdl_init_rect ();
  gsdl_init_color ();
  gsdl_init_video ();
  gsdl_init_surface ();
  gsdl_init_rotozoom ();
  gsdl_init_event ();
  gsdl_init_joystick ();
  gsdl_init_cdrom ();
}

MDEFLINKFUNC ("sdl sdl-sup", sdl_sdl_sup, init_module)

/* sdl.c ends here */
