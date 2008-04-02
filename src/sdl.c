/* sdl.c --- SDL Wrappers for Guile
 *
 * 	Copyright (C) 2003,2004,2005,2007,2008 Thien-Thi Nguyen
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program; if not, write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA  02110-1301  USA
 */

#include <guile/gh.h>
#include <SDL/SDL.h>

#include "config.h"
#include "argcheck.h"
#include "sdlenums.h"
#include "sdlsmobs.h"
#include "retval.h"


/* Forward declarations on this page.  */

extern void gsdl_init_enums (void);
extern void gsdl_init_rect (void);
extern void gsdl_init_color (void);
extern void gsdl_init_video (void);
extern void gsdl_init_surface (void);
extern void gsdl_init_event (void);
extern void gsdl_init_joystick (void);
extern void gsdl_init_cdrom (void);
extern void gsdl_init_misc (void);


/* See ../include/sdlsmobs.h for discussion.
   The assignment is forces allocation in data space.  */

long gsdl_smob_tags[GSTX_TOO_MUCH] = {0x50feeb1e,};


static SCM init_flags;

GH_DEFPROC
(get_init_flags, "flagstash:init", 0, 0, 0, (),
 doc: /***********
Return the flagstash object for @code{init} flags.
@xref{Enums and Constants}.  */)
{
  return init_flags;
}


/* Initialization */
GH_DEFPROC
(init, "init", 1, 0, 0,
 (SCM sel),
 doc: /***********
Initialize SDL based on configuration flags @var{sel}.
@var{sel} is a list of symbols whose names all begin
with @code{SDL_INIT_}.  */)
{
#define FUNC_NAME s_init
  RETURN_INT (SDL_Init (GSDL_FLAGS2ULONG (sel, init_flags, ARGH1)));
#undef FUNC_NAME
}


GH_DEFPROC
(init_subsystem, "init-subsystem", 1, 0, 0,
 (SCM sel),
 doc: /***********
Initialize the SDL subsystems represented by @var{sel}.
@var{sel} is a list of flags (symbols)
from the same set useful for @code{init}.  */)
{
#define FUNC_NAME s_init_subsystem
  RETURN_INT (SDL_InitSubSystem (GSDL_FLAGS2ULONG (sel, init_flags, ARGH1)));
#undef FUNC_NAME
}


/* Termination */
GH_DEFPROC
(quit, "quit", 0, 0, 0,
 (void),
 doc: /***********
Shut down all SDL subsystems.
Return #t.  */)
{
#define FUNC_NAME s_quit
  scm_gc ();
  SDL_Quit ();
  RETURN_TRUE;
#undef FUNC_NAME
}


GH_DEFPROC
(quit_subsystem, "quit-subsystem", 1, 0, 0,
 (SCM sel),
 doc: /***********
Shut down the SDL subsystems represented by @var{sel}.
@var{sel} is a list of flags (symbols)
from the same set useful for @code{init}.
Return #t.  */)
{
#define FUNC_NAME s_quit_subsystem
  scm_gc ();
  SDL_QuitSubSystem (GSDL_FLAGS2ULONG (sel, init_flags, ARGH1));
  RETURN_TRUE;
#undef FUNC_NAME
}


/* Information */
GH_DEFPROC
(was_init, "was-init", 1, 0, 0,
 (SCM sel),
 doc: /***********
Check if the SDL subsystems represented by @var{sel} have
been initialized.  @var{sel} is a list of flags (symbols)
from the same set useful for @code{init}.  Return a list
likewise composed.  */)
{
#define FUNC_NAME s_was_init
  return gsdl_ulong2flags (SDL_WasInit (GSDL_FLAGS2ULONG
                                        (sel, init_flags, ARGH1)),
                           init_flags);
#undef FUNC_NAME
}


/* time functions */

GH_DEFPROC
(get_ticks, "get-ticks", 0, 0, 0,
 (void),
 doc: /***********
Return the number of milliseconds since
the SDL library initialization.  */)
{
#define FUNC_NAME s_get_ticks
  RETURN_INT (SDL_GetTicks ());
#undef FUNC_NAME
}


GH_DEFPROC
(delay, "delay", 1, 0, 0,
 (SCM ms),
 doc: /***********
Wait @var{ms} milliseconds.
The return value is unspecified.  */)
{
#define FUNC_NAME s_delay
  ASSERT_EXACT (ms, ARGH1);
  SDL_Delay (gh_scm2ulong (ms));
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


/* error handling */

GH_DEFPROC
(get_error, "get-error", 0, 0, 0,
 (void),
 doc: /***********
Return the current SDL error string.  */)
{
#define FUNC_NAME s_get_error
  char *error = SDL_GetError ();
  RETURN_0STR (error);
#undef FUNC_NAME
}


extern flagstash_t gsdl_init_flagstash;

static
void
init_module (void)
{
  /* Initialize enums first, so we can use them.  */
  gsdl_init_enums ();

  /* Init flags.  */
  init_flags = gsdl_make_flagstash (&gsdl_init_flagstash);

#include "sdl.x"

  /* Initialize subsystems.  */
  gsdl_init_rect ();
  gsdl_init_color ();
  gsdl_init_video ();
  gsdl_init_surface ();
  gsdl_init_event ();
  gsdl_init_joystick ();
  gsdl_init_cdrom ();
  gsdl_init_misc ();
}

GH_MODULE_LINK_FUNC ("sdl sdl", sdl_sdl, init_module)

/* sdl.c ends here */
