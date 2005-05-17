/* sdl.c --- SDL Joystick functions for Guile
 *
 * 	Copyright (C) 2003,2004,2005 Thien-Thi Nguyen
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
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA  02110-1301  USA
 */

#include <guile/gh.h>
#include <SDL/SDL.h>

#include "config.h"
#include "argcheck.h"
#include "sdlsmobs.h"
#include "retval.h"
#include "sym.h"


static long joystick_tag;

#define ASSERT_JOYSTICK(obj,which) \
  ASSERT_SMOB (obj, joystick_tag, which)

#define UNPACK_JOYSTICK(smob) \
  (SMOBGET (smob, SDL_Joystick *))

#define RETURN_NEW_JOYSTICK(x) \
  SCM_RETURN_NEWSMOB (joystick_tag, x)

#define JOYSTICK_P(x) \
  (SCM_SMOB_PREDICATE (joystick_tag, x))



GH_DEFPROC (joystick_p, "joystick?", 1, 0, 0,
            (SCM obj),
            "Return #t iff @var{obj} is a joystick object.")
{
#define FUNC_NAME s_joystick_p
  RETURN_BOOL
    (JOYSTICK_P (obj));
#undef FUNC_NAME
}


GH_DEFPROC (joystick_null_p, "joystick-null?", 1, 0, 0,
            (SCM joystick),
            "Return #t iff @var{joystick} is a NULL joystick.\n"
            "[What does that mean? --ttn]")
{
#define FUNC_NAME s_joystick_null_p
  ASSERT_JOYSTICK (joystick, ARGH1);

  RETURN_BOOL
    (! UNPACK_JOYSTICK (joystick));
#undef FUNC_NAME
}


GH_DEFPROC (num_joysticks, "num-joysticks", 0, 0, 0,
            (void),
            "Return the number of joysticks.")
{
#define FUNC_NAME s_num_joysticks
  RETURN_INT (SDL_NumJoysticks ());
#undef FUNC_NAME
}


GH_DEFPROC (joystick_name, "joystick-name", 0, 1, 0,
            (SCM n),
            "Return the name of the default joystick.\n"
            "Optional arg @var{n} specifies which joystick to check.")
{
#define FUNC_NAME s_joystick_name
  int cn = 0;

  if (BOUNDP (n))
    {
      ASSERT_EXACT (n, ARGH1);
      cn = gh_scm2long (n);
    }

  RETURN_0STR (SDL_JoystickName (cn));
#undef FUNC_NAME
}


GH_DEFPROC (joystick_open, "joystick-open", 0, 1, 0,
            (SCM n),
            "Return a handle to the default joystick opened for use.\n"
            "Optional arg @var{n} specifies which joystick to open.")
{
#define FUNC_NAME s_joystick_open
  int cn = 0;

  if (BOUNDP (n))
    {
      ASSERT_EXACT (n, ARGH1);
      cn = gh_scm2long (n);
    }

  RETURN_NEW_JOYSTICK (SDL_JoystickOpen (cn));
#undef FUNC_NAME
}


GH_DEFPROC (joystick_opened_p, "joystick-opened?", 0, 1, 0,
            (SCM n),
            "Return #t iff the default joystick is opened.\n"
            "Optional arg @var{n} specifies which joystick to check.")
{
#define FUNC_NAME s_joystick_opened_p
  int cn = 0;

  if (BOUNDP (n))
    {
      ASSERT_EXACT (n, ARGH1);
      cn = gh_scm2long (n);
    }

  RETURN_BOOL
    (SDL_JoystickOpened (cn));
#undef FUNC_NAME
}


GH_DEFPROC (joystick_index, "joystick-index", 1, 0, 0,
            (SCM joystick),
            "Return the index of @var{joystick}.")
{
#define FUNC_NAME s_joystick_index
  SDL_Joystick *joy;

  ASSERT_JOYSTICK (joystick, ARGH1);

  joy = UNPACK_JOYSTICK (joystick);

  RETURN_INT (joy
              ? SDL_JoystickIndex (joy)
              : -1);
#undef FUNC_NAME
}


GH_DEFPROC (joystick_num_axes, "joystick-num-axes", 1, 0, 0,
            (SCM joystick),
            "Return the number of axes for @var{joystick}.")
{
#define FUNC_NAME s_joystick_num_axes
  SDL_Joystick *joy;

  ASSERT_JOYSTICK (joystick, ARGH1);

  joy = UNPACK_JOYSTICK (joystick);

  RETURN_INT (joy
              ? SDL_JoystickNumAxes (joy)
              : -1);
#undef FUNC_NAME
}


GH_DEFPROC (joystick_num_balls, "joystick-num-balls", 1, 0, 0,
            (SCM joystick),
            "Return the number trackballs for @var{joystick}.")
{
#define FUNC_NAME s_joystick_num_balls
  SDL_Joystick *joy;

  ASSERT_JOYSTICK (joystick, ARGH1);

  joy = UNPACK_JOYSTICK (joystick);

  RETURN_INT (joy
              ? SDL_JoystickNumBalls (joy)
              : -1);
#undef FUNC_NAME
}


GH_DEFPROC (joystick_num_hats, "joystick-num-hats", 1, 0, 0,
            (SCM joystick),
            "Return the number of hats for @var{joystick}.")
{
#define FUNC_NAME s_joystick_num_hats
  SDL_Joystick *joy;

  ASSERT_JOYSTICK (joystick, ARGH1);

  joy = UNPACK_JOYSTICK (joystick);

  RETURN_INT (joy
              ? SDL_JoystickNumHats (joy)
              : -1);
#undef FUNC_NAME
}


GH_DEFPROC (joystick_num_buttons, "joystick-num-buttons", 1, 0, 0,
            (SCM joystick),
            "Return number of buttons for @var{joystick}.")
{
#define FUNC_NAME s_joystick_num_buttons
  SDL_Joystick *joy;

  ASSERT_JOYSTICK (joystick, ARGH1);

  joy = UNPACK_JOYSTICK (joystick);

  RETURN_INT (joy
              ? SDL_JoystickNumButtons (joy)
              : -1);
#undef FUNC_NAME
}


GH_DEFPROC (joystick_update, "joystick-update", 0, 0, 0,
            (void),
            "Update the state of all Joysticks.")
{
#define FUNC_NAME s_joystick_update
  SDL_JoystickUpdate ();
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


GH_DEFPROC (joystick_event_state, "joystick-event-state", 1, 0, 0,
            (SCM state),
            "Set the Joystick event processing model to @var{state}.")
{
#define FUNC_NAME s_joystick_event_state
  ASSERT_EXACT (state, ARGH1);

  RETURN_INT (SDL_JoystickEventState (gh_scm2long (state)));
#undef FUNC_NAME
}


GH_DEFPROC (joystick_get_axis, "joystick-get-axis", 2, 0, 0,
            (SCM joystick,
             SCM axis),
            "For @var{joystick}, return state of @var{axis}.")
{
#define FUNC_NAME s_joystick_get_axis
  SDL_Joystick *joy;

  ASSERT_JOYSTICK (joystick, ARGH1);
  ASSERT_EXACT (axis, ARGH2);

  joy = UNPACK_JOYSTICK (joystick);

  RETURN_INT (joy
              ? SDL_JoystickGetAxis (joy, gh_scm2long (axis))
              : -1);
#undef FUNC_NAME
}


DECLARE_SIMPLE_SYM (dx);
DECLARE_SIMPLE_SYM (dy);

GH_DEFPROC (joystick_get_ball, "joystick-get-ball", 2, 0, 0,
            (SCM joystick,
             SCM n),
            "For @var{joystick}, return relative motion of trackball\n"
            "@var{n}, as an alist with keys @code{dx} and @code{dy}.\n"
            "On error, return #f.")
{
#define FUNC_NAME s_joystick_get_ball
  SDL_Joystick *joy;
  int dx, dy;

  ASSERT_JOYSTICK (joystick, ARGH1);
  ASSERT_EXACT (n, ARGH2);

  joy = UNPACK_JOYSTICK (joystick);

  if (joy)
    {
      int ret;

      ret = SDL_JoystickGetBall (joy, gh_scm2long (n), &dx, &dy);

      if (ret != -1)
        RETURN_LIST2 (gh_cons (SYM (dx), gh_long2scm (dx)),
                      gh_cons (SYM (dy), gh_long2scm (dy)));
    }

  RETURN_FALSE;
#undef FUNC_NAME
}


GH_DEFPROC (joystick_get_hat, "joystick-get-hat", 2, 0, 0,
            (SCM joystick,
             SCM n),
            "For @var{joystick}, return state of hat @var{n}.")
{
#define FUNC_NAME s_joystick_get_hat
  SDL_Joystick *joy;

  ASSERT_JOYSTICK (joystick, ARGH1);
  ASSERT_EXACT (n, ARGH2);

  joy = UNPACK_JOYSTICK (joystick);

  RETURN_INT (joy
              ? SDL_JoystickGetHat (joy, gh_scm2long (n))
              : -1);
#undef FUNC_NAME
}


GH_DEFPROC (joystick_get_button, "joystick-get-button", 2, 0, 0,
            (SCM joystick,
             SCM n),
            "For @var{joystick}, return state of button @var{n}.")
{
#define FUNC_NAME s_joystick_get_button
  SDL_Joystick *joy;

  ASSERT_JOYSTICK (joystick, ARGH1);
  ASSERT_EXACT (n, ARGH2);

  joy = UNPACK_JOYSTICK (joystick);

  RETURN_INT (joy
              ? SDL_JoystickGetButton (joy, gh_scm2long (n))
              : -1);
#undef FUNC_NAME
}


GH_DEFPROC (joystick_close, "joystick-close", 1, 0, 0,
            (SCM joystick),
            "Close a previously opened @var{joystick}.\n"
            "The return value is unspecified.")
{
#define FUNC_NAME s_joystick_close
  SDL_Joystick *joy;

  ASSERT_JOYSTICK (joystick, ARGH1);
  joy = UNPACK_JOYSTICK (joystick);

  if (joy)
    {
      SDL_JoystickClose (joy);
      SMOBSET (joystick, NULL);
    }

  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


/*-------------------------------------------------------------*/

static
size_t
free_joy (SCM joystick)
{
  SDL_Joystick *joy = UNPACK_JOYSTICK (joystick);

  if (joy)
    SDL_JoystickClose (joy);

  return 0;
}

static
int
print_joy (SCM joystick, SCM port, scm_print_state *pstate)
{
  SDL_Joystick *joy = UNPACK_JOYSTICK (joystick);

  scm_puts      ("#<SDL-Joystick ", port);
  if (joy)
    scm_intprint                        (SDL_JoystickIndex (joy), 10, port);
  else
    scm_puts                            ("NULL", port);
  scm_putc      ('>', port);

  /* Non-zero means success */
  return 1;
}


void
gsdl_init_joystick (void)
{
  joystick_tag = scm_make_smob_type ("SDL-Joystick", sizeof (SDL_Joystick *));
  scm_set_smob_free  (joystick_tag, free_joy);
  scm_set_smob_print (joystick_tag, print_joy);

#include "sdljoystick.x"
}

/* sdljoystick.c ends here */
