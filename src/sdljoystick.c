/* sdl.c --- SDL Joystick functions for Guile
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
#include "sdlsmobs.h"


static long joystick_tag;

#define ASSERT_JOYSTICK(obj,which) \
  ASSERT_SMOB (obj, joystick_tag, which)


MDEFLOCEXP (sdl_joystick_p, "sdl-joystick?", 1, 0, 0,
            (SCM obj),
            "Return #t iff @var{obj} is a joystick smob.")
#define FUNC_NAME s_sdl_joystick_p
{
  return gh_bool2scm
    (SCM_SMOB_PREDICATE (joystick_tag, obj));
}
#undef FUNC_NAME


MDEFLOCEXP (sdl_joystick_null_p, "sdl-joystick-null?", 1, 0, 0,
            (SCM joy_smob),
            "Return #t iff @var{joystick} is a NULL joystick.")
#define FUNC_NAME s_sdl_joystick_null_p
{
  ASSERT_JOYSTICK (joy_smob, ARGH1);

  return gh_bool2scm
    (NULL == SMOBGET (joy_smob, SDL_Joystick *));
}
#undef FUNC_NAME


MDEFLOCEXP (sdl_num_joysticks, "sdl-num-joysticks", 0, 0, 0,
            (void),
            "Return the number of joysticks.")
#define FUNC_NAME s_sdl_num_joysticks
{
  return gh_long2scm (SDL_NumJoysticks ());
}
#undef FUNC_NAME


MDEFLOCEXP (sdl_joystick_name, "sdl-joystick-name", 0, 1, 0,
            (SCM s_index),
            "Return the name of the default joystick.\n"
            "Optional arg @var{n} specifies which joystick to check.")
#define FUNC_NAME s_sdl_joystick_name
{
  int index = 0;

  if (BOUNDP (s_index)) {
    ASSERT_EXACT (s_index, ARGH1);
    index = gh_scm2long (s_index);
  }

  return gh_str02scm (SDL_JoystickName (index));
}
#undef FUNC_NAME


MDEFLOCEXP (sdl_joystick_open, "sdl-joystick-open", 0, 1, 0,
            (SCM s_index),
            "Return a handle to the default joystick opened for use.\n"
            "Optional arg @var{n} specifies which joystick to open.")
#define FUNC_NAME s_sdl_joystick_open
{
  int index = 0;

  if (BOUNDP (s_index)) {
    ASSERT_EXACT (s_index, ARGH1);
    index = gh_scm2long (s_index);
  }

  SCM_RETURN_NEWSMOB (joystick_tag, SDL_JoystickOpen (index));
}
#undef FUNC_NAME


MDEFLOCEXP (sdl_joystick_opened_p, "sdl-joystick-opened?", 0, 1, 0,
            (SCM s_index),
            "Return #t iff the default joystick is opened.\n"
            "Optional arg @var{n} specifies which joystick to check.")
#define FUNC_NAME s_sdl_joystick_opened_p
{
  int index = 0;

  if (BOUNDP (s_index)) {
    ASSERT_EXACT (s_index, ARGH1);
    index = gh_scm2long (s_index);
  }

  return gh_bool2scm
    (SDL_JoystickOpened (index));
}
#undef FUNC_NAME


MDEFLOCEXP (sdl_joystick_index, "sdl-joystick-index", 1, 0, 0,
            (SCM joy_smob),
            "Return the index of @var{joystick}.")
#define FUNC_NAME s_sdl_joystick_index
{
  SDL_Joystick *joy;

  ASSERT_JOYSTICK (joy_smob, ARGH1);

  joy = SMOBGET (joy_smob, SDL_Joystick *);

  return gh_long2scm (joy != NULL
                      ? SDL_JoystickIndex (joy)
                      : -1);
}
#undef FUNC_NAME


MDEFLOCEXP (sdl_joystick_num_axes, "sdl-joystick-num-axes", 1, 0, 0,
            (SCM joy_smob),
            "Return the number of axes for @var{joystick}.")
#define FUNC_NAME s_sdl_joystick_num_axes
{
  SDL_Joystick *joy;

  ASSERT_JOYSTICK (joy_smob, ARGH1);

  joy = SMOBGET (joy_smob, SDL_Joystick *);

  return gh_long2scm (joy != NULL
                      ? SDL_JoystickNumAxes (joy)
                      : -1);
}
#undef FUNC_NAME


MDEFLOCEXP (sdl_joystick_num_balls, "sdl-joystick-num-balls", 1, 0, 0,
            (SCM joy_smob),
            "Return the number trackballs for @var{joystick}.")
#define FUNC_NAME s_sdl_joystick_num_balls
{
  SDL_Joystick *joy;

  ASSERT_JOYSTICK (joy_smob, ARGH1);

  joy = SMOBGET (joy_smob, SDL_Joystick *);

  return gh_long2scm (joy != NULL
                      ? SDL_JoystickNumBalls (joy)
                      : -1);
}
#undef FUNC_NAME


MDEFLOCEXP (sdl_joystick_num_hats, "sdl-joystick-num-hats", 1, 0, 0,
            (SCM joy_smob),
            "Return the number of hats for @var{joystick}.")
#define FUNC_NAME s_sdl_joystick_num_hats
{
  SDL_Joystick *joy;

  ASSERT_JOYSTICK (joy_smob, ARGH1);

  joy = SMOBGET (joy_smob, SDL_Joystick *);

  return gh_long2scm (joy != NULL
                      ? SDL_JoystickNumHats (joy)
                      : -1);
}
#undef FUNC_NAME


MDEFLOCEXP (sdl_joystick_num_buttons, "sdl-joystick-num-buttons", 1, 0, 0,
            (SCM joy_smob),
            "Return number of buttons for @var{joystick}.")
#define FUNC_NAME s_sdl_joystick_num_buttons
{
  SDL_Joystick *joy;

  ASSERT_JOYSTICK (joy_smob, ARGH1);

  joy = SMOBGET (joy_smob, SDL_Joystick *);

  return gh_long2scm (joy != NULL
                      ? SDL_JoystickNumButtons (joy)
                      : -1);
}
#undef FUNC_NAME


MDEFLOCEXP (sdl_joystick_update, "sdl-joystick-update", 0, 0, 0,
            (void),
            "Update the state of all Joysticks.")
#define FUNC_NAME s_sdl_joystick_update
{
  SDL_JoystickUpdate ();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


MDEFLOCEXP (sdl_joystick_event_state, "sdl-joystick-event-state", 1, 0, 0,
            (SCM s_state),
            "Set the Joystick event processing model to @var{state}.")
#define FUNC_NAME s_sdl_joystick_event_state
{
  ASSERT_EXACT (s_state, ARGH1);

  return gh_long2scm (SDL_JoystickEventState (gh_scm2long (s_state)));
}
#undef FUNC_NAME


MDEFLOCEXP (sdl_joystick_get_axis, "sdl-joystick-get-axis", 2, 0, 0,
            (SCM joy_smob,
             SCM s_index),
            "For @var{joystick}, return state of @var{axis}.")
#define FUNC_NAME s_sdl_joystick_get_axis
{
  SDL_Joystick *joy;

  ASSERT_JOYSTICK (joy_smob, ARGH1);
  ASSERT_EXACT (s_index, ARGH2);

  joy = SMOBGET (joy_smob, SDL_Joystick *);

  return gh_long2scm (joy != NULL
                      ? SDL_JoystickGetAxis (joy, gh_scm2long (s_index))
                      : -1);
}
#undef FUNC_NAME


SCM_SYMBOL (gsdl_sym_dx, "dx");
SCM_SYMBOL (gsdl_sym_dy, "dy");

MDEFLOCEXP (sdl_joystick_get_ball, "sdl-joystick-get-ball", 2, 0, 0,
            (SCM joy_smob,
             SCM s_index),
            "For @var{joystick}, return relative motion of trackball\n"
            "@var{n}, as an alist with keys @code{dx} and @code{dy}.")
#define FUNC_NAME s_sdl_joystick_get_ball
{
  SDL_Joystick *joy;
  SCM s_ret;
  int dx, dy;

  ASSERT_JOYSTICK (joy_smob, ARGH1);
  ASSERT_EXACT (s_index, ARGH2);

  joy = SMOBGET (joy_smob, SDL_Joystick *);

  s_ret = SCM_EOL;
  if (joy != NULL) {
    int ret;

    ret = SDL_JoystickGetBall (joy, gh_scm2long (s_index), &dx, &dy);

    if (ret != -1) {
      s_ret = SCM_LIST2 (gh_cons (gsdl_sym_dx, gh_long2scm (dx)),
                         gh_cons (gsdl_sym_dy, gh_long2scm (dy)));
    }
  }

  return s_ret;
}
#undef FUNC_NAME


MDEFLOCEXP (sdl_joystick_get_hat, "sdl-joystick-get-hat", 2, 0, 0,
            (SCM joy_smob,
             SCM s_index),
            "For @var{joystick}, return state of hat @var{n}.")
#define FUNC_NAME s_sdl_joystick_get_hat
{
  SDL_Joystick *joy;

  ASSERT_JOYSTICK (joy_smob, ARGH1);
  ASSERT_EXACT (s_index, ARGH2);

  joy = SMOBGET (joy_smob, SDL_Joystick *);

  return gh_long2scm (joy != NULL
                      ? SDL_JoystickGetHat (joy, gh_scm2long (s_index))
                      : -1);
}
#undef FUNC_NAME


MDEFLOCEXP (sdl_joystick_get_button, "sdl-joystick-get-button", 2, 0, 0,
            (SCM joy_smob,
             SCM s_index),
            "For @var{joystick}, return state of button @var{n}.")
#define FUNC_NAME s_sdl_joystick_get_button
{
  SDL_Joystick *joy;

  ASSERT_JOYSTICK (joy_smob, ARGH1);
  ASSERT_EXACT (s_index, ARGH2);

  joy = SMOBGET (joy_smob, SDL_Joystick *);

  return gh_long2scm (joy != NULL
                      ? SDL_JoystickGetButton (joy, gh_scm2long (s_index))
                      : -1);
}
#undef FUNC_NAME


MDEFLOCEXP (sdl_joystick_close, "sdl-joystick-close", 1, 0, 0,
            (SCM joy_smob),
            "Close a previously opened @var{joystick}.")
#define FUNC_NAME s_sdl_joystick_close
{
  SDL_Joystick *joy;

  ASSERT_JOYSTICK (joy_smob, ARGH1);
  joy = SMOBGET (joy_smob, SDL_Joystick *);

  if (joy != NULL) {
    SDL_JoystickClose (joy);
    SMOBSET (joy_smob, NULL);
  }

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


/*-------------------------------------------------------------*/

static
SCM
mark_joy (SCM joy_smob)
{
  return joy_smob;
}

static
size_t
free_joy (SCM joy_smob)
{
  SDL_Joystick *joy = SMOBGET (joy_smob, SDL_Joystick *);

  if (joy != NULL)
    SDL_JoystickClose (joy);

  return 0; /* No idea of how much is actually freed */
}

static
int
print_joy (SCM joy_smob, SCM port, scm_print_state *pstate)
{
  SDL_Joystick *joy = SMOBGET (joy_smob, SDL_Joystick *);

  scm_puts                          ("#<SDL-Joystick ", port);
  if (joy)
    scm_display (gh_long2scm (SDL_JoystickIndex (joy)), port);
  else
    scm_puts                                   ("NULL", port);
  scm_puts                                        (">", port);

  /* Non-zero means success */
  return 1;
}


void
gsdl_init_joystick (void)
{
  joystick_tag = scm_make_smob_type ("SDL-Joystick", sizeof (SDL_Joystick *));
  scm_set_smob_mark  (joystick_tag, mark_joy);
  scm_set_smob_free  (joystick_tag, free_joy);
  scm_set_smob_print (joystick_tag, print_joy);

#include "sdljoystick.x"
}

/* sdljoystick.c ends here */
