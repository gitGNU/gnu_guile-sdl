/* sdl.c --- SDL Joystick functions for Guile
 *
 * Copyright (C) 2003, 2004, 2005, 2007, 2009, 2011, 2013 Thien-Thi Nguyen
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

#include "guile-sdl.h"
#include <stdio.h>
#include "b-values.h"

static long joystick_tag;

#define joystick_nick "SDL-Joystick"

#define ASSERT_JOYSTICK(obj,which) \
  ASSERT_SMOB (obj, joystick, which)

#define UNPACK_JOYSTICK(smob) \
  (SMOBGET (smob, SDL_Joystick *))

#define RETURN_NEW_JOYSTICK(x) \
  NEWSMOB_OR_FALSE (joystick_tag, x)

#define JOYSTICK_P(x) \
  (SCM_SMOB_PREDICATE (joystick_tag, x))



PRIMPROC
(joystick_p, "joystick?", 1, 0, 0,
 (SCM obj),
 doc: /***********
Return @code{#t} iff @var{obj} is a joystick object.  */)
{
#define FUNC_NAME s_joystick_p
  RETURN_BOOL
    (JOYSTICK_P (obj));
#undef FUNC_NAME
}


PRIMPROC
(num_joysticks, "num-joysticks", 0, 0, 0,
 (void),
 doc: /***********
Return the number of joysticks.  */)
{
#define FUNC_NAME s_num_joysticks
  RETURN_INT (SDL_NumJoysticks ());
#undef FUNC_NAME
}


PRIMPROC
(joystick_name, "joystick-name", 0, 1, 0,
 (SCM n),
 doc: /***********
Return the (string) name of the default joystick, or @code{#f}.
Optional arg @var{n} specifies which joystick to check.  */)
{
#define FUNC_NAME s_joystick_name
  int cn = 0;

  if (BOUNDP (n))
    ASSERT_LONG_COPY (n, 1);

  RETURN_0STR_OR_FALSE (SDL_JoystickName (cn));
#undef FUNC_NAME
}


PRIMPROC
(joystick_open, "joystick-open", 0, 1, 0,
 (SCM n),
 doc: /***********
Return a handle to the default joystick opened for use.
Optional arg @var{n} specifies which joystick to open.  */)
{
#define FUNC_NAME s_joystick_open
  int cn = 0;

  if (BOUNDP (n))
    ASSERT_LONG_COPY (n, 1);

  RETURN_NEW_JOYSTICK (SDL_JoystickOpen (cn));
#undef FUNC_NAME
}


PRIMPROC
(joystick_opened_p, "joystick-opened?", 0, 1, 0,
 (SCM n),
 doc: /***********
Return @code{#t} iff the default joystick is opened.
Optional arg @var{n} specifies which joystick to check.  */)
{
#define FUNC_NAME s_joystick_opened_p
  int cn = 0;

  if (BOUNDP (n))
    ASSERT_LONG_COPY (n, 1);

  RETURN_BOOL
    (SDL_JoystickOpened (cn));
#undef FUNC_NAME
}


PRIMPROC
(joystick_index, "joystick-index", 1, 0, 0,
 (SCM joystick),
 doc: /***********
Return the index of @var{joystick}.  */)
{
#define FUNC_NAME s_joystick_index
  SDL_Joystick *joy;

  ASSERT_JOYSTICK (joystick, 1);

  joy = UNPACK_JOYSTICK (joystick);

  RETURN_INT (joy
              ? SDL_JoystickIndex (joy)
              : -1);
#undef FUNC_NAME
}


PRIMPROC
(joystick_num_axes, "joystick-num-axes", 1, 0, 0,
 (SCM joystick),
 doc: /***********
Return the number of axes for @var{joystick}.  */)
{
#define FUNC_NAME s_joystick_num_axes
  SDL_Joystick *joy;

  ASSERT_JOYSTICK (joystick, 1);

  joy = UNPACK_JOYSTICK (joystick);

  RETURN_INT (joy
              ? SDL_JoystickNumAxes (joy)
              : -1);
#undef FUNC_NAME
}


PRIMPROC
(joystick_num_balls, "joystick-num-balls", 1, 0, 0,
 (SCM joystick),
 doc: /***********
Return the number trackballs for @var{joystick}.  */)
{
#define FUNC_NAME s_joystick_num_balls
  SDL_Joystick *joy;

  ASSERT_JOYSTICK (joystick, 1);

  joy = UNPACK_JOYSTICK (joystick);

  RETURN_INT (joy
              ? SDL_JoystickNumBalls (joy)
              : -1);
#undef FUNC_NAME
}


PRIMPROC
(joystick_num_hats, "joystick-num-hats", 1, 0, 0,
 (SCM joystick),
 doc: /***********
Return the number of hats for @var{joystick}.  */)
{
#define FUNC_NAME s_joystick_num_hats
  SDL_Joystick *joy;

  ASSERT_JOYSTICK (joystick, 1);

  joy = UNPACK_JOYSTICK (joystick);

  RETURN_INT (joy
              ? SDL_JoystickNumHats (joy)
              : -1);
#undef FUNC_NAME
}


PRIMPROC
(joystick_num_buttons, "joystick-num-buttons", 1, 0, 0,
 (SCM joystick),
 doc: /***********
Return number of buttons for @var{joystick}.  */)
{
#define FUNC_NAME s_joystick_num_buttons
  SDL_Joystick *joy;

  ASSERT_JOYSTICK (joystick, 1);

  joy = UNPACK_JOYSTICK (joystick);

  RETURN_INT (joy
              ? SDL_JoystickNumButtons (joy)
              : -1);
#undef FUNC_NAME
}


PRIMPROC
(joystick_update, "joystick-update", 0, 0, 0,
 (void),
 doc: /***********
Update the state of all Joysticks.  */)
{
#define FUNC_NAME s_joystick_update
  SDL_JoystickUpdate ();
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


PRIMPROC
(joystick_event_state, "joystick-event-state", 1, 0, 0,
 (SCM state),
 doc: /***********
Set the Joystick event processing model to @var{state}.  */)
{
#define FUNC_NAME s_joystick_event_state
  ASSERT_INTEGER (state, 1);

  RETURN_INT (SDL_JoystickEventState (C_LONG (state)));
#undef FUNC_NAME
}


PRIMPROC
(joystick_get_axis, "joystick-get-axis", 2, 0, 0,
 (SCM joystick,
  SCM axis),
 doc: /***********
For @var{joystick}, return state of @var{axis}.  */)
{
#define FUNC_NAME s_joystick_get_axis
  SDL_Joystick *joy;

  ASSERT_JOYSTICK (joystick, 1);
  ASSERT_INTEGER (axis, 2);

  joy = UNPACK_JOYSTICK (joystick);

  RETURN_INT (joy
              ? SDL_JoystickGetAxis (joy, C_LONG (axis))
              : -1);
#undef FUNC_NAME
}


PRIMPROC
(joystick_ball_xy, "joystick-ball-xy", 2, 0, 0,
 (SCM joystick,
  SCM n),
 doc: /***********
Return relative motion of @var{joystick} trackball @var{n}
as two values: @code{dx} and @code{dy} (both integers).  */)
{
#define FUNC_NAME s_joystick_ball_xy
  SDL_Joystick *joy;
  int dx, dy;

  ASSERT_JOYSTICK (joystick, 1);
  ASSERT_INTEGER (n, 2);

  joy = UNPACK_JOYSTICK (joystick);
  if (0 > SDL_JoystickGetBall (joy, C_LONG (n), &dx, &dy))
    SCM_MISC_ERROR ("invalid parameter", SCM_EOL);

  RETURN_VALUES2
    (NUM_LONG (dx),
     NUM_LONG (dy));
#undef FUNC_NAME
}


DECLARE_SIMPLE_SYM (dx);
DECLARE_SIMPLE_SYM (dy);

PRIMPROC
(joystick_get_ball, "joystick-get-ball", 2, 0, 0,
 (SCM joystick,
  SCM n),
 doc: /***********
NB: This procedure is obsoleted by @code{joystick-ball-xy}
and @strong{will be removed} after 2013-12-31.

For @var{joystick}, return relative motion of trackball
@var{n}, as an alist with keys @code{dx} and @code{dy}.
On error, return @code{#f}.  */)
{
#define FUNC_NAME s_joystick_get_ball
  SDL_Joystick *joy;
  int dx, dy;

  ASSERT_JOYSTICK (joystick, 1);
  ASSERT_INTEGER (n, 2);

  joy = UNPACK_JOYSTICK (joystick);

  if (joy)
    {
      int ret;

      ret = SDL_JoystickGetBall (joy, C_LONG (n), &dx, &dy);

      if (ret != -1)
        RETURN_LIST2 (CONS (SYM (dx), NUM_LONG (dx)),
                      CONS (SYM (dy), NUM_LONG (dy)));
    }

  RETURN_FALSE;
#undef FUNC_NAME
}


PRIMPROC
(joystick_get_hat, "joystick-get-hat", 2, 0, 0,
 (SCM joystick,
  SCM n),
 doc: /***********
For @var{joystick}, return state of hat @var{n}.  */)
{
#define FUNC_NAME s_joystick_get_hat
  SDL_Joystick *joy;

  ASSERT_JOYSTICK (joystick, 1);
  ASSERT_INTEGER (n, 2);

  joy = UNPACK_JOYSTICK (joystick);

  RETURN_INT (joy
              ? SDL_JoystickGetHat (joy, C_LONG (n))
              : -1);
#undef FUNC_NAME
}


PRIMPROC
(joystick_get_button, "joystick-get-button", 2, 0, 0,
 (SCM joystick,
  SCM n),
 doc: /***********
For @var{joystick}, return state of button @var{n}.  */)
{
#define FUNC_NAME s_joystick_get_button
  SDL_Joystick *joy;

  ASSERT_JOYSTICK (joystick, 1);
  ASSERT_INTEGER (n, 2);

  joy = UNPACK_JOYSTICK (joystick);

  RETURN_INT (joy
              ? SDL_JoystickGetButton (joy, C_LONG (n))
              : -1);
#undef FUNC_NAME
}


PRIMPROC
(joystick_close, "joystick-close", 1, 0, 0,
 (SCM joystick),
 doc: /***********
Close a previously opened @var{joystick}.  */)
{
#define FUNC_NAME s_joystick_close
  SDL_Joystick *joy;

  ASSERT_JOYSTICK (joystick, 1);
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
  char buf[32];

  snprintf (buf, 32, "#<%s %d>", joystick_nick,
            joy ? SDL_JoystickIndex (joy) : -1);
  scm_puts (buf, port);
  return 1;
}


void
gsdl_init_joystick (void)
{
  DEFSMOB (joystick_tag, joystick_nick,
           NULL,
           free_joy,
           print_joy);

#include "sdljoystick.x"
}

/* sdljoystick.c ends here */
