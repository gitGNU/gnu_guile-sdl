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

static smob_tag_t joystick_tag;

#define joystick_nick "SDL-Joystick"

#define ASSERT_JOYSTICK(obj,which) \
  ASSERT_SMOB (obj, joystick, which)

#define UNPACK_JOYSTICK(smob) \
  (SMOBGET (smob, SDL_Joystick *))

static SDL_Joystick *
assert_open_joystick (const char *FUNC_NAME, SCM obj, int which)
{
  SDL_Joystick *joy;

  ASSERT_JOYSTICK (obj, which);
  if (! (joy = UNPACK_JOYSTICK (obj)))
    SCM_MISC_ERROR ("joystick not open", SCM_EOL);

  return joy;
}

#define ASSERT_FIRST_ARG_OPEN_JOYSTICK()                                \
  SDL_Joystick *joy = assert_open_joystick (FUNC_NAME, joystick, 1)

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
  ASSERT_FIRST_ARG_OPEN_JOYSTICK ();

  RETURN_INT (SDL_JoystickIndex (joy));
#undef FUNC_NAME
}


PRIMPROC
(joystick_num_axes, "joystick-num-axes", 1, 0, 0,
 (SCM joystick),
 doc: /***********
Return the number of axes for @var{joystick}.  */)
{
#define FUNC_NAME s_joystick_num_axes
  ASSERT_FIRST_ARG_OPEN_JOYSTICK ();

  RETURN_INT (SDL_JoystickNumAxes (joy));
#undef FUNC_NAME
}


PRIMPROC
(joystick_num_balls, "joystick-num-balls", 1, 0, 0,
 (SCM joystick),
 doc: /***********
Return the number trackballs for @var{joystick}.  */)
{
#define FUNC_NAME s_joystick_num_balls
  ASSERT_FIRST_ARG_OPEN_JOYSTICK ();

  RETURN_INT (SDL_JoystickNumBalls (joy));
#undef FUNC_NAME
}


PRIMPROC
(joystick_num_hats, "joystick-num-hats", 1, 0, 0,
 (SCM joystick),
 doc: /***********
Return the number of hats for @var{joystick}.  */)
{
#define FUNC_NAME s_joystick_num_hats
  ASSERT_FIRST_ARG_OPEN_JOYSTICK ();

  RETURN_INT (SDL_JoystickNumHats (joy));
#undef FUNC_NAME
}


PRIMPROC
(joystick_num_buttons, "joystick-num-buttons", 1, 0, 0,
 (SCM joystick),
 doc: /***********
Return number of buttons for @var{joystick}.  */)
{
#define FUNC_NAME s_joystick_num_buttons
  ASSERT_FIRST_ARG_OPEN_JOYSTICK ();

  RETURN_INT (SDL_JoystickNumButtons (joy));
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
(joystick_polling, "joystick-polling", 0, 1, 0,
 (SCM setting),
 doc: /***********
Return @code{#t} if joystick events are polled and queued (such
that it is unnecessary to ``manually'' call @code{joystick-update}),
otherwise @code{#f}.
If @var{setting} is specified, set joystick events polling
to the truth value of @var{setting} first.  */)
{
#define FUNC_NAME s_joystick_polling
  return BOOLEAN (SDL_ENABLE == SDL_JoystickEventState
                  (CSTATE_FROM_SETTING (setting)));
#undef FUNC_NAME
}


PRIMPROC
(joystick_event_state, "joystick-event-state", 1, 0, 0,
 (SCM state),
 doc: /***********
NB: This procedure is obsoleted by @code{joystick-polling}
and @strong{will be removed} after 2013-12-31.

Set or query the state of internal joystick event processing,
based on @var{state} (a symbol).
If @var{state} is @code{SDL_QUERY}, return the current state.
If it is @code{SDL_IGNORE} or @code{SDL_ENABLE},
disable or enable, respectively, internal joystick
event processing and return @var{state}.
When enabled, it is not necessary to call @code{joystick-update}.  */)
{
#define FUNC_NAME s_joystick_event_state
#define ESE btw->event_state_enum
  DECLINIT_SYM2NUM_CC    (1, ESE);
  int cstate = ENUM2LONG (1, state);
  int ret = SDL_JoystickEventState (cstate);

  return btw->long2enum (ret, ESE);
#undef ESE
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
  ASSERT_FIRST_ARG_OPEN_JOYSTICK ();

  ASSERT_INTEGER (axis, 2);

  RETURN_INT (SDL_JoystickGetAxis (joy, C_LONG (axis)));
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
  ASSERT_FIRST_ARG_OPEN_JOYSTICK ();
  int dx, dy;

  ASSERT_INTEGER (n, 2);

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
If @var{n} is invalid, return @code{#f}.  */)
{
#define FUNC_NAME s_joystick_get_ball
  ASSERT_FIRST_ARG_OPEN_JOYSTICK ();
  int dx, dy;

  ASSERT_INTEGER (n, 2);

  return 0 > SDL_JoystickGetBall (joy, C_LONG (n), &dx, &dy)
    ? BOOL_FALSE
    : LIST2 (CONS (SYM (dx), NUM_LONG (dx)),
             CONS (SYM (dy), NUM_LONG (dy)));
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
  ASSERT_FIRST_ARG_OPEN_JOYSTICK ();

  ASSERT_INTEGER (n, 2);

  RETURN_INT (SDL_JoystickGetHat (joy, C_LONG (n)));
#undef FUNC_NAME
}


PRIMPROC
(joystick_get_button, "joystick-get-button", 2, 0, 0,
 (SCM joystick,
  SCM n),
 doc: /***********
For @var{joystick}, return state of button @var{n},
a symbol, one of: @code{released} or @code{pressed}.  */)
{
#define FUNC_NAME s_joystick_get_button
  ASSERT_FIRST_ARG_OPEN_JOYSTICK ();

  ASSERT_INTEGER (n, 2);

  return btw->long2enum (SDL_JoystickGetButton (joy, C_LONG (n)),
                         btw->updn_enum);
#undef FUNC_NAME
}


PRIMPROC
(joystick_close, "joystick-close", 1, 0, 0,
 (SCM joystick),
 doc: /***********
Close a previously opened @var{joystick}.  */)
{
#define FUNC_NAME s_joystick_close
  ASSERT_FIRST_ARG_OPEN_JOYSTICK ();

  SDL_JoystickClose (joy);
  SMOBSET (joystick, NULL);
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
print_joy (SCM joystick, SCM port, UNUSED scm_print_state *pstate)
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
