/* sdlevent.c --- SDL input handling for Guile
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
#include <SDL/SDL_events.h>

#include "config.h"
#include "argcheck.h"
#include "sdlsmobs.h"
#include "sdlenums.h"
#include "wholefns.h"

/* enum/flag types */
SCM event_type_enum;
SCM event_state_enum;
SCM event_keysym_enum;

static SCM event_mod_flags;

MDEFLOCEXP (sdl_get_event_mod_flags, "flagstash:event-mod", 0, 0, 0, (),
            "Return the flagstash object for event mod flags.")
{
  return event_mod_flags;
}


static long event_tag;

#define ASSERT_EVENT(obj,which) \
  ASSERT_SMOB (obj, event_tag, which)

static
SCM
mark_event (SCM event)
{
  return event;
}

static
size_t
free_event (SCM event)
{
  free (SMOBGET (event, SDL_Event *));
  /* return sizeof (SDL_Event); */
  return 0;
}


static long keysym_tag;

#define ASSERT_KEYSYMe(obj,which) \
  ASSERT_SMOB (obj, keysym_tag, which)

static
SCM
mark_keysym (SCM keysym)
{
  return keysym;
}

static
size_t
free_keysym (SCM keysym)
{
  SDL_keysym *k = SMOBGET (keysym, SDL_keysym *);
  free (k);
  return sizeof (SDL_keysym);
}


/* constructors */

MDEFLOCEXP (make_event, "sdl-make-event", 0, 1, 0,
            (SCM s_type),
            "Create a new SDL event.")
#define FUNC_NAME s_make_event
{
  SDL_Event *event;
  int type=SDL_NOEVENT;

  if (BOUNDP (s_type)) {
    type = gsdl_enum2long (s_type, event_type_enum, ARGH1, FUNC_NAME);
  }

  event = (SDL_Event *) scm_must_malloc (sizeof (SDL_Event), FUNC_NAME);
  event->type = type;

  SCM_RETURN_NEWSMOB (event_tag, event);
}
#undef FUNC_NAME

MDEFLOCEXP (make_keysym, "sdl-make-keysym", 0, 2, 0,
            (SCM sym, SCM mod),
            "")
#define FUNC_NAME s_make_keysym
{
  SDL_keysym *keysym;

  /* alloc the keysym */
  keysym = (SDL_keysym *) scm_must_malloc (sizeof (SDL_keysym), FUNC_NAME);

  /* set the sym if given */
  if (BOUNDP (sym)) {
    ASSERT_EXACT (sym, ARGH1);
    /* keysym->sym = (SDLKey) gh_scm2long (sym); */
    keysym->sym = (SDLKey) gsdl_enum2long (sym, event_keysym_enum,
                                           ARGH1, FUNC_NAME);
  }

  /* set the mod if given */
  if (BOUNDP (mod)) {
    ASSERT_EXACT (mod, ARGH2);
    keysym->mod = (SDLMod) GSDL_FLAGS2ULONG (mod, event_mod_flags, ARGH2);
  }

  /* return the new smob */
  SCM_RETURN_NEWSMOB (keysym_tag, keysym);
}
#undef FUNC_NAME


/* smob getters and setters */


#define ENUM_GETTER(s_frag, c_frag, c_field, etypefrag) \
  GSDL_ENUM_GETTER ("sdl-event:" s_frag,                \
                    event_ ## c_frag,                   \
                    event_tag, SDL_Event *, c_field,    \
                    etypefrag ## _enum)

#define ENUM_SETTER(s_frag, c_frag, c_field, etypefrag) \
  GSDL_ENUM_SETTER ("sdl-event:" s_frag,                \
                    event_ ## c_frag,                   \
                    event_tag, SDL_Event *,             \
                    c_field, etypefrag ## _enum)

#define ENUM_GETSET(get_s, get_c, set_s, set_c, c_field, etypefrag)     \
  ENUM_GETTER (get_s, get_c, c_field, etypefrag)                        \
  ENUM_SETTER (set_s, set_c, c_field, etypefrag)


#define NUMBER_GETTER(s_frag, c_frag, c_field)          \
  GSDL_NUMBER_GETTER ("sdl-event:" s_frag,              \
                      event_ ## c_frag,                 \
                      event_tag, SDL_Event *, c_field)

#define NUMBER_SETTER(s_frag, c_frag, c_field)  \
  GSDL_NUMBER_SETTER ("sdl-event:" s_frag,      \
                      event_ ## c_frag,         \
                      event_tag, SDL_Event *,   \
                      c_field)

#define NUM2_GETTER(a    ,    b)                \
  NUMBER_GETTER   (#a   ":"  #b,                \
                    a ## _ ## b,                \
                    a ## . ## b)

#define NUM2_SETTER(a      ,      b)            \
  NUMBER_SETTER   (#a   ":set-"  #b "!",        \
                    a ## _set_ ## b,            \
                    a ##   .   ## b)

#define NUM2_GETSET(a, b) \
  NUM2_GETTER (a, b)      \
  NUM2_SETTER (a, b)


#define NUM3_GETTER(a    ,    b    ,    c)      \
  NUMBER_GETTER   (#a   ":"  #b   ":"  #c,      \
                    a ## _ ## b ## _ ## c,      \
                    a ## . ## b ## . ## c)

#define NUM3_SETTER(a    ,    b      ,      c)          \
  NUMBER_SETTER   (#a   ":"  #b   ":set-"  #c "!",      \
                    a ## _ ## b ## _set_ ## c,          \
                    a ## . ## b ##   .   ## c)

#define NUM3_GETSET(a, b, c) \
  NUM3_GETTER (a, b, c)      \
  NUM3_SETTER (a, b, c)


#define FLAG_GETTER(s_frag, c_frag, c_field, stash)     \
  GSDL_FLAG_GETTER ("sdl-event:" s_frag,                \
                    event_ ## c_frag,                   \
                    event_tag, SDL_Event *,             \
                    c_field, stash)

#define FLAG_SETTER(s_frag, c_frag, c_field, stash)     \
  GSDL_FLAG_SETTER ("sdl-event:" s_frag,                \
                    event_ ## c_frag,                   \
                    event_tag, SDL_Event *,             \
                    c_field, stash)

#define FLAG_GETSET(get_s, get_c, set_s, set_c, c_field, stash) \
  FLAG_GETTER (get_s, get_c, c_field, stash)                    \
  FLAG_SETTER (set_s, set_c, c_field, stash)



ENUM_GETSET ("type",          type,
             "set-type!", set_type,
             type,
             event_type)

NUM2_GETSET (active, gain)
NUM2_GETSET (active, state)
NUM2_GETSET (key, state)

ENUM_GETSET ("key:keysym:sym",        key_keysym_sym,
             "key:keysym:set-sym!", key_keysym_set_sym,
             key.keysym.sym,
             event_keysym)

FLAG_GETSET ("key:keysym:mod",        key_keysym_mod,
             "key:keysym:set-mod!", key_keysym_set_mod,
             key.keysym.mod,
             event_mod_flags)

NUM3_GETSET (key, keysym, scancode)
NUM3_GETSET (key, keysym, unicode)

NUM2_GETSET (motion, state)
NUM2_GETSET (motion, x)
NUM2_GETSET (motion, y)
NUM2_GETSET (motion, xrel)
NUM2_GETSET (motion, yrel)

NUM2_GETSET (button, button)
NUM2_GETSET (button, state)
NUM2_GETSET (button, x)
NUM2_GETSET (button, y)

NUM2_GETSET (jaxis, which)
NUM2_GETSET (jaxis, axis)
NUM2_GETSET (jaxis, value)

NUM2_GETSET (jbutton, which)
NUM2_GETSET (jbutton, button)
NUM2_GETSET (jbutton, state)

NUM2_GETSET (jball, which)
NUM2_GETSET (jball, ball)
NUM2_GETSET (jball, xrel)
NUM2_GETSET (jball, yrel)

NUM2_GETSET (jhat, which)
NUM2_GETSET (jhat, hat)
NUM2_GETSET (jhat, value)

NUM2_GETSET (resize, w)
NUM2_GETSET (resize, h)

#if 0 /* what is this? --ttn */
NUM2_GETSET (user, code)
NUM2_GETSET (user, data1)
NUM2_GETSET (user, data2)
#endif /* 0 */


/* SDL event functions */

MDEFLOCEXP (pump_events, "pump-events", 0, 0, 0, (), "")
#define FUNC_NAME s_pump_events
{
  SDL_PumpEvents ();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

MDEFLOCEXP (peep_events, "peep-events", 4, 0, 0,
            (SCM events, SCM numevents, SCM action, SCM mask),
            "")
#define FUNC_NAME s_peep_events
{
  scm_misc_error (FUNC_NAME, "not yet implemented (sorry)", SCM_EOL);
  /*  int SDL_PeepEvents(SDL_Event *events, int numevents, */
  /*                     SDL_eventaction action, Uint32 mask); */
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

MDEFLOCEXP (poll_event, "poll-event", 0, 1, 0,
            (SCM event),
            "")
#define FUNC_NAME s_poll_event
{
  int result;

  if (UNBOUNDP (event)) {
    /* no args */
    result = SDL_PollEvent (NULL);
  } else {
    /* we're given an event smob - fill it */
    ASSERT_SMOB (event, event_tag, ARGH1);
    result = SDL_PollEvent (SMOBGET (event, SDL_Event *));
  }

  return gh_bool2scm
    (result);
}
#undef FUNC_NAME

MDEFLOCEXP (wait_event, "wait-event", 0, 1, 0,
            (SCM event),
            "")
#define FUNC_NAME s_wait_event
{
  int result;

  if (UNBOUNDP (event)) {
    /* no args */
    result = SDL_WaitEvent (NULL);
  } else {
    /* we're given an event smob - fill it */
    ASSERT_SMOB (event, event_tag, ARGH1);
    result = SDL_WaitEvent (SMOBGET (event, SDL_Event *));
  }

  return gh_bool2scm
    (result);
}
#undef FUNC_NAME

MDEFLOCEXP (push_event, "push-event", 1, 0, 0,
            (SCM event),
            "")
#define FUNC_NAME s_push_event
{
  int result;

  ASSERT_SMOB (event, event_tag, ARGH1);

  result = SDL_PushEvent (SMOBGET (event, SDL_Event *));
  return gh_long2scm (result);
}
#undef FUNC_NAME

MDEFLOCEXP (set_event_filter, "set-event-filter", 1, 0, 0,
            (SCM filter),
            "")
#define FUNC_NAME s_set_event_filter
{
  scm_misc_error (FUNC_NAME, "not yet implemented (sorry)", SCM_EOL);
  /* extern DECLSPEC void SDL_SetEventFilter(SDL_EventFilter filter); */
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

MDEFLOCEXP (get_event_filter, "get-event-filter", 1, 0, 0,
            (SCM filter),
            "")
#define FUNC_NAME s_get_event_filter
{
  scm_misc_error (FUNC_NAME, "not yet implemented (sorry)", SCM_EOL);
  /* extern DECLSPEC SDL_EventFilter SDL_GetEventFilter(void); */
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

MDEFLOCEXP (event_state, "event-state", 2, 0, 0,
            (SCM type, SCM state),
            "")
#define FUNC_NAME s_event_state
{
  scm_misc_error (FUNC_NAME, "not yet implemented (sorry)", SCM_EOL);
  /* extern DECLSPEC Uint8 SDL_EventState(Uint8 type, int state); */
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


MDEFLOCEXP (sdl_enable_unicode, "sdl-enable-unicode", 0, 1, 0,
            (SCM enable_p),
            "")
#define FUNC_NAME s_sdl_enable_unicode
{
  int result;

  if (UNBOUNDP (enable_p)) {
    result = SDL_EnableUNICODE (-1);
  } else if (! gh_scm2bool (enable_p)) {
    result = SDL_EnableUNICODE (0);
  } else {
    result = SDL_EnableUNICODE (1);
  }

  return gh_long2scm (result);
}
#undef FUNC_NAME

/*
 * If 'delay' is set to 0, keyboard repeat is disabled.
 */

MDEFLOCEXP (sdl_enable_key_repeat, "sdl-enable-key-repeat", 2, 0, 0,
            (SCM s_delay, SCM s_interval),
            "")
#define FUNC_NAME s_sdl_enable_key_repeat
{
  int interval, delay;

  ASSERT_EXACT (s_delay, ARGH1);
  ASSERT_EXACT (s_interval, ARGH2);

  delay    = gh_scm2long (s_delay);
  interval = gh_scm2long (s_interval);

  RETURN_TRUE_IF_0 (SDL_EnableKeyRepeat (delay, interval));
}
#undef FUNC_NAME

MDEFLOCEXP (sdl_get_key_state, "sdl-get-key-state", 1, 0, 0,
            (SCM numkeys),
            "Get a snapshot of the current state of the keyboard.\n"
            "Return an array of keystates, indexed by the SDLK_* syms.")
#define FUNC_NAME s_sdl_get_key_state
{
  scm_misc_error (FUNC_NAME, "not yet implemented (sorry)", SCM_EOL);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

MDEFLOCEXP (sdl_get_mod_state, "sdl-get-mod-state", 0, 0, 0,
            (),
            "Get the current key modifier state.")
#define FUNC_NAME s_sdl_get_mod_state
{
  return gsdl_ulong2flags (SDL_GetModState (), event_mod_flags);
}
#undef FUNC_NAME

MDEFLOCEXP (sdl_set_mod_state, "sdl-set-mod-state", 1, 0, 0,
            (SCM modstate),
            "Set the current key modifier state.\n"
            "This does not change the keyboard state,\n"
            "only the key modifier flags.")
#define FUNC_NAME s_sdl_set_mod_state
{
  ASSERT_EXACT (modstate, ARGH1);
  SDL_SetModState (GSDL_FLAGS2ULONG (modstate, event_mod_flags, ARGH1));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_SYMBOL (gsdl_sym_state, "state");
SCM_SYMBOL (gsdl_sym_x, "x");
SCM_SYMBOL (gsdl_sym_y, "y");

MDEFLOCEXP (sdl_get_mouse_state, "sdl-get-mouse-state", 0, 0, 0,
            (void),
            "Retrieve the current state of the mouse.")
#define FUNC_NAME s_sdl_get_mouse_state
{
  int buttons, x, y;
  buttons = SDL_GetMouseState (&x, &y);
  return SCM_LIST3 (gh_cons (gsdl_sym_state, gh_long2scm (buttons)),
                    gh_cons (gsdl_sym_x, gh_long2scm (x)),
                    gh_cons (gsdl_sym_y, gh_long2scm (y)));
}
#undef FUNC_NAME

MDEFLOCEXP (sdl_get_relative_mouse_state, "sdl-get-mouse-relative-state",
            0, 0, 0, (),
            "Retrieve the current state of the mouse.")
#define FUNC_NAME s_sdl_get_relative_mouse_state
{
  int buttons, x, y;
  buttons = SDL_GetRelativeMouseState (&x, &y);
  return SCM_LIST3 (gh_cons (gsdl_sym_state, gh_long2scm (buttons)),
                    gh_cons (gsdl_sym_x, gh_long2scm (x)),
                    gh_cons (gsdl_sym_y, gh_long2scm (y)));
}
#undef FUNC_NAME

MDEFLOCEXP (sdl_button_p, "sdl-button?", 1, 0, 0,
            (SCM mask),
            "")
#define FUNC_NAME s_sdl_button_p
{
  ASSERT_EXACT (mask, ARGH1);
  return gh_bool2scm
    (SDL_BUTTON (gh_scm2long (mask)));
}
#undef FUNC_NAME


extern flagstash_t gsdl_kmod_flagstash;

/* Initialize glue */
void
gsdl_init_event (void)
{
  event_tag = scm_make_smob_type ("SDL-Event",  sizeof (SDL_Event));
  scm_set_smob_mark (event_tag, mark_event);
  scm_set_smob_free (event_tag, free_event);

  keysym_tag = scm_make_smob_type ("SDL-Keysym", sizeof (SDL_keysym));
  scm_set_smob_mark (keysym_tag, mark_keysym);
  scm_set_smob_free (keysym_tag, free_keysym);

  /* event type constants */
  event_type_enum = gsdl_define_enum (
    "sdl-event-types",
    "SDL_ACTIVEEVENT",      SDL_ACTIVEEVENT,
    "SDL_KEYDOWN",          SDL_KEYDOWN,
    "SDL_KEYUP",            SDL_KEYUP,
    "SDL_MOUSEMOTION",      SDL_MOUSEMOTION,
    "SDL_MOUSEBUTTONDOWN",  SDL_MOUSEBUTTONDOWN,
    "SDL_MOUSEBUTTONUP",    SDL_MOUSEBUTTONUP,
    "SDL_JOYAXISMOTION",    SDL_JOYAXISMOTION,
    "SDL_JOYBALLMOTION",    SDL_JOYBALLMOTION,
    "SDL_JOYHATMOTION",     SDL_JOYHATMOTION,
    "SDL_JOYBUTTONDOWN",    SDL_JOYBUTTONDOWN,
    "SDL_JOYBUTTONUP",      SDL_JOYBUTTONUP,
    "SDL_QUIT",             SDL_QUIT,
    "SDL_SYSWMEVENT",       SDL_SYSWMEVENT,
    "SDL_VIDEORESIZE",      SDL_VIDEORESIZE,
    "SDL_USEREVENT",        SDL_USEREVENT,
    NULL);

  /* keysyms */
  event_keysym_enum = gsdl_define_enum (
    "sdl-event-keys",
    "SDLK_BACKSPACE",  SDLK_BACKSPACE,
    "SDLK_TAB",  SDLK_TAB,
    "SDLK_CLEAR",  SDLK_CLEAR,
    "SDLK_RETURN",  SDLK_RETURN,
    "SDLK_PAUSE",  SDLK_PAUSE,
    "SDLK_ESCAPE",  SDLK_ESCAPE,
    "SDLK_SPACE",  SDLK_SPACE,
    "SDLK_EXCLAIM",  SDLK_EXCLAIM,
    "SDLK_QUOTEDBL",  SDLK_QUOTEDBL,
    "SDLK_HASH",  SDLK_HASH,
    "SDLK_DOLLAR",  SDLK_DOLLAR,
    "SDLK_AMPERSAND",  SDLK_AMPERSAND,
    "SDLK_QUOTE",  SDLK_QUOTE,
    "SDLK_LEFTPAREN",  SDLK_LEFTPAREN,
    "SDLK_RIGHTPAREN",  SDLK_RIGHTPAREN,
    "SDLK_ASTERISK",  SDLK_ASTERISK,
    "SDLK_PLUS",  SDLK_PLUS,
    "SDLK_COMMA",  SDLK_COMMA,
    "SDLK_MINUS",  SDLK_MINUS,
    "SDLK_PERIOD",  SDLK_PERIOD,
    "SDLK_SLASH",  SDLK_SLASH,
    "SDLK_0",  SDLK_0,
    "SDLK_1",  SDLK_1,
    "SDLK_2",  SDLK_2,
    "SDLK_3",  SDLK_3,
    "SDLK_4",  SDLK_4,
    "SDLK_5",  SDLK_5,
    "SDLK_6",  SDLK_6,
    "SDLK_7",  SDLK_7,
    "SDLK_8",  SDLK_8,
    "SDLK_9",  SDLK_9,
    "SDLK_COLON",  SDLK_COLON,
    "SDLK_SEMICOLON",  SDLK_SEMICOLON,
    "SDLK_LESS",  SDLK_LESS,
    "SDLK_EQUALS",  SDLK_EQUALS,
    "SDLK_GREATER",  SDLK_GREATER,
    "SDLK_QUESTION",  SDLK_QUESTION,
    "SDLK_AT",  SDLK_AT,
    "SDLK_LEFTBRACKET",  SDLK_LEFTBRACKET,
    "SDLK_BACKSLASH",  SDLK_BACKSLASH,
    "SDLK_RIGHTBRACKET",  SDLK_RIGHTBRACKET,
    "SDLK_CARET",  SDLK_CARET,
    "SDLK_UNDERSCORE",  SDLK_UNDERSCORE,
    "SDLK_BACKQUOTE",  SDLK_BACKQUOTE,
    "SDLK_a",  SDLK_a,
    "SDLK_b",  SDLK_b,
    "SDLK_c",  SDLK_c,
    "SDLK_d",  SDLK_d,
    "SDLK_e",  SDLK_e,
    "SDLK_f",  SDLK_f,
    "SDLK_g",  SDLK_g,
    "SDLK_h",  SDLK_h,
    "SDLK_i",  SDLK_i,
    "SDLK_j",  SDLK_j,
    "SDLK_k",  SDLK_k,
    "SDLK_l",  SDLK_l,
    "SDLK_m",  SDLK_m,
    "SDLK_n",  SDLK_n,
    "SDLK_o",  SDLK_o,
    "SDLK_p",  SDLK_p,
    "SDLK_q",  SDLK_q,
    "SDLK_r",  SDLK_r,
    "SDLK_s",  SDLK_s,
    "SDLK_t",  SDLK_t,
    "SDLK_u",  SDLK_u,
    "SDLK_v",  SDLK_v,
    "SDLK_w",  SDLK_w,
    "SDLK_x",  SDLK_x,
    "SDLK_y",  SDLK_y,
    "SDLK_z",  SDLK_z,
    "SDLK_DELETE",  SDLK_DELETE,
    "SDLK_KP0",  SDLK_KP0,
    "SDLK_KP1",  SDLK_KP1,
    "SDLK_KP2",  SDLK_KP2,
    "SDLK_KP3",  SDLK_KP3,
    "SDLK_KP4",  SDLK_KP4,
    "SDLK_KP5",  SDLK_KP5,
    "SDLK_KP6",  SDLK_KP6,
    "SDLK_KP7",  SDLK_KP7,
    "SDLK_KP8",  SDLK_KP8,
    "SDLK_KP9",  SDLK_KP9,
    "SDLK_KP_PERIOD",  SDLK_KP_PERIOD,
    "SDLK_KP_DIVIDE",  SDLK_KP_DIVIDE,
    "SDLK_KP_MULTIPLY",  SDLK_KP_MULTIPLY,
    "SDLK_KP_MINUS",  SDLK_KP_MINUS,
    "SDLK_KP_PLUS",  SDLK_KP_PLUS,
    "SDLK_KP_ENTER",  SDLK_KP_ENTER,
    "SDLK_KP_EQUALS",  SDLK_KP_EQUALS,
    "SDLK_UP",  SDLK_UP,
    "SDLK_DOWN",  SDLK_DOWN,
    "SDLK_RIGHT",  SDLK_RIGHT,
    "SDLK_LEFT",  SDLK_LEFT,
    "SDLK_INSERT",  SDLK_INSERT,
    "SDLK_HOME",  SDLK_HOME,
    "SDLK_END",  SDLK_END,
    "SDLK_PAGEUP",  SDLK_PAGEUP,
    "SDLK_PAGEDOWN",  SDLK_PAGEDOWN,
    "SDLK_F1",  SDLK_F1,
    "SDLK_F2",  SDLK_F2,
    "SDLK_F3",  SDLK_F3,
    "SDLK_F4",  SDLK_F4,
    "SDLK_F5",  SDLK_F5,
    "SDLK_F6",  SDLK_F6,
    "SDLK_F7",  SDLK_F7,
    "SDLK_F8",  SDLK_F8,
    "SDLK_F9",  SDLK_F9,
    "SDLK_F10",  SDLK_F10,
    "SDLK_F11",  SDLK_F11,
    "SDLK_F12",  SDLK_F12,
    "SDLK_F13",  SDLK_F13,
    "SDLK_F14",  SDLK_F14,
    "SDLK_F15",  SDLK_F15,
    "SDLK_NUMLOCK",  SDLK_NUMLOCK,
    "SDLK_CAPSLOCK",  SDLK_CAPSLOCK,
    "SDLK_SCROLLOCK",  SDLK_SCROLLOCK,
    "SDLK_RSHIFT",  SDLK_RSHIFT,
    "SDLK_LSHIFT",  SDLK_LSHIFT,
    "SDLK_RCTRL",  SDLK_RCTRL,
    "SDLK_LCTRL",  SDLK_LCTRL,
    "SDLK_RALT",  SDLK_RALT,
    "SDLK_LALT",  SDLK_LALT,
    "SDLK_RMETA",  SDLK_RMETA,
    "SDLK_LMETA",  SDLK_LMETA,
    "SDLK_LSUPER",  SDLK_LSUPER,
    "SDLK_RSUPER",  SDLK_RSUPER,
    "SDLK_MODE",  SDLK_MODE,
    "SDLK_HELP",  SDLK_HELP,
    "SDLK_PRINT",  SDLK_PRINT,
    "SDLK_SYSREQ",  SDLK_SYSREQ,
    "SDLK_BREAK",  SDLK_BREAK,
    "SDLK_MENU",  SDLK_MENU,
    "SDLK_POWER",  SDLK_POWER,
    "SDLK_EURO",  SDLK_EURO,
    NULL);

  event_mod_flags = gsdl_make_flagstash (&gsdl_kmod_flagstash);

  /* event states */
  event_state_enum = gsdl_define_enum (
    "sdl-event-states",
    "SDL_QUERY",  SDL_QUERY,
    "SDL_IGNORE",  SDL_IGNORE,
    "SDL_DISABLE",  SDL_DISABLE,
    "SDL_ENABLE",  SDL_ENABLE,
    NULL);

#include "sdlevent.x"
}

/* sdlevent.c ends here */
