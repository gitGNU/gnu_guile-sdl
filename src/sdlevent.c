/* sdlevent.c --- SDL input handling for Guile
 *
 * 	Copyright (C) 2003,2004,2005,2006 Thien-Thi Nguyen
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
#include <SDL/SDL_events.h>

#include "config.h"
#include "argcheck.h"
#include "sdlsmobs.h"
#include "sdlenums.h"
#include "wholefns.h"
#include "retval.h"
#include "sym.h"
#include "sym.h"

/* enum/flag types */
SCM event_type_enum;
SCM event_state_enum;
SCM event_keysym_enum;
SCM event_action_enum;

static SCM event_mod_flags;
static SCM event_mask_flags;

GH_DEFPROC (get_event_mod_flags, "flagstash:event-mod", 0, 0, 0,
            (void),
            "Return the flagstash object for event mod flags.")
{
  return event_mod_flags;
}

GH_DEFPROC (get_event_mask_flags, "flagstash:event-mask", 0, 0, 0,
            (void),
            "Return the flagstash object for event mask flags.")
{
  return event_mask_flags;
}


static long event_tag;

#define ASSERT_EVENT(obj,which) \
  ASSERT_SMOB (obj, event_tag, which)

#define UNPACK_EVENT(smob) \
  (SMOBGET (smob, SDL_Event *))

#define RETURN_NEW_EVENT(x) \
  SCM_RETURN_NEWSMOB (event_tag, x)

static
size_t
free_event (SCM event)
{
  free (UNPACK_EVENT (event));
  return sizeof (SDL_Event);
}


static long keysym_tag;

#define ASSERT_KEYSYM(obj,which) \
  ASSERT_SMOB (obj, keysym_tag, which)

#define UNPACK_KEYSYM(smob) \
  (SMOBGET (smob, SDL_keysym *))

#define RETURN_NEW_KEYSYM(x) \
  SCM_RETURN_NEWSMOB (keysym_tag, x)

static
size_t
free_keysym (SCM keysym)
{
  free (UNPACK_KEYSYM (keysym));
  return sizeof (SDL_keysym);
}


/* Constructors */

GH_DEFPROC (make_event, "make-event", 0, 1, 0,
            (SCM type),
            "Return a new SDL event.\n"
            "Optional arg @var{type} is one of the symbols\n"
            "enumerated in the variable @code{event-types}.")
{
#define FUNC_NAME s_make_event
  SDL_Event *event;
  int ctype = SDL_NOEVENT;

  if (BOUNDP (type))
    ctype = GSDL_ENUM2LONG (type, event_type_enum, ARGH1);

  event = (SDL_Event *) scm_must_malloc (sizeof (SDL_Event), FUNC_NAME);
  event->type = ctype;

  RETURN_NEW_EVENT (event);
#undef FUNC_NAME
}

GH_DEFPROC (make_keysym, "make-keysym", 0, 2, 0,
            (SCM sym, SCM mod),
            "Return a new keysym.  Optional args @var{sym} and @var{mod}\n"
            "specify a particular symbol and modifier, respectively.")
{
#define FUNC_NAME s_make_keysym
  SDL_keysym *keysym;

  /* Alloc the keysym.  */
  keysym = (SDL_keysym *) scm_must_malloc (sizeof (SDL_keysym), FUNC_NAME);

  /* Set the sym if given.  */
  UNBOUND_MEANS_FALSE (sym);
  if (NOT_FALSEP (sym))
    {
      ASSERT_EXACT (sym, ARGH1);
      /* keysym->sym = (SDLKey) gh_scm2long (sym); */
      keysym->sym = (SDLKey) GSDL_ENUM2LONG (sym, event_keysym_enum, ARGH1);
    }

  /* Set the mod if given.  */
  if (BOUNDP (mod))
    {
      ASSERT_EXACT (mod, ARGH2);
      keysym->mod = (SDLMod) GSDL_FLAGS2ULONG (mod, event_mod_flags, ARGH2);
    }

  /* Return the new smob.  */
  RETURN_NEW_KEYSYM (keysym);
#undef FUNC_NAME
}


/* Smob getters and setters */


#define ENUM_GETTER(s_frag, c_frag, c_field, etypefrag) \
  GSDL_ENUM_GETTER ("event:" s_frag,                    \
                    event_ ## c_frag,                   \
                    event_tag, SDL_Event *, c_field,    \
                    etypefrag ## _enum)

#define ENUM_SETTER(s_frag, c_frag, c_field, etypefrag) \
  GSDL_ENUM_SETTER ("event:" s_frag,                    \
                    event_ ## c_frag,                   \
                    event_tag, SDL_Event *,             \
                    c_field, etypefrag ## _enum)

#define ENUM_GETSET(get_s, get_c, set_s, set_c, c_field, etypefrag)     \
  ENUM_GETTER (get_s, get_c, c_field, etypefrag)                        \
  ENUM_SETTER (set_s, set_c, c_field, etypefrag)


#define NUMBER_GETTER(s_frag, c_frag, c_field)          \
  GSDL_NUMBER_GETTER ("event:" s_frag,                  \
                      event_ ## c_frag,                 \
                      event_tag, SDL_Event *, c_field)

#define NUMBER_SETTER(s_frag, c_frag, c_field)  \
  GSDL_NUMBER_SETTER ("event:" s_frag,          \
                      event_ ## c_frag,         \
                      event_tag, SDL_Event *,   \
                      c_field, gh_scm2ulong)

#define NUM2_GETTER(a    ,    b)                \
  NUMBER_GETTER   (#a   ":"  #b,                \
                    a ## _ ## b,                \
                    a    .    b)

#define NUM2_SETTER(a      ,      b)            \
  NUMBER_SETTER   (#a   ":set-"  #b "!",        \
                    a ## _set_ ## b,            \
                    a      .      b)

#define NUM2_GETSET(a, b) \
  NUM2_GETTER (a, b)      \
  NUM2_SETTER (a, b)


#define NUM3_GETTER(a    ,    b    ,    c)      \
  NUMBER_GETTER   (#a   ":"  #b   ":"  #c,      \
                    a ## _ ## b ## _ ## c,      \
                    a    .    b    .    c)

#define NUM3_SETTER(a    ,    b      ,      c)          \
  NUMBER_SETTER   (#a   ":"  #b   ":set-"  #c "!",      \
                    a ## _ ## b ## _set_ ## c,          \
                    a    .    b      .      c)

#define NUM3_GETSET(a, b, c) \
  NUM3_GETTER (a, b, c)      \
  NUM3_SETTER (a, b, c)


#define FLAG_GETTER(s_frag, c_frag, c_field, stash)     \
  GSDL_FLAG_GETTER ("event:" s_frag,                    \
                    event_ ## c_frag,                   \
                    event_tag, SDL_Event *,             \
                    c_field, stash)

#define FLAG_SETTER(s_frag, c_frag, c_field, stash)     \
  GSDL_FLAG_SETTER ("event:" s_frag,                    \
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

GH_DEFPROC (pump_events, "pump-events", 0, 0, 0,
            (void),
            "Gather events from input devices and update the event\n"
            "queue.  The return value is unspecified.")
{
#define FUNC_NAME s_pump_events
  SDL_PumpEvents ();
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}

GH_DEFPROC (peep_events, "peep-events", 4, 0, 0,
            (SCM events, SCM numevents, SCM action, SCM mask),
            "Check the event queue for messages and perhaps return some.\n"
            "If @var{action} is @code{SDL_ADDEVENT} (a symbol), add up to\n"
            "@var{numevents} (an integer) events from @var{events} (a list)\n"
            "to the back of the event queue.\n"
            "If it is @code{SDL_PEEKEVENT}, return a count (number less than\n"
            "or equal to @var{numevents}) of events at the front of the event\n"
            "queue that match @var{mask}, without changing the queue.\n"
            "If it is @code{SDL_GETEVENT}, act like for @code{SDL_PEEKEVENT}\n"
            "except return a list of matching events instead of a count,\n"
            "removing them from the queue.\n"
            "[incomplete: missing validation and error handling]")
{
#define FUNC_NAME s_peep_events
  SDL_Event *cevents = NULL;
  int cnumevents, caction, i, ret = -1;
  Uint32 cmask;
  SCM ls = SCM_BOOL_F;

  cnumevents = gh_scm2long (numevents);
  caction = GSDL_ENUM2LONG (action, event_action_enum, ARGH3);

  switch (caction)
    {
    case SDL_ADDEVENT: case SDL_PEEKEVENT: case SDL_GETEVENT: break;
    default: SCM_ASSERT (0, action, ARGH3, FUNC_NAME);
    }

  switch (caction)
    {
    case SDL_ADDEVENT:
      /* Do two passes: first to make sure we have as much as we say we do,
         second to allocate the array and copy the events (ugh).  This will
         most certainly be re-implemented w/ user-visible uniform vectors.  */
      for (i = cnumevents, ls = events;
           i && !gh_null_p (ls);
           i--, ls = gh_cdr (ls));
      SCM_ASSERT (!i, numevents, ARGH2, FUNC_NAME);
      cevents = alloca (cnumevents * sizeof (SDL_Event));
      for (i = 0, ls = events;
           i < cnumevents;
           i++, ls = gh_cdr (ls))
        cevents[i] = *(UNPACK_EVENT (gh_car (ls)));
      ret = SDL_PeepEvents (cevents, cnumevents, caction, 0);
      break;

    case SDL_GETEVENT:
      cevents = alloca (cnumevents * sizeof (SDL_Event));
      /* fallthrough */

    case SDL_PEEKEVENT:
      cmask = gsdl_flags2ulong (mask, event_mask_flags, ARGH4, FUNC_NAME);
      ret = SDL_PeepEvents (cevents, cnumevents, caction, cmask);
      if (0 > ret)
        scm_misc_error (FUNC_NAME, "badness", SCM_EOL);
      if (cevents)
        {
          SDL_Event *cev; SCM ev;

          ls = SCM_EOL;
          for (i = ret - 1; -1 < i; i--)
            {
              cev = (SDL_Event *) scm_must_malloc (sizeof (SDL_Event), FUNC_NAME);
              *cev = cevents[i];
              SCM_NEWSMOB (ev, event_tag, cev);
              ls = gh_cons (ev, ls);
            }
        }
      break;
    }

  switch (caction)
    {
    case SDL_ADDEVENT:
    case SDL_PEEKEVENT:
      RETURN_INT (ret);
    case SDL_GETEVENT:
      return ls;
    default:
      RETURN_UNSPECIFIED;
    }
#undef FUNC_NAME
}

GH_DEFPROC (poll_event, "poll-event", 0, 1, 0,
            (SCM event),
            "Poll for events and return #t if there are any pending.\n"
            "Optional arg @var{event} specifies an event object (from\n"
            "@code{make-event}) to be filled in with the next event from\n"
            "the queue (if available).")
{
#define FUNC_NAME s_poll_event
  int result;

  if (UNBOUNDP (event))
    /* No args.  */
    result = SDL_PollEvent (NULL);
  else
    {
      /* We're given an event smob - fill it.  */
      ASSERT_EVENT (event, ARGH1);
      result = SDL_PollEvent (UNPACK_EVENT (event));
    }

  RETURN_BOOL
    (result);
#undef FUNC_NAME
}

GH_DEFPROC (wait_event, "wait-event", 0, 1, 0,
            (SCM event),
            "Wait indefinitely for and return #f only if there were errors.\n"
            "Optional arg @var{event} specifies an event object (from\n"
            "@code{make-event}) to be filled in with the next event from\n"
            "the queue.")
{
#define FUNC_NAME s_wait_event
  int result;

  if (UNBOUNDP (event))
    /* No args.  */
    result = SDL_WaitEvent (NULL);
  else
    {
      /* We're given an event smob - fill it.  */
      ASSERT_EVENT (event, ARGH1);
      result = SDL_WaitEvent (UNPACK_EVENT (event));
    }

  RETURN_BOOL
    (result);
#undef FUNC_NAME
}

GH_DEFPROC (push_event, "push-event", 1, 0, 0,
            (SCM event),
            "Push @var{event} onto the queue.  Return 1 for success,\n"
            "0 if the queue was full, -1 for other errors.")
{
#define FUNC_NAME s_push_event
  int result;

  ASSERT_EVENT (event, ARGH1);

  result = SDL_PushEvent (UNPACK_EVENT (event));
  RETURN_INT (result);
#undef FUNC_NAME
}

GH_DEFPROC (set_event_filter, "set-event-filter", 1, 0, 0,
            (SCM filter),
            "[not yet implemented]")
{
#define FUNC_NAME s_set_event_filter
  THROW_NOT_YET_IMPLEMENTED;
  /* extern DECLSPEC void SDL_SetEventFilter (SDL_EventFilter filter); */
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}

GH_DEFPROC (get_event_filter, "get-event-filter", 1, 0, 0,
            (SCM filter),
            "[not yet implemented]")
{
#define FUNC_NAME s_get_event_filter
  THROW_NOT_YET_IMPLEMENTED;
  /* extern DECLSPEC SDL_EventFilter SDL_GetEventFilter (void); */
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}

GH_DEFPROC (event_state, "event-state", 2, 0, 0,
            (SCM type, SCM state),
            "Query or set event @var{type} to @var{state}.\n"
            "@var{type} should be one elements from @code{event-types},\n"
            "and likewise @var{state} from @code{event-states}.\n"
            "If @var{state} is @code{SDL_QUERY}, return the current\n"
            "processing state of the specified event.")
{
#define FUNC_NAME s_event_state
  int ctype, cstate, ret;

  ctype = GSDL_ENUM2LONG (type, event_type_enum, ARGH1);
  cstate = GSDL_ENUM2LONG (state, event_state_enum, ARGH2);

  ret = SDL_EventState (ctype, cstate);
  if (SDL_QUERY == cstate)
    return gsdl_long2enum (ret, event_state_enum);
  else
    RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


GH_DEFPROC (enable_unicode, "enable-unicode", 0, 1, 0,
            (SCM enable_p),
            "Return #t iff UNICODE keyboard translation is enabled.\n"
            "Optional arg @var{enable?} if non-#f, enables UNICODE\n"
            "keyboard translation, or disables it if #f.")
{
#define FUNC_NAME s_enable_unicode
  RETURN_BOOL
    (SDL_EnableUNICODE (UNBOUNDP (enable_p)
                        ? -1
                        : gh_scm2bool (enable_p)));
#undef FUNC_NAME
}

/*
 * If 'delay' is set to 0, keyboard repeat is disabled.
 */

GH_DEFPROC (enable_key_repeat, "enable-key-repeat", 2, 0, 0,
            (SCM delay, SCM interval),
            "Enable or disable keyboard repeat.\n"
            "@var{delay} is the initial delay in ms between the time\n"
            "when a key is pressed, and keyboard repeat begins.\n"
            "@var{interval} is the time in ms between keyboard repeat\n"
            "events.  If @var{delay} is 0, keyboard repeat is disabled.\n"
            "Return #t on success.")
{
#define FUNC_NAME s_enable_key_repeat
  int cinterval, cdelay;

  ASSERT_EXACT (delay, ARGH1);
  ASSERT_EXACT (interval, ARGH2);

  cdelay    = gh_scm2long (delay);
  cinterval = gh_scm2long (interval);

  RETURN_TRUE_IF_0 (SDL_EnableKeyRepeat (cdelay, cinterval));
#undef FUNC_NAME
}

GH_DEFPROC (get_key_state, "get-key-state", 0, 0, 0,
            (),
            "Return a list of pressed keys (SDLK_* symbols).")
{
#define FUNC_NAME s_get_key_state
  Uint8 *keystate;
  int count, i;
  SCM ls = SCM_EOL;

  keystate = SDL_GetKeyState (&count);

  for (i = 0; i < count; i++)
    if (keystate[i])
      ls = gh_cons (gsdl_long2enum (i, event_keysym_enum), ls);

  return ls;
#undef FUNC_NAME
}

GH_DEFPROC (get_mod_state, "get-mod-state", 0, 0, 0,
            (void),
            "Return the current key modifier state as a list of symbols.")
{
#define FUNC_NAME s_get_mod_state
  return gsdl_ulong2flags (SDL_GetModState (), event_mod_flags);
#undef FUNC_NAME
}

GH_DEFPROC (set_mod_state, "set-mod-state", 1, 0, 0,
            (SCM modstate),
            "Set the current key modifier state to @var{modstate},\n"
            "a list of symbols.  This does not change the keyboard state,\n"
            "only the key modifier flags.  The return value is unspecified.")
{
#define FUNC_NAME s_set_mod_state
  ASSERT_EXACT (modstate, ARGH1);
  SDL_SetModState (GSDL_FLAGS2ULONG (modstate, event_mod_flags, ARGH1));
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}

DECLARE_SIMPLE_SYM (state);
DECLARE_SIMPLE_SYM (x);
DECLARE_SIMPLE_SYM (y);

GH_DEFPROC (get_mouse_state, "get-mouse-state", 0, 0, 0,
            (void),
            "Return the current state of the mouse as an alist with\n"
            "symbolic keys: @code{state}, @code{x} and @code{y}.")
{
#define FUNC_NAME s_get_mouse_state
  int buttons, x, y;
  buttons = SDL_GetMouseState (&x, &y);
  RETURN_LIST3 (gh_cons (SYM (state), gh_long2scm (buttons)),
                gh_cons (SYM (x), gh_long2scm (x)),
                gh_cons (SYM (y), gh_long2scm (y)));
#undef FUNC_NAME
}

GH_DEFPROC (get_relative_mouse_state, "get-mouse-relative-state", 0, 0, 0,
            (void),
            "Return the current relative state of the mouse as an alist\n"
            "symbolic keys: @code{state}, @code{x} and @code{y}.")
{
#define FUNC_NAME s_get_relative_mouse_state
  int buttons, x, y;
  buttons = SDL_GetRelativeMouseState (&x, &y);
  RETURN_LIST3 (gh_cons (SYM (state), gh_long2scm (buttons)),
                gh_cons (SYM (x), gh_long2scm (x)),
                gh_cons (SYM (y), gh_long2scm (y)));
#undef FUNC_NAME
}

GH_DEFPROC (button_p, "button?", 1, 0, 0,
            (SCM mask),
            "Return #t if buttons specified in @var{mask} (an integer)\n"
            "are pressed.  Use 1 for left, 2 for middle and 4 for right,\n"
            "combined with @code{logior}, to form @var{mask}.  For example,\n"
            "a value of 5 specifies both left and right buttons.")
{
#define FUNC_NAME s_button_p
  ASSERT_EXACT (mask, ARGH1);
  RETURN_BOOL
    (SDL_BUTTON (gh_scm2long (mask)));
#undef FUNC_NAME
}


extern flagstash_t gsdl_kmod_flagstash;
extern flagstash_t gsdl_evmask_flagstash;

/* Initialize glue.  */
void
gsdl_init_event (void)
{
  event_tag = scm_make_smob_type ("SDL-Event", sizeof (SDL_Event));
  scm_set_smob_free (event_tag, free_event);

  keysym_tag = scm_make_smob_type ("SDL-Keysym", sizeof (SDL_keysym));
  scm_set_smob_free (keysym_tag, free_keysym);

  /* event type constants */
  event_type_enum = gsdl_define_enum
    ("event-types",
     GSDL_CSCS (SDL_ACTIVEEVENT),
     GSDL_CSCS (SDL_KEYDOWN),
     GSDL_CSCS (SDL_KEYUP),
     GSDL_CSCS (SDL_MOUSEMOTION),
     GSDL_CSCS (SDL_MOUSEBUTTONDOWN),
     GSDL_CSCS (SDL_MOUSEBUTTONUP),
     GSDL_CSCS (SDL_JOYAXISMOTION),
     GSDL_CSCS (SDL_JOYBALLMOTION),
     GSDL_CSCS (SDL_JOYHATMOTION),
     GSDL_CSCS (SDL_JOYBUTTONDOWN),
     GSDL_CSCS (SDL_JOYBUTTONUP),
     GSDL_CSCS (SDL_QUIT),
     GSDL_CSCS (SDL_SYSWMEVENT),
     GSDL_CSCS (SDL_VIDEORESIZE),
     GSDL_CSCS (SDL_USEREVENT),
     NULL);

  /* keysyms */
  event_keysym_enum = gsdl_define_enum
    ("event-keys",
     GSDL_CSCS (SDLK_BACKSPACE),
     GSDL_CSCS (SDLK_TAB),
     GSDL_CSCS (SDLK_CLEAR),
     GSDL_CSCS (SDLK_RETURN),
     GSDL_CSCS (SDLK_PAUSE),
     GSDL_CSCS (SDLK_ESCAPE),
     GSDL_CSCS (SDLK_SPACE),
     GSDL_CSCS (SDLK_EXCLAIM),
     GSDL_CSCS (SDLK_QUOTEDBL),
     GSDL_CSCS (SDLK_HASH),
     GSDL_CSCS (SDLK_DOLLAR),
     GSDL_CSCS (SDLK_AMPERSAND),
     GSDL_CSCS (SDLK_QUOTE),
     GSDL_CSCS (SDLK_LEFTPAREN),
     GSDL_CSCS (SDLK_RIGHTPAREN),
     GSDL_CSCS (SDLK_ASTERISK),
     GSDL_CSCS (SDLK_PLUS),
     GSDL_CSCS (SDLK_COMMA),
     GSDL_CSCS (SDLK_MINUS),
     GSDL_CSCS (SDLK_PERIOD),
     GSDL_CSCS (SDLK_SLASH),
     GSDL_CSCS (SDLK_0),
     GSDL_CSCS (SDLK_1),
     GSDL_CSCS (SDLK_2),
     GSDL_CSCS (SDLK_3),
     GSDL_CSCS (SDLK_4),
     GSDL_CSCS (SDLK_5),
     GSDL_CSCS (SDLK_6),
     GSDL_CSCS (SDLK_7),
     GSDL_CSCS (SDLK_8),
     GSDL_CSCS (SDLK_9),
     GSDL_CSCS (SDLK_COLON),
     GSDL_CSCS (SDLK_SEMICOLON),
     GSDL_CSCS (SDLK_LESS),
     GSDL_CSCS (SDLK_EQUALS),
     GSDL_CSCS (SDLK_GREATER),
     GSDL_CSCS (SDLK_QUESTION),
     GSDL_CSCS (SDLK_AT),
     GSDL_CSCS (SDLK_LEFTBRACKET),
     GSDL_CSCS (SDLK_BACKSLASH),
     GSDL_CSCS (SDLK_RIGHTBRACKET),
     GSDL_CSCS (SDLK_CARET),
     GSDL_CSCS (SDLK_UNDERSCORE),
     GSDL_CSCS (SDLK_BACKQUOTE),
     GSDL_CSCS (SDLK_a),
     GSDL_CSCS (SDLK_b),
     GSDL_CSCS (SDLK_c),
     GSDL_CSCS (SDLK_d),
     GSDL_CSCS (SDLK_e),
     GSDL_CSCS (SDLK_f),
     GSDL_CSCS (SDLK_g),
     GSDL_CSCS (SDLK_h),
     GSDL_CSCS (SDLK_i),
     GSDL_CSCS (SDLK_j),
     GSDL_CSCS (SDLK_k),
     GSDL_CSCS (SDLK_l),
     GSDL_CSCS (SDLK_m),
     GSDL_CSCS (SDLK_n),
     GSDL_CSCS (SDLK_o),
     GSDL_CSCS (SDLK_p),
     GSDL_CSCS (SDLK_q),
     GSDL_CSCS (SDLK_r),
     GSDL_CSCS (SDLK_s),
     GSDL_CSCS (SDLK_t),
     GSDL_CSCS (SDLK_u),
     GSDL_CSCS (SDLK_v),
     GSDL_CSCS (SDLK_w),
     GSDL_CSCS (SDLK_x),
     GSDL_CSCS (SDLK_y),
     GSDL_CSCS (SDLK_z),
     GSDL_CSCS (SDLK_DELETE),
     GSDL_CSCS (SDLK_KP0),
     GSDL_CSCS (SDLK_KP1),
     GSDL_CSCS (SDLK_KP2),
     GSDL_CSCS (SDLK_KP3),
     GSDL_CSCS (SDLK_KP4),
     GSDL_CSCS (SDLK_KP5),
     GSDL_CSCS (SDLK_KP6),
     GSDL_CSCS (SDLK_KP7),
     GSDL_CSCS (SDLK_KP8),
     GSDL_CSCS (SDLK_KP9),
     GSDL_CSCS (SDLK_KP_PERIOD),
     GSDL_CSCS (SDLK_KP_DIVIDE),
     GSDL_CSCS (SDLK_KP_MULTIPLY),
     GSDL_CSCS (SDLK_KP_MINUS),
     GSDL_CSCS (SDLK_KP_PLUS),
     GSDL_CSCS (SDLK_KP_ENTER),
     GSDL_CSCS (SDLK_KP_EQUALS),
     GSDL_CSCS (SDLK_UP),
     GSDL_CSCS (SDLK_DOWN),
     GSDL_CSCS (SDLK_RIGHT),
     GSDL_CSCS (SDLK_LEFT),
     GSDL_CSCS (SDLK_INSERT),
     GSDL_CSCS (SDLK_HOME),
     GSDL_CSCS (SDLK_END),
     GSDL_CSCS (SDLK_PAGEUP),
     GSDL_CSCS (SDLK_PAGEDOWN),
     GSDL_CSCS (SDLK_F1),
     GSDL_CSCS (SDLK_F2),
     GSDL_CSCS (SDLK_F3),
     GSDL_CSCS (SDLK_F4),
     GSDL_CSCS (SDLK_F5),
     GSDL_CSCS (SDLK_F6),
     GSDL_CSCS (SDLK_F7),
     GSDL_CSCS (SDLK_F8),
     GSDL_CSCS (SDLK_F9),
     GSDL_CSCS (SDLK_F10),
     GSDL_CSCS (SDLK_F11),
     GSDL_CSCS (SDLK_F12),
     GSDL_CSCS (SDLK_F13),
     GSDL_CSCS (SDLK_F14),
     GSDL_CSCS (SDLK_F15),
     GSDL_CSCS (SDLK_NUMLOCK),
     GSDL_CSCS (SDLK_CAPSLOCK),
     GSDL_CSCS (SDLK_SCROLLOCK),
     GSDL_CSCS (SDLK_RSHIFT),
     GSDL_CSCS (SDLK_LSHIFT),
     GSDL_CSCS (SDLK_RCTRL),
     GSDL_CSCS (SDLK_LCTRL),
     GSDL_CSCS (SDLK_RALT),
     GSDL_CSCS (SDLK_LALT),
     GSDL_CSCS (SDLK_RMETA),
     GSDL_CSCS (SDLK_LMETA),
     GSDL_CSCS (SDLK_LSUPER),
     GSDL_CSCS (SDLK_RSUPER),
     GSDL_CSCS (SDLK_MODE),
     GSDL_CSCS (SDLK_HELP),
     GSDL_CSCS (SDLK_PRINT),
     GSDL_CSCS (SDLK_SYSREQ),
     GSDL_CSCS (SDLK_BREAK),
     GSDL_CSCS (SDLK_MENU),
     GSDL_CSCS (SDLK_POWER),
     GSDL_CSCS (SDLK_EURO),
     NULL);

  event_action_enum = gsdl_define_enum
    ("event-actions",
     GSDL_CSCS (SDL_ADDEVENT),
     GSDL_CSCS (SDL_PEEKEVENT),
     GSDL_CSCS (SDL_GETEVENT),
     NULL);

  event_mod_flags = gsdl_make_flagstash (&gsdl_kmod_flagstash);
  event_mask_flags = gsdl_make_flagstash (&gsdl_evmask_flagstash);

  /* event states */
  event_state_enum = gsdl_define_enum
    ("event-states",
     GSDL_CSCS (SDL_QUERY),
     GSDL_CSCS (SDL_IGNORE),
     /* SDL_DISABLE is not mentioned in the associated comment in SDL_events.h
        (SDL 1.2), and moreover, its value is the same as SDL_IGNORE, so we
        tickle the irony bone a bit and don't include it in Guile-SDL.  */
     /* GSDL_CSCS (SDL_DISABLE), */
     GSDL_CSCS (SDL_ENABLE),
     NULL);

#include "sdlevent.x"
}

/* sdlevent.c ends here */
