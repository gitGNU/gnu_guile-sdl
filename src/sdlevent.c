/* sdlevent.c --- SDL input handling for Guile
 *
 * Copyright (C) 2003, 2004, 2005, 2006, 2007,
 *   2009, 2011, 2013 Thien-Thi Nguyen
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
#include <SDL/SDL_events.h>

/* enum/flag types */
static SCM event_type_enum;
static valaka_t event_type_eback[] = {
  VALAKA (SDL_ACTIVEEVENT),
  VALAKA (SDL_KEYDOWN),
  VALAKA (SDL_KEYUP),
  VALAKA (SDL_MOUSEMOTION),
  VALAKA (SDL_MOUSEBUTTONDOWN),
  VALAKA (SDL_MOUSEBUTTONUP),
  VALAKA (SDL_JOYAXISMOTION),
  VALAKA (SDL_JOYBALLMOTION),
  VALAKA (SDL_JOYHATMOTION),
  VALAKA (SDL_JOYBUTTONDOWN),
  VALAKA (SDL_JOYBUTTONUP),
  VALAKA (SDL_QUIT),
  VALAKA (SDL_SYSWMEVENT),
  VALAKA (SDL_VIDEORESIZE),
  VALAKA (SDL_USEREVENT)
};

static SCM event_state_enum;
static valaka_t event_state_eback[] = {
  VALAKA (SDL_QUERY),
  VALAKA (SDL_IGNORE),
  /* SDL_DISABLE is not mentioned in the associated comment in SDL_events.h
     (SDL 1.2), and moreover, its value is the same as SDL_IGNORE, so we
     tickle the irony bone a bit and don't include it in Guile-SDL.  */
  /* VALAKA (SDL_DISABLE), */
  VALAKA (SDL_ENABLE)
};

static SCM event_keysym_enum;
static valaka_t event_keysym_eback[] = {
  VALAKA (SDLK_BACKSPACE),
  VALAKA (SDLK_TAB),
  VALAKA (SDLK_CLEAR),
  VALAKA (SDLK_RETURN),
  VALAKA (SDLK_PAUSE),
  VALAKA (SDLK_ESCAPE),
  VALAKA (SDLK_SPACE),
  VALAKA (SDLK_EXCLAIM),
  VALAKA (SDLK_QUOTEDBL),
  VALAKA (SDLK_HASH),
  VALAKA (SDLK_DOLLAR),
  VALAKA (SDLK_AMPERSAND),
  VALAKA (SDLK_QUOTE),
  VALAKA (SDLK_LEFTPAREN),
  VALAKA (SDLK_RIGHTPAREN),
  VALAKA (SDLK_ASTERISK),
  VALAKA (SDLK_PLUS),
  VALAKA (SDLK_COMMA),
  VALAKA (SDLK_MINUS),
  VALAKA (SDLK_PERIOD),
  VALAKA (SDLK_SLASH),
  VALAKA (SDLK_0),
  VALAKA (SDLK_1),
  VALAKA (SDLK_2),
  VALAKA (SDLK_3),
  VALAKA (SDLK_4),
  VALAKA (SDLK_5),
  VALAKA (SDLK_6),
  VALAKA (SDLK_7),
  VALAKA (SDLK_8),
  VALAKA (SDLK_9),
  VALAKA (SDLK_COLON),
  VALAKA (SDLK_SEMICOLON),
  VALAKA (SDLK_LESS),
  VALAKA (SDLK_EQUALS),
  VALAKA (SDLK_GREATER),
  VALAKA (SDLK_QUESTION),
  VALAKA (SDLK_AT),
  VALAKA (SDLK_LEFTBRACKET),
  VALAKA (SDLK_BACKSLASH),
  VALAKA (SDLK_RIGHTBRACKET),
  VALAKA (SDLK_CARET),
  VALAKA (SDLK_UNDERSCORE),
  VALAKA (SDLK_BACKQUOTE),
  VALAKA (SDLK_a),
  VALAKA (SDLK_b),
  VALAKA (SDLK_c),
  VALAKA (SDLK_d),
  VALAKA (SDLK_e),
  VALAKA (SDLK_f),
  VALAKA (SDLK_g),
  VALAKA (SDLK_h),
  VALAKA (SDLK_i),
  VALAKA (SDLK_j),
  VALAKA (SDLK_k),
  VALAKA (SDLK_l),
  VALAKA (SDLK_m),
  VALAKA (SDLK_n),
  VALAKA (SDLK_o),
  VALAKA (SDLK_p),
  VALAKA (SDLK_q),
  VALAKA (SDLK_r),
  VALAKA (SDLK_s),
  VALAKA (SDLK_t),
  VALAKA (SDLK_u),
  VALAKA (SDLK_v),
  VALAKA (SDLK_w),
  VALAKA (SDLK_x),
  VALAKA (SDLK_y),
  VALAKA (SDLK_z),
  VALAKA (SDLK_DELETE),
  VALAKA (SDLK_KP0),
  VALAKA (SDLK_KP1),
  VALAKA (SDLK_KP2),
  VALAKA (SDLK_KP3),
  VALAKA (SDLK_KP4),
  VALAKA (SDLK_KP5),
  VALAKA (SDLK_KP6),
  VALAKA (SDLK_KP7),
  VALAKA (SDLK_KP8),
  VALAKA (SDLK_KP9),
  VALAKA (SDLK_KP_PERIOD),
  VALAKA (SDLK_KP_DIVIDE),
  VALAKA (SDLK_KP_MULTIPLY),
  VALAKA (SDLK_KP_MINUS),
  VALAKA (SDLK_KP_PLUS),
  VALAKA (SDLK_KP_ENTER),
  VALAKA (SDLK_KP_EQUALS),
  VALAKA (SDLK_UP),
  VALAKA (SDLK_DOWN),
  VALAKA (SDLK_RIGHT),
  VALAKA (SDLK_LEFT),
  VALAKA (SDLK_INSERT),
  VALAKA (SDLK_HOME),
  VALAKA (SDLK_END),
  VALAKA (SDLK_PAGEUP),
  VALAKA (SDLK_PAGEDOWN),
  VALAKA (SDLK_F1),
  VALAKA (SDLK_F2),
  VALAKA (SDLK_F3),
  VALAKA (SDLK_F4),
  VALAKA (SDLK_F5),
  VALAKA (SDLK_F6),
  VALAKA (SDLK_F7),
  VALAKA (SDLK_F8),
  VALAKA (SDLK_F9),
  VALAKA (SDLK_F10),
  VALAKA (SDLK_F11),
  VALAKA (SDLK_F12),
  VALAKA (SDLK_F13),
  VALAKA (SDLK_F14),
  VALAKA (SDLK_F15),
  VALAKA (SDLK_NUMLOCK),
  VALAKA (SDLK_CAPSLOCK),
  VALAKA (SDLK_SCROLLOCK),
  VALAKA (SDLK_RSHIFT),
  VALAKA (SDLK_LSHIFT),
  VALAKA (SDLK_RCTRL),
  VALAKA (SDLK_LCTRL),
  VALAKA (SDLK_RALT),
  VALAKA (SDLK_LALT),
  VALAKA (SDLK_RMETA),
  VALAKA (SDLK_LMETA),
  VALAKA (SDLK_LSUPER),
  VALAKA (SDLK_RSUPER),
  VALAKA (SDLK_MODE),
  VALAKA (SDLK_HELP),
  VALAKA (SDLK_PRINT),
  VALAKA (SDLK_SYSREQ),
  VALAKA (SDLK_BREAK),
  VALAKA (SDLK_MENU),
  VALAKA (SDLK_POWER),
  VALAKA (SDLK_EURO)
};

static SCM event_action_enum;
static valaka_t event_action_eback[] = {
  VALAKA (SDL_ADDEVENT),
  VALAKA (SDL_PEEKEVENT),
  VALAKA (SDL_GETEVENT)
};

static SCM event_mb_flags;
static SCM event_mod_flags;
static SCM event_mask_flags;

#define ALLOCA_EVENTS(count)  alloca (count * sizeof (SDL_Event))

PRIMPROC
(get_event_mod_flags, "flagstash:event-mod", 0, 0, 0,
 (void),
 doc: /***********
Return the flagstash object for event mod flags.
@xref{Enums and Constants}.  */)
{
  return event_mod_flags;
}

PRIMPROC
(get_event_mask_flags, "flagstash:event-mask", 0, 0, 0,
 (void),
 doc: /***********
Return the flagstash object for event mask flags.
@xref{Enums and Constants}.  */)
{
  return event_mask_flags;
}


static long event_tag;

#define event_nick "SDL-Event"

#define EVENT_P(obj)  SCM_SMOB_PREDICATE (event_tag, obj)

#define ASSERT_EVENT(obj,which) \
  ASSERT_SMOB (obj, event, which)

#define UNPACK_EVENT(smob) \
  (SMOBGET (smob, SDL_Event *))

#define NEW_EVENT_X(scm,ptr) \
  SCM_NEWSMOB (scm, event_tag, ptr)

#define RETURN_NEW_EVENT(x) \
  NEWSMOB_OR_FALSE (event_tag, x)

static
size_t
free_event (SCM event)
{
  SDL_Event *cevent = UNPACK_EVENT (event);

  GCFREE (cevent, event_nick);
  return GCRV (cevent);
}


static long keysym_tag;

#define keysym_nick "SDL-Keysym"

#define ASSERT_KEYSYM(obj,which) \
  ASSERT_SMOB (obj, keysym, which)

#define UNPACK_KEYSYM(smob) \
  (SMOBGET (smob, SDL_keysym *))

#define RETURN_NEW_KEYSYM(x) \
  NEWSMOB_OR_FALSE (keysym_tag, x)

static
size_t
free_keysym (SCM keysym)
{
  SDL_keysym *ckeysym = UNPACK_KEYSYM (keysym);

  GCFREE (ckeysym, keysym_nick);
  return GCRV (ckeysym);
}


/* Constructors */

#define GCMALLOC_EVENT()  GCMALLOC (sizeof (SDL_Event), event_nick)

PRIMPROC
(make_event, "make-event", 0, 1, 0,
 (SCM type),
 doc: /***********
Return a new SDL event.
Optional arg @var{type} is one of the symbols
enumerated in the variable @code{event-types}.
If omitted, the default is @code{SDL_NOEVENT}.
@xref{Enums and Constants}.  */)
{
#define FUNC_NAME s_make_event
  SDL_Event *event;
  int ctype = SDL_NOEVENT;

  if (BOUNDP (type))
    ctype = GSDL_ENUM2LONG (type, event_type_enum, 1);

  if ((event = GCMALLOC_EVENT ()))
    event->type = ctype;

  RETURN_NEW_EVENT (event);
#undef FUNC_NAME
}

PRIMPROC
(make_keysym, "make-keysym", 0, 2, 0,
 (SCM sym, SCM mod),
 doc: /***********
Return a new keysym.  Optional args @var{sym} and @var{mod}
specify one of the @code{event-keys} (@pxref{Enums and Constants})
and any modifiers (from @code{flasgstash:event-mod}), respectively.  */)
{
#define FUNC_NAME s_make_keysym
  SDL_keysym *keysym;

  if ((keysym = GCMALLOC (sizeof (SDL_keysym), keysym_nick)))
    {
      /* Set the sym if given.  */
      UNBOUND_MEANS_FALSE (sym);
      if (NOT_FALSEP (sym))
        {
          ASSERT_INTEGER (sym, 1);
          /* keysym->sym = (SDLKey) C_LONG (sym); */
          keysym->sym = (SDLKey) GSDL_ENUM2LONG (sym, event_keysym_enum, 1);
        }

      /* Set the mod if given.  */
      if (BOUNDP (mod))
        {
          ASSERT_INTEGER (mod, 2);
          keysym->mod = (SDLMod) GSDL_FLAGS2ULONG (mod, event_mod_flags, 2);
        }
    }

  /* Return the new smob.  */
  RETURN_NEW_KEYSYM (keysym);
#undef FUNC_NAME
}


/* Smob getters and setters */


#define ENUM_GETTER(s_frag, c_frag, c_field, etypefrag) \
  GSDL_ENUM_GETTER ("event:" s_frag,                    \
                    event_ ## c_frag,                   \
                    event, SDL_Event *, c_field,        \
                    etypefrag ## _enum)

#define ENUM_SETTER(s_frag, c_frag, c_field, etypefrag) \
  GSDL_ENUM_SETTER ("event:" s_frag,                    \
                    event_ ## c_frag,                   \
                    event, SDL_Event *,                 \
                    c_field, etypefrag ## _enum)

#define ENUM_GETSET(get_s, get_c, set_s, set_c, c_field, etypefrag)     \
  ENUM_GETTER (get_s, get_c, c_field, etypefrag)                        \
  ENUM_SETTER (set_s, set_c, c_field, etypefrag)


#define NUMBER_GETTER(s_frag, c_frag, c_field)          \
  GSDL_NUMBER_GETTER ("event:" s_frag,                  \
                      event_ ## c_frag,                 \
                      event, Event, c_field)

#define SNUMBER_SETTER(s_frag, c_frag, c_field) \
  GSDL_NUMBER_SETTER ("event:" s_frag,          \
                      event_ ## c_frag,         \
                      event, Event,             \
                      c_field, C_LONG)

#define UNUMBER_SETTER(s_frag, c_frag, c_field) \
  GSDL_NUMBER_SETTER ("event:" s_frag,          \
                      event_ ## c_frag,         \
                      event, Event,             \
                      c_field, C_ULONG)

#define NUM2_GETTER(a    ,    b)                \
  NUMBER_GETTER   (#a   ":"  #b,                \
                    a ## _ ## b,                \
                        a.b)

#define SNUM2_SETTER(a      ,      b)           \
  SNUMBER_SETTER   (#a   ":set-"  #b "!",       \
                     a ## _set_ ## b,           \
                           a.b)

#define UNUM2_SETTER(a      ,      b)           \
  UNUMBER_SETTER   (#a   ":set-"  #b "!",       \
                     a ## _set_ ## b,           \
                           a.b)

#define SNUM2_GETSET(a, b) \
  NUM2_GETTER (a, b)      \
  SNUM2_SETTER (a, b)

#define UNUM2_GETSET(a, b) \
  NUM2_GETTER (a, b)      \
  UNUM2_SETTER (a, b)


#define NUM3_GETTER(a    ,    b    ,    c)      \
  NUMBER_GETTER   (#a   ":"  #b   ":"  #c,      \
                    a ## _ ## b ## _ ## c,      \
                            a.b.c)

#define SNUM3_SETTER(a    ,    b      ,      c)         \
  SNUMBER_SETTER   (#a   ":"  #b   ":set-"  #c "!",     \
                    a ## _ ## b ## _set_ ## c,          \
                            a.b.c)

#define UNUM3_SETTER(a    ,    b      ,      c)         \
  UNUMBER_SETTER   (#a   ":"  #b   ":set-"  #c "!",     \
                    a ## _ ## b ## _set_ ## c,          \
                            a.b.c)

#define SNUM3_GETSET(a, b, c) \
  NUM3_GETTER (a, b, c)       \
  SNUM3_SETTER (a, b, c)

#define UNUM3_GETSET(a, b, c) \
  NUM3_GETTER (a, b, c)       \
  UNUM3_SETTER (a, b, c)


#define FLAG_GETTER(s_frag, c_frag, c_field, stash)     \
  GSDL_FLAG_GETTER ("event:" s_frag,                    \
                    event_ ## c_frag,                   \
                    event, SDL_Event *,                 \
                    c_field, c_field, stash)

#define FLAG_SETTER(s_frag, c_frag, c_field, stash)     \
  GSDL_FLAG_SETTER ("event:" s_frag,                    \
                    event_ ## c_frag,                   \
                    event, SDL_Event *,                 \
                    c_field, c_field, stash)

#define FLAG_GETSET(get_s, get_c, set_s, set_c, c_field, stash) \
  FLAG_GETTER (get_s, get_c, c_field, stash)                    \
  FLAG_SETTER (set_s, set_c, c_field, stash)



ENUM_GETSET ("type",          type,
             "set-type!", set_type,
             type,
             event_type)

UNUM2_GETSET (active, gain)
UNUM2_GETSET (active, state)
UNUM2_GETSET (key, state)

ENUM_GETSET ("key:keysym:sym",        key_keysym_sym,
             "key:keysym:set-sym!", key_keysym_set_sym,
             key.keysym.sym,
             event_keysym)

FLAG_GETSET ("key:keysym:mod",        key_keysym_mod,
             "key:keysym:set-mod!", key_keysym_set_mod,
             key.keysym.mod,
             event_mod_flags)

UNUM3_GETSET (key, keysym, scancode)
UNUM3_GETSET (key, keysym, unicode)

FLAG_GETSET ("motion:state",        motion_state,
             "motion:set-state!", motion_set_state,
             motion.state,
             event_mb_flags)
UNUM2_GETSET (motion, x)
UNUM2_GETSET (motion, y)
SNUM2_GETSET (motion, xrel)
SNUM2_GETSET (motion, yrel)

UNUM2_GETSET (button, button)
UNUM2_GETSET (button, state)
UNUM2_GETSET (button, x)
UNUM2_GETSET (button, y)

UNUM2_GETSET (jaxis, which)
UNUM2_GETSET (jaxis, axis)
SNUM2_GETSET (jaxis, value)

UNUM2_GETSET (jbutton, which)
UNUM2_GETSET (jbutton, button)
UNUM2_GETSET (jbutton, state)

UNUM2_GETSET (jball, which)
UNUM2_GETSET (jball, ball)
SNUM2_GETSET (jball, xrel)
SNUM2_GETSET (jball, yrel)

UNUM2_GETSET (jhat, which)
UNUM2_GETSET (jhat, hat)
UNUM2_GETSET (jhat, value)

SNUM2_GETSET (resize, w)
SNUM2_GETSET (resize, h)

#if 0 /* what is this? --ttn */
NUM2_GETSET (user, code)
NUM2_GETSET (user, data1)
NUM2_GETSET (user, data2)
#endif /* 0 */


/* SDL event functions */

PRIMPROC
(pump_events, "pump-events", 0, 0, 0,
 (void),
 doc: /***********
Gather events from input devices and update the event queue.  */)
{
#define FUNC_NAME s_pump_events
  SDL_PumpEvents ();
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}

PRIMPROC
(peep_events, "peep-events", 4, 0, 0,
 (SCM events, SCM numevents, SCM action, SCM mask),
 doc: /***********
Manage the event queue, depending on @var{action}:

@table @code
@item SDL_ADDEVENT
Add up to @var{numevents} (an integer) events from
@var{events} (a list) to the back of the event queue.

@item SDL_PEEKEVENT
Return a count (number less than or equal to @var{numevents})
of events at the front of the event queue that match @var{mask}
(see @code{flagstash:event-mask}), without changing the queue.

@item SDL_GETEVENT
Act like for @code{SDL_PEEKEVENT} except return a list of
matching events instead of a count, removing them from the queue.
@end table  */)
{
#define FUNC_NAME s_peep_events
  SDL_Event *cevents = NULL;
  SDL_eventaction caction;
  int cnumevents, i, ret = -1;
  Uint32 cmask;
  SCM ls = SCM_BOOL_F;

  ASSERT_LONG_COPY (numevents, 2);
  caction = GSDL_ENUM2LONG (action, event_action_enum, 3);

  switch (caction)
    {
    case SDL_ADDEVENT:
      /* Do two passes: first to validate argument types and consistency,
         second to allocate the array and copy the events (ugh).  This will
         most certainly be re-implemented w/ user-visible uniform vectors.  */
      SCM_VALIDATE_LIST_COPYLEN (1, events, i);
      if (cnumevents > i)
        SCM_MISC_ERROR ("numevents greater than events length",
                        SCM_EOL);
      for (i = 0, ls = events;
           i < cnumevents;
           i++, ls = CDR (ls))
        if (! EVENT_P (CAR (ls)))
          break;
      ASSERT_TYPE (i == cnumevents, events, 1, "list of SDL-Event");
      cevents = ALLOCA_EVENTS (cnumevents);
      for (i = 0, ls = events;
           i < cnumevents;
           i++, ls = CDR (ls))
        cevents[i] = *(UNPACK_EVENT (CAR (ls)));
      ret = SDL_PeepEvents (cevents, cnumevents, caction, 0);
      break;

    case SDL_GETEVENT:
      cevents = ALLOCA_EVENTS (cnumevents);
      /* fallthrough */

    case SDL_PEEKEVENT:
      cmask = GSDL_FLAGS2ULONG (mask, event_mask_flags, 4);
      ret = SDL_PeepEvents (cevents, cnumevents, caction, cmask);
      if (0 > ret)
        SCM_MISC_ERROR ("badness", SCM_EOL);
      if (cevents)
        {
          SDL_Event *cev; SCM ev;

          ls = SCM_EOL;
          for (i = ret - 1; -1 < i; i--)
            {
              cev = GCMALLOC_EVENT ();
              *cev = cevents[i];
              NEW_EVENT_X (ev, cev);
              ls = CONS (ev, ls);
            }
        }
      break;

    default:
      SCM_MISC_ERROR ("bad action", SCM_EOL);
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

PRIMPROC
(poll_event, "poll-event", 0, 1, 0,
 (SCM event),
 doc: /***********
Poll for events and return @code{#t} if there are any pending.
Optional arg @var{event} specifies an event object (from
@code{make-event}) to be filled in with the next event from
the queue (if available).  */)
{
#define FUNC_NAME s_poll_event
  int result;

  if (UNBOUNDP (event))
    /* No args.  */
    result = SDL_PollEvent (NULL);
  else
    {
      /* We're given an event smob - fill it.  */
      ASSERT_EVENT (event, 1);
      result = SDL_PollEvent (UNPACK_EVENT (event));
    }

  RETURN_BOOL
    (result);
#undef FUNC_NAME
}

PRIMPROC
(wait_event, "wait-event", 0, 1, 0,
 (SCM event),
 doc: /***********
Wait indefinitely for and return @code{#f} only if there were errors.
Optional arg @var{event} specifies an event object (from
@code{make-event}) to be filled in with the next event from
the queue.  */)
{
#define FUNC_NAME s_wait_event
  int result;

  if (UNBOUNDP (event))
    /* No args.  */
    result = SDL_WaitEvent (NULL);
  else
    {
      /* We're given an event smob - fill it.  */
      ASSERT_EVENT (event, 1);
      result = SDL_WaitEvent (UNPACK_EVENT (event));
    }

  RETURN_BOOL
    (result);
#undef FUNC_NAME
}

PRIMPROC
(push_event, "push-event", 1, 0, 0,
 (SCM event),
 doc: /***********
Push @var{event} onto the queue.  Return 1 for success,
0 if the queue was full, -1 for other errors.  */)
{
#define FUNC_NAME s_push_event
  int result;

  ASSERT_EVENT (event, 1);

  result = SDL_PushEvent (UNPACK_EVENT (event));
  RETURN_INT (result);
#undef FUNC_NAME
}

struct event_filter_info {
  enum {
    EF_TYPE_ONLY,
    EF_FULL_EVENT
  }                type;
  SCM              proc;
};

static struct event_filter_info efi;

static int
the_event_filter (const SDL_Event *event)
{
  SCM arg = BOOL_FALSE;

  switch (efi.type)
    {
    case EF_TYPE_ONLY:
      arg = btw->long2enum (event->type, event_type_enum);
      break;
    case EF_FULL_EVENT:
      {
        SDL_Event *copy;

        copy = GCMALLOC_EVENT ();
        *copy = *event;
        NEW_EVENT_X (arg, copy);
      }
      break;
    }
  return NOT_FALSEP (CALL1 (efi.proc, arg));
}

PRIMPROC
(set_event_filter, "set-event-filter", 2, 0, 0,
 (SCM filter, SCM fullp),
 doc: /***********
Set the event filter to @var{filter}, or clear it if @var{filter}
is @code{#f}.  This is a procedure called with one arg, and whose
return value, if non-@code{#f}, means to keep the event, otherwise
discard it.  If @var{full?} is @code{#f}, the arg the event type (a
symbol), otherwise it is an event object.

-args: (2 0 0 filter full?)  */)
{
#define FUNC_NAME s_set_event_filter
  if (NOT_FALSEP (filter))
    SCM_VALIDATE_PROC (1, filter);

  efi.proc = filter;
  efi.type = EXACTLY_FALSEP (fullp)
    ? EF_TYPE_ONLY
    : EF_FULL_EVENT;

  SDL_SetEventFilter (EXACTLY_FALSEP (filter)
                      ? NULL
                      : the_event_filter);
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}

PRIMPROC
(get_event_filter, "get-event-filter", 0, 0, 0,
 (void),
 doc: /***********
Return information on the current event filter, or @code{#f}
if none is set.  If there is a filter, the value is a pair
with car the filter proc, and cdr @code{#f} if the proc takes
an event type, or @code{#t} if the proc takes an event object.  */)
{
#define FUNC_NAME s_get_event_filter
  return EXACTLY_FALSEP (efi.proc)
    ? BOOL_FALSE
    : CONS (efi.proc, (EF_FULL_EVENT == efi.type
                       ? BOOL_TRUE
                       : BOOL_FALSE));
#undef FUNC_NAME
}

PRIMPROC
(event_state, "event-state", 2, 0, 0,
 (SCM type, SCM state),
 doc: /***********
Query or set event @var{type} to @var{state}.
@var{type} should be one elements from @code{event-types},
and likewise @var{state} from @code{event-states}.
@xref{Enums and Constants}.
If @var{state} is @code{SDL_QUERY}, return the current
processing state of the specified event.  */)
{
#define FUNC_NAME s_event_state
  int ctype, cstate, ret;

  ctype = GSDL_ENUM2LONG (type, event_type_enum, 1);
  cstate = GSDL_ENUM2LONG (state, event_state_enum, 2);

  ret = SDL_EventState (ctype, cstate);
  if (SDL_QUERY == cstate)
    return btw->long2enum (ret, event_state_enum);
  else
    RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


PRIMPROC
(enable_unicode, "enable-unicode", 0, 1, 0,
 (SCM enable_p),
 doc: /***********
Return @code{#t} iff UNICODE keyboard translation is enabled.
Optional arg @var{enable?} if non-@code{#f}, enables UNICODE
keyboard translation, or disables it if @code{#f}.  */)
{
#define FUNC_NAME s_enable_unicode
  RETURN_BOOL
    (SDL_EnableUNICODE (UNBOUNDP (enable_p)
                        ? -1
                        : C_BOOL (enable_p)));
#undef FUNC_NAME
}

/*
 * If 'delay' is set to 0, keyboard repeat is disabled.
 */

PRIMPROC
(enable_key_repeat, "enable-key-repeat", 2, 0, 0,
 (SCM delay, SCM interval),
 doc: /***********
Enable or disable keyboard repeat.
@var{delay} is the initial delay in ms between the time
when a key is pressed, and keyboard repeat begins.
@var{interval} is the time in ms between keyboard repeat
events.  If @var{delay} is 0, keyboard repeat is disabled.
Return @code{#t} on success.  */)
{
#define FUNC_NAME s_enable_key_repeat
  int cinterval, cdelay;

  ASSERT_LONG_COPY (delay, 1);
  ASSERT_LONG_COPY (interval, 2);

  RETURN_TRUE_IF_0 (SDL_EnableKeyRepeat (cdelay, cinterval));
#undef FUNC_NAME
}

PRIMPROC
(get_key_state, "get-key-state", 0, 0, 0,
 (),
 doc: /***********
Return a list of pressed keys (SDLK_* symbols).  */)
{
#define FUNC_NAME s_get_key_state
  Uint8 *keystate;
  int count, i;
  SCM ls = SCM_EOL;

  keystate = SDL_GetKeyState (&count);

  for (i = 0; i < count; i++)
    if (keystate[i])
      ls = CONS (btw->long2enum (i, event_keysym_enum), ls);

  return ls;
#undef FUNC_NAME
}

PRIMPROC
(get_mod_state, "get-mod-state", 0, 0, 0,
 (void),
 doc: /***********
Return the current key modifier state as a list of symbols.  */)
{
#define FUNC_NAME s_get_mod_state
  return btw->ulong2flags (SDL_GetModState (), event_mod_flags);
#undef FUNC_NAME
}

PRIMPROC
(set_mod_state, "set-mod-state", 1, 0, 0,
 (SCM modstate),
 doc: /***********
Set the current key modifier state to @var{modstate},
a list of symbols.  This does not change the keyboard state,
only the key modifier flags.  */)
{
#define FUNC_NAME s_set_mod_state
  ASSERT_INTEGER (modstate, 1);
  SDL_SetModState (GSDL_FLAGS2ULONG (modstate, event_mod_flags, 1));
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}

typedef Uint8 SDLCALL (*getmouse_sdl_fn) (int *x, int *y);

DECLARE_SIMPLE_SYM (state);
DECLARE_SIMPLE_SYM (x);
DECLARE_SIMPLE_SYM (y);

static SCM
getmouse (const char *FUNC_NAME, SCM symbolic, getmouse_sdl_fn fn)
{
  unsigned long buttons;
  int x, y;

  UNBOUND_MEANS_FALSE (symbolic);

  buttons = fn (&x, &y);
  RETURN_LIST3
    (CONS (SYM (state),
           EXACTLY_FALSEP (symbolic)
           ? NUM_ULONG (buttons)
           : (buttons
              ? btw->ulong2flags (buttons, event_mb_flags)
              : SCM_EOL)),
     CONS (SYM (x), NUM_LONG (x)),
     CONS (SYM (y), NUM_LONG (y)));
}

PRIMPROC
(get_mouse_state, "get-mouse-state", 0, 1, 0,
 (SCM symbolic),
 doc: /***********
Return the current state of the mouse as an alist with
symbolic keys: @code{state}, @code{x} and @code{y}.
Normally, all values are integers.
However, if optional arg @var{symbolic} is non-@code{#f},
the @code{state} value is instead a (possibly empty)
list of symbols of the set:

@example
left middle right
wheel-up wheel-down
x1 x2
@end example  */)
{
  return getmouse (s_get_mouse_state,
                   symbolic,
                   SDL_GetMouseState);
}

PRIMPROC
(get_relative_mouse_state, "get-mouse-relative-state", 0, 1, 0,
 (SCM symbolic),
 doc: /***********
Return the current relative state of the mouse as an alist
with symbolic keys: @code{state}, @code{x} and @code{y}.
Optional arg @var{symbolic} has the same effect as
for @code{get-mouse-state}.  */)
{
  return getmouse (s_get_relative_mouse_state,
                   symbolic,
                   SDL_GetRelativeMouseState);
}

PRIMPROC
(button_p, "button?", 1, 0, 0,
 (SCM mask),
 doc: /***********
Return @code{#t} if buttons specified in @var{mask} are pressed,
otherwise @code{#f}.
@var{mask} is a symbol or a list of symbols from the set returned
by @code{get-mouse-state}.

For backward compatability, @var{mask} can also be the (integer)
logior of the buttons, using mapping:

@example
 1  left
 2  middle
 4  right
 8  wheel-up
16  wheel-down
32  x1
64  x2
@end example

For example, a value of 5 specifies both left and right buttons,
equivalent to @code{(left right)}.  */)
{
#define FUNC_NAME s_button_p
  unsigned long cmask, buttons;

  if (INTEGERP (mask))
    ASSERT_ULONG_COPY (mask, 1);
  else
    cmask = GSDL_FLAGS2ULONG (mask, event_mb_flags, 1);

  if (! cmask)
    RETURN_FALSE;

  buttons = SDL_GetMouseState (NULL, NULL);
  RETURN_BOOL (cmask == (cmask & buttons));
#undef FUNC_NAME
}


#include "mb.c"
#include "kmod.c"
#include "evmask.c"

/* Initialize glue.  */
void
gsdl_init_event (void)
{
  DEFSMOB (event_tag, event_nick,
           NULL,
           free_event,
           /* TODO: print_event */ NULL);

  DEFSMOB (keysym_tag, keysym_nick,
           NULL,
           free_keysym,
           /* TODO: print_keysym */ NULL);

  /* event type constants */
  event_type_enum = DEFINE_ENUM ("event-types", event_type_eback);

  /* keysyms */
  event_keysym_enum = DEFINE_ENUM ("event-keys", event_keysym_eback);
  event_action_enum = DEFINE_ENUM ("event-actions", event_action_eback);
  event_mb_flags = btw->make_flagstash (&mb_flagstash);
  event_mod_flags = btw->make_flagstash (&kmod_flagstash);
  event_mask_flags = btw->make_flagstash (&evmask_flagstash);

  /* event states */
  event_state_enum = DEFINE_ENUM ("event-states", event_state_eback);

  efi.proc = SCM_BOOL_F;

#include "sdlevent.x"
}

/* sdlevent.c ends here */
