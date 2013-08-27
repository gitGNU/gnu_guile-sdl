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
#include "SDL_events.h"
#include "SDL_active.h"
#include "b-values.h"

/* enum/flag types */

static SCM active_enum;
#include "k/active.c"

static SCM updn_enum;
#include "k/updn.c"

static SCM mbut_enum;
#include "k/mbut.c"

static SCM event_type_enum;
#include "k/evtype.c"

static SCM event_state_enum;
#include "k/evstate.c"

static SCM event_keysym_enum;
#include "k/keysym.c"

static SCM event_action_enum;
#include "k/evaction.c"

static SCM event_mb_flags;
static SCM event_mod_flags;
static SCM event_mask_flags;
static SCM event_jhpos_flags;
static SCM appstate_flags;

#define ALLOCA_EVENTS(count)  alloca (count * sizeof (SDL_Event))


static smob_tag_t event_tag;

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


/* Constructors */

#define GCMALLOC_EVENT()  GCMALLOC (sizeof (SDL_Event), event_nick)

PRIMPROC
(make_event, "make-event", 0, 1, 0,
 (SCM type),
 doc: /***********
Return a new SDL event.
Optional arg @var{type} is a symbol (@pxref{event-type enums}).
If omitted, the default is @code{SDL_NOEVENT}.  */)
{
#define FUNC_NAME s_make_event
  DECLINIT_SYM2NUM_CC (1, event_type_enum);
  SDL_Event *event;
  int ctype = SDL_NOEVENT;

  if (BOUNDP (type))
    ctype = ENUM2LONG (1, type);

  if ((event = GCMALLOC_EVENT ()))
    event->type = ctype;

  RETURN_NEW_EVENT (event);
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

ENUM_GETSET ("active:gain",        active_gain,
             "active:set-gain!", active_set_gain,
             active.gain,
             active)
FLAG_GETSET ("active:state",        active_state,
             "active:set-state!", active_set_state,
             active.state,
             appstate_flags)

ENUM_GETSET ("key:state",       key_state,
             "key:set-state!", key_set_state,
             key.state,
             updn)

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

ENUM_GETSET ("button:button",        button_button,
             "button:set-button!", button_set_button,
             button.button,
             mbut)
ENUM_GETSET ("button:state",         button_state,
             "button:set-state!", button_set_state,
             button.state,
             updn)
UNUM2_GETSET (button, x)
UNUM2_GETSET (button, y)

UNUM2_GETSET (jaxis, which)
UNUM2_GETSET (jaxis, axis)
SNUM2_GETSET (jaxis, value)

UNUM2_GETSET (jbutton, which)
UNUM2_GETSET (jbutton, button)
ENUM_GETSET ("jbutton:state",        jbutton_state,
             "jbutton:set-state!", jbutton_set_state,
             jbutton.state,
             updn)

UNUM2_GETSET (jball, which)
UNUM2_GETSET (jball, ball)
SNUM2_GETSET (jball, xrel)
SNUM2_GETSET (jball, yrel)

UNUM2_GETSET (jhat, which)
UNUM2_GETSET (jhat, hat)
FLAG_GETSET ("jhat:value",        jhat_value,
             "jhat:set-value!", jhat_set_value,
             jhat.value,
             event_jhpos_flags)

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
(evqueue_add, "evqueue-add", 0, 0, 1,
 (SCM events),
 doc: /***********
Add @code{events} to the back of the event queue.
Return the count of succesfully added events.  */)
{
#define FUNC_NAME s_evqueue_add
  SDL_Event *cevents = NULL;
  int i, count;
  SCM ls, head;

  /* Pass 1: Validate argument types and consistency.  */
  SCM_VALIDATE_LIST_COPYLEN (1, events, count);
  for (i = 0, ls = events;
       i < count;
       i++, ls = CDR (ls))
    {
      head = CAR (ls);
      ASSERT_EVENT (head, 1 + i);
    }

  /* Pass 2: Allocate the array and copy the events (ugh).  */
  if (! (cevents = ALLOCA_EVENTS (count)))
    abort ();
  for (i = 0, ls = events;
       i < count;
       i++, ls = CDR (ls))
    cevents[i] = *(UNPACK_EVENT (CAR (ls)));

  RETURN_INT
    (SDL_PeepEvents (cevents, count, SDL_ADDEVENT, 0));
#undef FUNC_NAME
}

struct evqueue_do_details {
  const char            *who;
  const SDL_eventaction  act;
  const bool             acc;
};

static SCM
evqueue_do (const struct evqueue_do_details *dd, SCM n, SCM mask)
{
  const char *FUNC_NAME = dd->who;
  DECLINIT_SYM2NUM_CC (2, event_mask_flags);
  SDL_Event *got;
  unsigned long cn, cmask;
  int count;

  ASSERT_ULONG_COPY (n, 1);
  cmask = FLAGS2ULONG (2, mask);

  if (! (got = ALLOCA_EVENTS (cn))
      || 0 > (count = SDL_PeepEvents (got, cn, dd->act, cmask)))
    return BOOL_FALSE;

  if (! dd->acc)
    RETURN_INT (count);
  else
    {
      SCM ls = SCM_EOL;

      while (count--)
        {
          SDL_Event *cev = GCMALLOC_EVENT ();
          SCM ev;

          *cev = got[count];
          NEW_EVENT_X (ev, cev);
          ls = CONS (ev, ls);
        }
      return ls;
    }
}

PRIMPROC
(evqueue_peek, "evqueue-peek", 2, 1, 0,
 (SCM n, SCM mask, SCM accumulate),
 doc: /***********
Return a count (less than or equal to @var{n}) of events at
the front of the event queue that match @var{mask},
without changing the queue.  Optional arg @var{accumulate} if
non-@code{#f} means to return the list of matched events, instead.
If there are errors, return @code{#f}.

@xref{event-mask flags}.  */)
{
  const struct evqueue_do_details dd =
    {
      .who = s_evqueue_peek,
      .act = SDL_PEEKEVENT,
      .acc = BOUNDP (accumulate) && NOT_FALSEP (accumulate)
    };

  return evqueue_do (&dd, n, mask);
}

PRIMPROC
(evqueue_get, "evqueue-get", 2, 0, 0,
 (SCM n, SCM mask),
 doc: /***********
Return a list (of length at most @var{n}) of
events at the front of the event queue that match
@var{mask}, removing them from the queue.
If there are errors, return @code{#f}.

@xref{event-mask flags}.  */)
{
  const struct evqueue_do_details dd =
    {
      .who = s_evqueue_get,
      .act = SDL_GETEVENT,
      .acc = true
    };

  return evqueue_do (&dd, n, mask);
}

PRIMPROC
(peep_events, "peep-events", 4, 0, 0,
 (SCM events, SCM numevents, SCM action, SCM mask),
 doc: /***********
NB: This procedure is obsoleted by @code{evqueue-add},
@code{evqueue-peek} and @code{evqueue-get};
it @strong{will be removed} after 2013-12-31.

Manage the event queue, depending
on @var{action} (@pxref{event-action enums}):

@table @code
@item SDL_ADDEVENT
Add up to @var{numevents} (an integer) events from
@var{events} (a list) to the back of the event queue.

@item SDL_PEEKEVENT
Return a count (number less than or equal to @var{numevents})
of events at the front of the event queue that match @var{mask}
(@pxref{event-mask flags}), without changing the queue.

@item SDL_GETEVENT
Act like for @code{SDL_PEEKEVENT} except return a list of
matching events instead of a count, removing them from the queue.
@end table  */)
{
#define FUNC_NAME s_peep_events
  DECLINIT_SYM2NUM_CC (3, event_action_enum);
  DECLINIT_SYM2NUM_CC (4, event_mask_flags);
  SDL_Event *cevents = NULL;
  SDL_eventaction caction;
  int cnumevents, i, ret = -1;
  Uint32 cmask;
  SCM ls = SCM_BOOL_F;

  ASSERT_LONG_COPY (numevents, 2);
  caction = ENUM2LONG (3, action);

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
      cmask = FLAGS2ULONG (4, mask);
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
Push @var{event} onto the queue.
Return @code{#t} on success.  */)
{
#define FUNC_NAME s_push_event
  ASSERT_EVENT (event, 1);

  RETURN_TRUE_IF_0
    (SDL_PushEvent (UNPACK_EVENT (event)));
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
(event_type_handling, "event-type-handling", 1, 1, 0,
 (SCM type, SCM setting),
 doc: /***********
Return @code{#t} if event @var{type} (@pxref{event-type enums})
is recognized and queued, or @code{#f} if it is ignored.
If @var{setting} is specified, set the handling of
@var{type} to the truth value of @var{setting} first.  */)
{
#define FUNC_NAME s_event_type_handling
  DECLINIT_SYM2NUM_CC   (1, event_type_enum);
  int ctype = ENUM2LONG (1, type);

  return BOOLEAN (SDL_ENABLE == SDL_EventState
                  (ctype, CSTATE_FROM_SETTING (setting)));
#undef FUNC_NAME
}

PRIMPROC
(event_state, "event-state", 2, 0, 0,
 (SCM type, SCM state),
 doc: /***********
NB: This procedure is obsoleted by @code{event-type-handling}
and @strong{will be removed} after 2013-12-31.

Set or query the state of event @var{type} (@pxref{event-type enums})
processing, based on @var{state} (@pxref{event-state enums}).
If @var{state} is @code{SDL_QUERY}, return the current state.
If it is @code{SDL_IGNORE} or @code{SDL_ENABLE},
disable or enable, respectively, internal event @var{type}
processing and return @var{state}.  */)
{
#define FUNC_NAME s_event_state
  DECLINIT_SYM2NUM_CC   (1, event_type_enum);
  int ctype = ENUM2LONG (1, type);
  DECLINIT_SYM2NUM_CC    (2, event_state_enum);
  int cstate = ENUM2LONG (2, state);
  int ret = SDL_EventState (ctype, cstate);

  return btw->long2enum (ret, event_state_enum);
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
Return a list of pressed keys (@pxref{keysym enums}).  */)
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
  DECLINIT_SYM2NUM_CC          (1, event_mod_flags);
  SDL_SetModState (FLAGS2ULONG (1, modstate));
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
  return LIST3
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
NB: This procedure is obsoleted by @code{mouse-bxy}
and @strong{will be removed} after 2013-12-31.

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
NB: This procedure is obsoleted by @code{mouse-bxy}
and @strong{will be removed} after 2013-12-31.

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
    {
      DECLINIT_SYM2NUM_CC (1, event_mb_flags);
      cmask = FLAGS2ULONG (1, mask);
    }

  if (! cmask)
    return BOOL_FALSE;

  buttons = SDL_GetMouseState (NULL, NULL);
  RETURN_BOOL (cmask == (cmask & buttons));
#undef FUNC_NAME
}

PRIMPROC
(mouse_bxy, "mouse-bxy", 0, 1, 0,
 (SCM relative),
 doc: /***********
Return three values: a (possibly empty) list of symbols
representing pressed mouse buttons (like @code{event:button:button}),
and two integer coordinates @var{x} and @var{y}.

Optional arg @code{relative} non-@code{#f} means the
coordinates are relative to the last time the underlying
@code{SDL_GetRelativeMouseState} was called.  */)
{
#define FUNC_NAME s_mouse_bxy
  getmouse_sdl_fn fn;
  int buttons, x, y;

  UNBOUND_MEANS_FALSE (relative);
  fn = EXACTLY_FALSEP (relative)
    ? SDL_GetMouseState
    : SDL_GetRelativeMouseState;

  buttons = fn (&x, &y);
  RETURN_VALUES3
    (btw->ulong2flags (buttons, event_mb_flags),
     NUM_INT (x),
     NUM_INT (y));
#undef FUNC_NAME
}

PRIMPROC
(get_app_state, "get-app-state", 0, 0, 0,
 (void),
 doc: /***********
Return the current state of the application, a list of symbols.
The list may include: `mousefocus', `inputfocus', `active'.  */)
{
  return btw->ulong2flags (SDL_GetAppState (), appstate_flags);
}


#include "k/mb.c"
#include "k/kmod.c"
#include "k/jhpos.c"
#include "k/evmask.c"
#include "k/appstate.c"

/* Initialize glue.  */
void
gsdl_init_event (void)
{
  DEFSMOB (event_tag, event_nick,
           NULL,
           free_event,
           /* TODO: print_event */ NULL);

  /* constants */
  {
    kp_init_t allp[] = {
      { &event_type_enum, &evtype_kp },
      { &event_keysym_enum, &keysym_kp },
      { &event_action_enum, &evaction_kp },
      { &event_state_enum, &evstate_kp },
      { &mbut_enum, &mbut_kp },
      { &updn_enum, &updn_kp },
      { &active_enum, &active_kp }
    };

    REGISTER_KP_V (allp);
  }
  {
    kf_init_t allf[] = {
      { &event_mb_flags, &mb_flagstash },
      { &event_mod_flags, &kmod_flagstash },
      { &event_mask_flags, &evmask_flagstash },
      { &event_jhpos_flags, &jhpos_flagstash },
      { &appstate_flags, &appstate_flagstash }
    };

    REGISTER_KF_V (allf);
  }

  btw->event_state_enum = event_state_enum;
  btw->updn_enum = updn_enum;

  efi.proc = SCM_BOOL_F;

#include "sdlevent.x"
}

/* sdlevent.c ends here */
