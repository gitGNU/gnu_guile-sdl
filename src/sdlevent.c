/*******************************************************************
 *  event.c -- SDL input handling for Guile                        *
 *                                                                 *
 *  Created:    <2001-05-27 13:58:16 foof>                         *
 *  Time-stamp: <2001-06-30 01:19:05 foof>                         *
 *  Author:     Alex Shinn <foof@debian.org>                       *
 *                                                                 *
 *  Copyright (C) 2001 Alex Shinn                                  *
 *                                                                 *
 *  This program is free software; you can redistribute it and/or  *
 * modify it under the terms of the GNU General Public License as  *
 * published by the Free Software Foundation; either version 2 of  *
 * the License, or (at your option) any later version.             *
 *                                                                 *
 * This program is distributed in the hope that it will be useful, *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of  *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the   *
 * GNU General Public License for more details.                    *
 *                                                                 *
 * You should have received a copy of the GNU General Public       *
 * License along with this program; if not, write to the Free      *
 * Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,  *
 * MA 02111-1307 USA                                               *
 ******************************************************************/

/* event headers */
#include "sdlsmobs.h"
#include "sdlenums.h"
#include "sdlevent.h"

/* tags for SDL smobs */
long event_tag;
long keysym_tag;

SCM event_type_enum;
SCM event_keysym_enum;

scm_sizet
free_event (SCM event)
{
   free ((SDL_Event*) SCM_SMOB_DATA (event));
   return sizeof (SDL_Event);
}

/* constructors */
SCM make_event (SCM s_type)
{
   SDL_Event *event;
   int type=SDL_NOEVENT;

   if (s_type != SCM_UNDEFINED) {
      SCM_ASSERT (scm_exact_p (s_type), s_type, SCM_ARG1, "sdl-make-event");
      type = scm_num2long (s_type, SCM_ARG1, "scm_num2long");
   }

   event = (SDL_Event *) scm_must_malloc (sizeof (SDL_Event), "sdl-make-event");
   event->type = type;

   SCM_RETURN_NEWSMOB (event_tag, event);
}

SCM make_keysym (SCM sym, SCM mod)
{
   SDL_keysym *keysym;

   /* alloc the keysym */
   keysym = (SDL_keysym *) scm_must_malloc (sizeof (SDL_keysym),
                                            "sdl-make-keysym");

   /* set the sym if given */
   if (sym != SCM_UNDEFINED) {
      SCM_ASSERT (scm_exact_p (sym), sym, SCM_ARG1, "sdl-make-keysym");
      keysym->sym = (SDLKey) scm_num2long (sym, SCM_ARG1, "scm_num2long");
   }

   /* set the mod if given */
   if (mod != SCM_UNDEFINED) {
      SCM_ASSERT (scm_exact_p (mod), mod, SCM_ARG2, "sdl-make-keysym");
      keysym->mod = (SDLMod) scm_num2long (mod, SCM_ARG1, "scm_num2long");
   }

   /* return the new smob */
   SCM_RETURN_NEWSMOB (keysym_tag, keysym);
}

/* smob getters */

/* SCM_DEFINE_NUMBER_GETTER ("event:type", event_type, event_tag,  */
/*                         SDL_Event*, type)  */

SCM event_type (SCM event)
{
   SCM t;
   SCM_ASSERT_SMOB (event, event_tag, SCM_ARG1, "sdl-event:type");
   t = scm_long2num (((SDL_Event*) SCM_CDR (event))->type);
   return scm_number_to_enum (event_type_enum, t);
}


SCM_DEFINE_NUMBER_GETTER ("sdl-event:active:gain", event_active_gain, event_tag,
                        SDL_Event*, active.gain)
SCM_DEFINE_NUMBER_GETTER ("sdl-event:active:state", event_active_state, event_tag,
                        SDL_Event*, active.state)

SCM_DEFINE_NUMBER_GETTER ("sdl-event:key:state", event_key_state, event_tag,
                        SDL_Event*, key.state)

/* SCM_DEFINE_NUMBER_GETTER ("event:key:keysym:sym", event_key_keysym_sym,  */
/*                         event_tag, SDL_Event*, key.keysym.sym)  */
SCM event_key_keysym_sym (SCM s_event)
{
   SCM sym;
   SDL_Event* event;

   SCM_ASSERT_SMOB (s_event, event_tag, SCM_ARG1, "sdl-event:key:keysym:sym");
   event = (SDL_Event*) SCM_CDR (s_event);
   sym = scm_long2num (event->key.keysym.sym);
   return scm_number_to_enum (event_keysym_enum, sym);
}

SCM_DEFINE_NUMBER_GETTER ("sdl-event:key:keysym:mod", event_key_keysym_mod,
                        event_tag, SDL_Event*, key.keysym.mod)
SCM_DEFINE_NUMBER_GETTER ("sdl-event:key:keysym:scancode", event_key_keysym_scancode,
                        event_tag, SDL_Event*, key.keysym.scancode)
SCM_DEFINE_NUMBER_GETTER ("sdl-event:key:keysym:unicode", event_key_keysym_unicode,
                        event_tag, SDL_Event*, key.keysym.unicode)

SCM_DEFINE_NUMBER_GETTER ("sdl-event:motion:state", event_motion_state, event_tag,
                        SDL_Event*, motion.state)
SCM_DEFINE_NUMBER_GETTER ("sdl-event:motion:x", event_motion_x, event_tag,
                        SDL_Event*, motion.x)
SCM_DEFINE_NUMBER_GETTER ("sdl-event:motion:y", event_motion_y, event_tag,
                        SDL_Event*, motion.y)
SCM_DEFINE_NUMBER_GETTER ("sdl-event:motion:xrel", event_motion_xrel, event_tag,
                        SDL_Event*, motion.xrel)
SCM_DEFINE_NUMBER_GETTER ("sdl-event:motion:yrel", event_motion_yrel, event_tag,
                        SDL_Event*, motion.yrel)

SCM_DEFINE_NUMBER_GETTER ("sdl-event:button:button", event_button_button, event_tag,
                        SDL_Event*, button.button)
SCM_DEFINE_NUMBER_GETTER ("sdl-event:button:state", event_button_state, event_tag,
                        SDL_Event*, button.state)
SCM_DEFINE_NUMBER_GETTER ("sdl-event:button:x", event_button_x, event_tag,
                        SDL_Event*, button.x)
SCM_DEFINE_NUMBER_GETTER ("sdl-event:button:y", event_button_y, event_tag,
                        SDL_Event*, button.y)

SCM_DEFINE_NUMBER_GETTER ("sdl-event:jaxis:which", event_jaxis_which, event_tag,
                        SDL_Event*, jaxis.which)
SCM_DEFINE_NUMBER_GETTER ("sdl-event:jaxis:axis", event_jaxis_axis, event_tag,
                        SDL_Event*, jaxis.axis)
SCM_DEFINE_NUMBER_GETTER ("sdl-event:jaxis:value", event_jaxis_value, event_tag,
                        SDL_Event*, jaxis.value)

SCM_DEFINE_NUMBER_GETTER ("sdl-event:jbutton:which", event_jbutton_which, event_tag,
                        SDL_Event*, jbutton.which)
SCM_DEFINE_NUMBER_GETTER ("sdl-event:jbutton:button", event_jbutton_button, event_tag,
                        SDL_Event*, jbutton.button)
SCM_DEFINE_NUMBER_GETTER ("sdl-event:jbutton:state", event_jbutton_state, event_tag,
                        SDL_Event*, jbutton.state)

SCM_DEFINE_NUMBER_GETTER ("sdl-event:jball:which", event_jball_which, event_tag,
                        SDL_Event*, jball.which)
SCM_DEFINE_NUMBER_GETTER ("sdl-event:jball:ball", event_jball_ball, event_tag,
                        SDL_Event*, jball.ball)
SCM_DEFINE_NUMBER_GETTER ("sdl-event:jball:xrel", event_jball_xrel, event_tag,
                        SDL_Event*, jball.xrel)
SCM_DEFINE_NUMBER_GETTER ("sdl-event:jball:yrel", event_jball_yrel, event_tag,
                        SDL_Event*, jball.yrel)

SCM_DEFINE_NUMBER_GETTER ("sdl-event:jhat:which", event_jhat_which, event_tag,
                        SDL_Event*, jhat.which)
SCM_DEFINE_NUMBER_GETTER ("sdl-event:jhat:hat", event_jhat_hat, event_tag,
                        SDL_Event*, jhat.hat)
SCM_DEFINE_NUMBER_GETTER ("sdl-event:jhat:value", event_jhat_state, event_tag,
                        SDL_Event*, jhat.value)

SCM_DEFINE_NUMBER_GETTER ("sdl-event:resize:w", event_resize_w, event_tag,
                        SDL_Event*, resize.w)
SCM_DEFINE_NUMBER_GETTER ("sdl-event:resize:h", event_resize_h, event_tag,
                        SDL_Event*, resize.h)

/* SCM_DEFINE_NUMBER_GETTER ("sdl-event:user:code", event_user_code, event_tag, */
/*                         SDL_Event*, user.code) */
/* SCM_DEFINE_NUMBER_GETTER ("event:user:data1", event_user_data1, event_tag,  */
/*                         SDL_Event*, user.data1)  */
/* SCM_DEFINE_NUMBER_GETTER ("event:user:data2", event_user_data2, event_tag,  */
/*                         SDL_Event*, user.data2)  */

/* SCM_DEFINE_NUMBER_GETTER ("keysym:scancode", keysym_scancode, keysym_tag,  */
/*                         SDL_keysym*, scancode)  */
/* SCM_DEFINE_NUMBER_GETTER ("keysym:sym", keysym_sym, keysym_tag,  */
/*                         SDL_keysym*, sym)  */
/* SCM_DEFINE_NUMBER_GETTER ("keysym:mod", keysym_mod, keysym_tag,  */
/*                         SDL_keysym*, mod)  */
/* SCM_DEFINE_NUMBER_GETTER ("keysym:unicode", keysym_unicode, keysym_tag,  */
/*                         SDL_keysym*, unicode)  */

/* SCM event_key_keysym (SCM s_event) */
/* { */
/* } */


/* SDL event functions */

/* extern DECLSPEC void SDL_PumpEvents(void); */
SCM pump_events (void)
{
   SDL_PumpEvents();
   return SCM_UNSPECIFIED;
}

/* extern DECLSPEC int SDL_PeepEvents(SDL_Event *events, int numevents, */
/* 				SDL_eventaction action, Uint32 mask); */
SCM peep_events (SCM events, SCM numevents, SCM action, SCM mask)
{
   return SCM_UNSPECIFIED;
}

/* extern DECLSPEC int SDL_PollEvent(SDL_Event *event); */
SCM poll_event (SCM event)
{
   int result;

   if (event == SCM_UNDEFINED) {
      /* no args */
      result = SDL_PollEvent (NULL);
   } else {
      /* we're given an event smob - fill it */
      SCM_ASSERT_SMOB (event, event_tag, SCM_ARG1, "sdl-poll-event");
      result = SDL_PollEvent ((SDL_Event*) SCM_CDR (event));
   }

   return scm_long2num (result);
}

/* extern DECLSPEC int SDL_WaitEvent(SDL_Event *event); */
SCM wait_event (SCM event)
{
   int result;

   if (event == SCM_UNDEFINED) {
      /* no args */
      result = SDL_WaitEvent (NULL);
   } else {
      /* we're given an event smob - fill it */
      SCM_ASSERT_SMOB (event, event_tag, SCM_ARG1, "sdl-wait-event");
      result = SDL_WaitEvent ((SDL_Event*) SCM_CDR (event));
   }

   return scm_long2num (result);
}

/* extern DECLSPEC int SDL_PushEvent(SDL_Event *event); */
SCM push_event (SCM event)
{
   int result;

   SCM_ASSERT_SMOB (event, event_tag, SCM_ARG1, "sdl-push-event");

   result = SDL_PushEvent ((SDL_Event*) SCM_CDR (event));
   return scm_long2num (result);
}

/* extern DECLSPEC void SDL_SetEventFilter(SDL_EventFilter filter); */
SCM set_event_filter (SCM filter)
{
   return SCM_UNSPECIFIED;
}

/* extern DECLSPEC SDL_EventFilter SDL_GetEventFilter(void); */
SCM get_event_filter (SCM filter)
{
   return SCM_UNSPECIFIED;
}

/* extern DECLSPEC Uint8 SDL_EventState(Uint8 type, int state); */
SCM event_state (SCM type, SCM state)
{
   return SCM_UNSPECIFIED;
}


SCM
sdl_enable_unicode (SCM enable_p)
{
   int result;

   if (enable_p == SCM_UNDEFINED) {
      result = SDL_EnableUNICODE (-1);
   } else if (SCM_FALSEP (enable_p)) {
      result = SDL_EnableUNICODE (0);
   } else {
      result = SDL_EnableUNICODE (1);
   }

   return scm_long2num (result);
}

/*
 * If 'delay' is set to 0, keyboard repeat is disabled.
 */
SCM
sdl_enable_key_repeat (SCM s_delay, SCM s_interval)
{
   int interval, delay;

   SCM_ASSERT (scm_exact_p (s_delay), s_delay, SCM_ARG1, "sdl-enable-key-repeat");
   SCM_ASSERT (scm_exact_p (s_interval), s_interval, SCM_ARG2, "sdl-enable-key-repeat");

   delay    = scm_num2long (s_delay, SCM_ARG1, "scm_num2long");
   interval = scm_num2long (s_interval, SCM_ARG1, "scm_num2long");

   SCM_RETURN_TRUE_IF_0 (SDL_EnableKeyRepeat (delay, interval));
}

/*
 * Get a snapshot of the current state of the keyboard.
 * Returns an array of keystates, indexed by the SDLK_* syms.
 */
SCM
sdl_get_key_state (SCM numkeys)
{
   return SCM_UNSPECIFIED;
}

/*
 * Get the current key modifier state
 */
SCM
sdl_get_mod_state (void)
{
   return scm_long2num (SDL_GetModState ());
}

/*
 * Set the current key modifier state
 * This does not change the keyboard state, only the key modifier flags.
 */
SCM
sdl_set_mod_state (SCM modstate)
{
   SCM_ASSERT (scm_exact_p (modstate), modstate, SCM_ARG1, "sdl-set-mod-state");
   SDL_SetModState (scm_num2long (modstate, SCM_ARG1, "scm_num2long"));
   return SCM_UNSPECIFIED;
}

/*
 * Retrieve the current state of the mouse.
 * The current button state is returned as a button bitmask, which can
 * be tested using the SDL_BUTTON(X) macros, and x and y are set to the
 * current mouse cursor position.  You can pass NULL for either x or y.
 */
SCM
sdl_get_mouse_state (void)
{
   int buttons, x, y;
   buttons = SDL_GetMouseState (&x, &y);
   return SCM_LIST3 (scm_long2num (buttons),
                     scm_long2num (x),
                     scm_long2num (y));
}

/*
 * Retrieve the current state of the mouse.
 * The current button state is returned as a button bitmask, which can
 * be tested using the SDL_BUTTON(X) macros, and x and y are set to the
 * mouse deltas since the last call to SDL_GetRelativeMouseState().
 */
SCM
sdl_get_relative_mouse_state ()
{
   int buttons, x, y;
   buttons = SDL_GetMouseState (&x, &y);
   return SCM_LIST3 (scm_long2num (buttons),
                     scm_long2num (x),
                     scm_long2num (y));
}

SCM
sdl_button_p (SCM mask)
{
   SCM_ASSERT (scm_exact_p (mask), mask, SCM_ARG1, "sdl-button?");
   return SDL_BUTTON (scm_num2long (mask, SCM_ARG1, "scm_num2long")) ? SCM_BOOL_T : SCM_BOOL_F;
}

/* Initialize glue */
void sdl_init_event (void)
{
   /* tags */
   event_tag   = scm_make_smob_type ("event",  sizeof (SDL_Event));
   /* keysym_tag  = scm_make_smob_type ("keysym", sizeof (SDL_keysym)); */

   scm_set_smob_free (event_tag, free_event);

   /* event type constants */
   event_type_enum = scm_c_define_enum (
      "sdl-event-types",
      "event/active",            SDL_ACTIVEEVENT,
      "event/key-down",          SDL_KEYDOWN,
      "event/key-up",            SDL_KEYUP,
      "event/mouse-motion",      SDL_MOUSEMOTION,
      "event/mouse-button-down", SDL_MOUSEBUTTONDOWN,
      "event/mouse-button-up",   SDL_MOUSEBUTTONUP,
      "event/joy-axis-motion",   SDL_JOYAXISMOTION,
      "event/joy-ball-motion",   SDL_JOYBALLMOTION,
      "event/joy-hat-motion",    SDL_JOYHATMOTION,
      "event/joy-button-down",   SDL_JOYBUTTONDOWN,
      "event/joy-button-up",     SDL_JOYBUTTONUP,
      "event/quit",              SDL_QUIT,
      "event/sys-wm",            SDL_SYSWMEVENT,
      "event/video-resize",      SDL_VIDEORESIZE,
      "event/user",              SDL_USEREVENT,
      NULL);

   /* keysyms */
   event_keysym_enum = scm_c_define_enum (
      "sdl-event-keys",
      "key/backspace",  SDLK_BACKSPACE,
      "key/tab",  SDLK_TAB,
      "key/clear",  SDLK_CLEAR,
      "key/return",  SDLK_RETURN,
      "key/pause",  SDLK_PAUSE,
      "key/escape",  SDLK_ESCAPE,
      "key/space",  SDLK_SPACE,
      "key/exclaim",  SDLK_EXCLAIM,
      "key/quotedbl",  SDLK_QUOTEDBL,
      "key/hash",  SDLK_HASH,
      "key/dollar",  SDLK_DOLLAR,
      "key/ampersand",  SDLK_AMPERSAND,
      "key/quote",  SDLK_QUOTE,
      "key/leftparen",  SDLK_LEFTPAREN,
      "key/rightparen",  SDLK_RIGHTPAREN,
      "key/asterisk",  SDLK_ASTERISK,
      "key/plus",  SDLK_PLUS,
      "key/comma",  SDLK_COMMA,
      "key/minus",  SDLK_MINUS,
      "key/period",  SDLK_PERIOD,
      "key/slash",  SDLK_SLASH,
      "key/0",  SDLK_0,
      "key/1",  SDLK_1,
      "key/2",  SDLK_2,
      "key/3",  SDLK_3,
      "key/4",  SDLK_4,
      "key/5",  SDLK_5,
      "key/6",  SDLK_6,
      "key/7",  SDLK_7,
      "key/8",  SDLK_8,
      "key/9",  SDLK_9,
      "key/colon",  SDLK_COLON,
      "key/semicolon",  SDLK_SEMICOLON,
      "key/less",  SDLK_LESS,
      "key/equals",  SDLK_EQUALS,
      "key/greater",  SDLK_GREATER,
      "key/question",  SDLK_QUESTION,
      "key/at",  SDLK_AT,
      "key/leftbracket",  SDLK_LEFTBRACKET,
      "key/backslash",  SDLK_BACKSLASH,
      "key/rightbracket",  SDLK_RIGHTBRACKET,
      "key/caret",  SDLK_CARET,
      "key/underscore",  SDLK_UNDERSCORE,
      "key/backquote",  SDLK_BACKQUOTE,
      "key/a",  SDLK_a,
      "key/b",  SDLK_b,
      "key/c",  SDLK_c,
      "key/d",  SDLK_d,
      "key/e",  SDLK_e,
      "key/f",  SDLK_f,
      "key/g",  SDLK_g,
      "key/h",  SDLK_h,
      "key/i",  SDLK_i,
      "key/j",  SDLK_j,
      "key/k",  SDLK_k,
      "key/l",  SDLK_l,
      "key/m",  SDLK_m,
      "key/n",  SDLK_n,
      "key/o",  SDLK_o,
      "key/p",  SDLK_p,
      "key/q",  SDLK_q,
      "key/r",  SDLK_r,
      "key/s",  SDLK_s,
      "key/t",  SDLK_t,
      "key/u",  SDLK_u,
      "key/v",  SDLK_v,
      "key/w",  SDLK_w,
      "key/x",  SDLK_x,
      "key/y",  SDLK_y,
      "key/z",  SDLK_z,
      "key/delete",  SDLK_DELETE,
      "key/kp0",  SDLK_KP0,
      "key/kp1",  SDLK_KP1,
      "key/kp2",  SDLK_KP2,
      "key/kp3",  SDLK_KP3,
      "key/kp4",  SDLK_KP4,
      "key/kp5",  SDLK_KP5,
      "key/kp6",  SDLK_KP6,
      "key/kp7",  SDLK_KP7,
      "key/kp8",  SDLK_KP8,
      "key/kp9",  SDLK_KP9,
      "key/kp-period",  SDLK_KP_PERIOD,
      "key/kp-divide",  SDLK_KP_DIVIDE,
      "key/kp-multiply",  SDLK_KP_MULTIPLY,
      "key/kp-minus",  SDLK_KP_MINUS,
      "key/kp-plus",  SDLK_KP_PLUS,
      "key/kp-enter",  SDLK_KP_ENTER,
      "key/kp-equals",  SDLK_KP_EQUALS,
      "key/up",  SDLK_UP,
      "key/down",  SDLK_DOWN,
      "key/right",  SDLK_RIGHT,
      "key/left",  SDLK_LEFT,
      "key/insert",  SDLK_INSERT,
      "key/home",  SDLK_HOME,
      "key/end",  SDLK_END,
      "key/pageup",  SDLK_PAGEUP,
      "key/pagedown",  SDLK_PAGEDOWN,
      "key/f1",  SDLK_F1,
      "key/f2",  SDLK_F2,
      "key/f3",  SDLK_F3,
      "key/f4",  SDLK_F4,
      "key/f5",  SDLK_F5,
      "key/f6",  SDLK_F6,
      "key/f7",  SDLK_F7,
      "key/f8",  SDLK_F8,
      "key/f9",  SDLK_F9,
      "key/f10",  SDLK_F10,
      "key/f11",  SDLK_F11,
      "key/f12",  SDLK_F12,
      "key/f13",  SDLK_F13,
      "key/f14",  SDLK_F14,
      "key/f15",  SDLK_F15,
      "key/numlock",  SDLK_NUMLOCK,
      "key/capslock",  SDLK_CAPSLOCK,
      "key/scrollock",  SDLK_SCROLLOCK,
      "key/rshift",  SDLK_RSHIFT,
      "key/lshift",  SDLK_LSHIFT,
      "key/rctrl",  SDLK_RCTRL,
      "key/lctrl",  SDLK_LCTRL,
      "key/ralt",  SDLK_RALT,
      "key/lalt",  SDLK_LALT,
      "key/rmeta",  SDLK_RMETA,
      "key/lmeta",  SDLK_LMETA,
      "key/lsuper",  SDLK_LSUPER,
      "key/rsuper",  SDLK_RSUPER,
      "key/mode",  SDLK_MODE,
      "key/help",  SDLK_HELP,
      "key/print",  SDLK_PRINT,
      "key/sysreq",  SDLK_SYSREQ,
      "key/break",  SDLK_BREAK,
      "key/menu",  SDLK_MENU,
      "key/power",  SDLK_POWER,
      "key/euro",  SDLK_EURO,
      NULL);

   /* modsysms */
   SCM_DEFINE_CONST ("sdl-mod/none",     KMOD_NONE);
   SCM_DEFINE_CONST ("sdl-mod/lshift",   KMOD_LSHIFT);
   SCM_DEFINE_CONST ("sdl-mod/rshift",   KMOD_RSHIFT);
   SCM_DEFINE_CONST ("sdl-mod/lctrl",    KMOD_LCTRL);
   SCM_DEFINE_CONST ("sdl-mod/rctrl",    KMOD_RCTRL);
   SCM_DEFINE_CONST ("sdl-mod/lalt",     KMOD_LALT);
   SCM_DEFINE_CONST ("sdl-mod/ralt",     KMOD_RALT);
   SCM_DEFINE_CONST ("sdl-mod/lmeta",    KMOD_LMETA);
   SCM_DEFINE_CONST ("sdl-mod/rmeta",    KMOD_RMETA);
   SCM_DEFINE_CONST ("sdl-mod/num",      KMOD_NUM);
   SCM_DEFINE_CONST ("sdl-mod/caps",     KMOD_CAPS);
   SCM_DEFINE_CONST ("sdl-mod/mode",     KMOD_MODE);
   SCM_DEFINE_CONST ("sdl-mod/reserved", KMOD_RESERVED);
   /* aggregate modifiers */
   SCM_DEFINE_CONST ("sdl-mod/ctrl",     KMOD_CTRL);
   SCM_DEFINE_CONST ("sdl-mod/shift",    KMOD_SHIFT);
   SCM_DEFINE_CONST ("sdl-mod/alt",      KMOD_ALT);
   SCM_DEFINE_CONST ("sdl-mod/meta",     KMOD_META);

   /* event states */
   SCM_DEFINE_CONST ("sdl-state/query",   SDL_QUERY);
   SCM_DEFINE_CONST ("sdl-state/ignore",  SDL_IGNORE);
   SCM_DEFINE_CONST ("sdl-state/disable", SDL_DISABLE);
   SCM_DEFINE_CONST ("sdl-state/enable",  SDL_ENABLE);

   /* functions */
   scm_c_define_gsubr ("sdl-make-event",      0, 1, 0, make_event);
   scm_c_define_gsubr ("sdl-make-keysym",     0, 2, 0, make_keysym);
   scm_c_define_gsubr ("sdl-poll-event",      0, 1, 0, poll_event);
   scm_c_define_gsubr ("sdl-wait-event",      0, 1, 0, wait_event);
   scm_c_define_gsubr ("sdl-push-event",      1, 0, 0, push_event);
   scm_c_define_gsubr ("sdl-enable-unicode",  0, 1, 0, sdl_enable_unicode);
   scm_c_define_gsubr ("sdl-enable-key-repeat",  2, 0, 0, sdl_enable_key_repeat);
   scm_c_define_gsubr ("sdl-get-key-state",   0, 0, 0, sdl_get_key_state);
   scm_c_define_gsubr ("sdl-get-mod-state",   0, 0, 0, sdl_get_mod_state);
   scm_c_define_gsubr ("sdl-set-mod-state",   1, 0, 0, sdl_set_mod_state);
   scm_c_define_gsubr ("sdl-get-mouse-state", 0, 0, 0, sdl_get_mouse_state);
   scm_c_define_gsubr ("sdl-get-relative-mouse-state", 0, 0, 0,
                       sdl_get_relative_mouse_state);
   scm_c_define_gsubr ("sdl-button?",         1, 0, 0, sdl_button_p);

   /* smob getters */
   scm_c_define_gsubr ("sdl-event:type", 1, 0, 0, event_type);
   scm_c_define_gsubr ("sdl-event:active:gain", 1, 0, 0, event_active_gain);
   scm_c_define_gsubr ("sdl-event:active:state", 1, 0, 0, event_active_state);
   scm_c_define_gsubr ("sdl-event:key:state", 1, 0, 0, event_key_state);
   scm_c_define_gsubr ("sdl-event:key:keysym:sym", 1, 0, 0, event_key_keysym_sym);
   scm_c_define_gsubr ("sdl-event:key:keysym:mod", 1, 0, 0, event_key_keysym_mod);
   scm_c_define_gsubr ("sdl-event:key:keysym:scancode", 1, 0, 0, event_key_keysym_scancode);
   scm_c_define_gsubr ("sdl-event:key:keysym:unicode", 1, 0, 0, event_key_keysym_unicode);
/*    scm_c_define_gsubr ("sdl-event:motion:state", 1, 0, 0, event_motion_state); */
/*    scm_c_define_gsubr ("sdl-event:motion:x", 1, 0, 0, event_motion_x); */
/*    scm_c_define_gsubr ("sdl-event:motion:y", 1, 0, 0, event_motion_y); */
/*    scm_c_define_gsubr ("sdl-event:motion:xrel", 1, 0, 0, event_motion_xrel); */
/*    scm_c_define_gsubr ("sdl-event:motion:yrel", 1, 0, 0, event_motion_yrel); */
/*    scm_c_define_gsubr ("sdl-event:button:button", 1, 0, 0, event_button_button); */
/*    scm_c_define_gsubr ("sdl-event:button:state", 1, 0, 0, event_button_state); */
/*    scm_c_define_gsubr ("sdl-event:button:x", 1, 0, 0, event_button_x); */
/*    scm_c_define_gsubr ("sdl-event:button:y", 1, 0, 0, event_button_y); */
/*    scm_c_define_gsubr ("sdl-event:jaxis:which", 1, 0, 0, event_jaxis_which); */
/*    scm_c_define_gsubr ("sdl-event:jaxis:axis", 1, 0, 0, event_jaxis_axis); */
/*    scm_c_define_gsubr ("sdl-event:jaxis:value", 1, 0, 0, event_jaxis_value); */
/*    scm_c_define_gsubr ("sdl-event:jbutton:which", 1, 0, 0, event_jbutton_which); */
/*    scm_c_define_gsubr ("sdl-event:jbutton:button", 1, 0, 0, event_jbutton_button); */
/*    scm_c_define_gsubr ("sdl-event:jbutton:state", 1, 0, 0, event_jbutton_state); */
/*    scm_c_define_gsubr ("sdl-event:jball:which", 1, 0, 0, event_jball_which); */
/*    scm_c_define_gsubr ("sdl-event:jball:ball", 1, 0, 0, event_jball_ball); */
/*    scm_c_define_gsubr ("sdl-event:jball:xrel", 1, 0, 0, event_jball_xrel); */
/*    scm_c_define_gsubr ("sdl-event:jball:yrel", 1, 0, 0, event_jball_yrel); */
/*    scm_c_define_gsubr ("sdl-event:jhat:which", 1, 0, 0, event_jhat_which); */
/*    scm_c_define_gsubr ("sdl-event:jhat:hat", 1, 0, 0, event_jhat_hat); */
/*    scm_c_define_gsubr ("sdl-event:jhat:value", 1, 0, 0, event_jhat_value); */
/*    scm_c_define_gsubr ("sdl-event:resize:w", 1, 0, 0, event_resize_w); */
/*    scm_c_define_gsubr ("sdl-event:resize:h", 1, 0, 0, event_resize_h); */
/*    scm_c_define_gsubr ("sdl-event:user:code", 1, 0, 0, event_user_code); */
/* /\*    scm_c_define_gsubr ("sdl-event:user:data1", 1, 0, 0, event_user_data1); *\/ */
/* /\*    scm_c_define_gsubr ("sdl-event:user:data2", 1, 0, 0, event_user_data2); *\/ */
/*    scm_c_define_gsubr ("sdl-keysym:scancode", 1, 0, 0, keysym_scancode); */
/*    scm_c_define_gsubr ("sdl-keysym:sym", 1, 0, 0, keysym_sym); */
/*    scm_c_define_gsubr ("sdl-keysym:mod", 1, 0, 0, keysym_mod); */
/*    scm_c_define_gsubr ("sdl-keysym:unicode", 1, 0, 0, keysym_unicode); */

   /* exported symbols */
   scm_c_export (
      /* enums */
      "sdl-event-types", "sdl-event-keys",
      /* mod constants */
      "sdl-mod/none",    "sdl-mod/lshift",   "sdl-mod/rshift",
      "sdl-mod/lctrl",   "sdl-mod/rctrl",    "sdl-mod/lalt",
      "sdl-mod/ralt",    "sdl-mod/lmeta",    "sdl-mod/rmeta",
      "sdl-mod/num",     "sdl-mod/caps",     "sdl-mod/mode",
      "sdl-mod/reserved",
      /* event states */
      "sdl-state/query",       "sdl-state/ignore",
      "sdl-state/disable",     "sdl-state/enable",
      /* smob getters */
      "sdl-event:type",                "sdl-event:active:gain",
      "sdl-event:active:state",        "sdl-event:key:state",
      "sdl-event:key:keysym:sym",      "sdl-event:key:keysym:mod",
      "sdl-event:key:keysym:scancode", "sdl-event:key:keysym:unicode",
/*       "sdl-event:motion:state",        "sdl-event:motion:x", */
/*       "sdl-event:motion:y",            "sdl-event:motion:xrel", */
/*       "sdl-event:motion:yrel",         "sdl-event:button:button", */
/*       "sdl-event:button:state",        "sdl-event:button:x", */
/*       "sdl-event:button:y",            "sdl-event:jaxis:which", */
/*       "sdl-event:jaxis:axis",          "sdl-event:jaxis:value", */
/*       "sdl-event:jbutton:which",       "sdl-event:jbutton:button", */
/*       "sdl-event:jbutton:state",       "sdl-event:jball:which", */
/*       "sdl-event:jball:ball",          "sdl-event:jball:xrel", */
/*       "sdl-event:jball:yrel",          "sdl-event:jhat:which", */
/*       "sdl-event:jhat:hat",            "sdl-event:jhat:value", */
/*       "sdl-event:resize:w",            "sdl-event:resize:h", */
/*       "sdl-event:user:code",           "sdl-event:user:data1", */
/*       "sdl-event:user:data2",          "sdl-keysym:scancode", */
/*       "sdl-keysym:sym",                "sdl-keysym:mod", */
/*       "sdl-keysym:unicode", */
      /* event functions */
      "sdl-make-event",  "sdl-make-keysym",  "sdl-poll-event",  "sdl-wait-event",
      "sdl-push-event",
      /* keyboard functions */
      "sdl-enable-unicode",    "sdl-enable-key-repeat",     "sdl-get-key-state",
      "sdl-get-mod-state",     "sdl-set-mod-state",
      /* mouse functions */
      "sdl-get-mouse-state",   "sdl-get-relative-mouse-state", "sdl-button?",
      NULL);
}

