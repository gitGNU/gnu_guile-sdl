/*******************************************************************
 *  event.c -- SDL input handling for Guile                        *
 *                                                                 *
 *  Created:    <2001-05-27 13:58:16 foof>                         *
 *  Time-stamp: <2001-06-04 01:06:30 foof>                         *
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
#include "scm.h"
#include "event.h"

/* tags for SDL smobs */
long event_tag;
long keysym_tag;

/* constructors */
SCM make_event (SCM s_type)
{
   SDL_Event *event;

   SCM_ASSERT (SCM_INUMP (s_type), s_type, SCM_ARG1, "make-event");

   event = (SDL_Event *) scm_must_malloc (sizeof (SDL_Event), "event");
   event->type = SCM_INUM (s_type);

   SCM_RETURN_NEWSMOB (event_tag, event);
}

SCM make_keysym (SCM sym, SCM mod)
{
   SDL_keysym *keysym;

   /* alloc the keysym */
   keysym = (SDL_keysym *) scm_must_malloc (sizeof (SDL_keysym), "keysym");

   /* set the sym if given */
   if (sym != SCM_UNDEFINED) {
      SCM_ASSERT (SCM_INUMP (sym), sym, SCM_ARG1, "make-keysym");
      keysym->sym = (SDLKey) SCM_INUM (sym);
   }

   /* set the mod if given */
   if (mod != SCM_UNDEFINED) {
      SCM_ASSERT (SCM_INUMP (mod), mod, SCM_ARG2, "make-keysym");
      keysym->mod = (SDLMod) SCM_INUM (mod);
   }

   /* return the new smob */
   SCM_RETURN_NEWSMOB (keysym_tag, keysym);
}

/* smob getters */

SCM_DEFINE_INUM_GETTER ("event:type", event_type, event_tag, 
                        SDL_Event*, type) 

SCM_DEFINE_INUM_GETTER ("event:active:gain", event_active_gain, event_tag, 
                        SDL_Event*, active.gain) 
SCM_DEFINE_INUM_GETTER ("event:active:state", event_active_state, event_tag, 
                        SDL_Event*, active.state) 

SCM_DEFINE_INUM_GETTER ("event:key:state", event_key_state, event_tag, 
                        SDL_Event*, key.state) 

SCM_DEFINE_INUM_GETTER ("event:key:keysym:sym", event_key_keysym_sym, 
                        event_tag, SDL_Event*, key.keysym.sym) 
SCM_DEFINE_INUM_GETTER ("event:key:keysym:mod", event_key_keysym_mod, 
                        event_tag, SDL_Event*, key.keysym.mod) 
SCM_DEFINE_INUM_GETTER ("event:key:keysym:scancode", event_key_keysym_scancode, 
                        event_tag, SDL_Event*, key.keysym.scancode) 
SCM_DEFINE_INUM_GETTER ("event:key:keysym:unicode", event_key_keysym_unicode, 
                        event_tag, SDL_Event*, key.keysym.unicode) 

SCM_DEFINE_INUM_GETTER ("event:motion:state", event_motion_state, event_tag, 
                        SDL_Event*, motion.state) 
SCM_DEFINE_INUM_GETTER ("event:motion:x", event_motion_x, event_tag, 
                        SDL_Event*, motion.x) 
SCM_DEFINE_INUM_GETTER ("event:motion:y", event_motion_y, event_tag, 
                        SDL_Event*, motion.y) 
SCM_DEFINE_INUM_GETTER ("event:motion:xrel", event_motion_xrel, event_tag, 
                        SDL_Event*, motion.xrel) 
SCM_DEFINE_INUM_GETTER ("event:motion:yrel", event_motion_yrel, event_tag, 
                        SDL_Event*, motion.yrel) 

SCM_DEFINE_INUM_GETTER ("event:button:button", event_button_button, event_tag, 
                        SDL_Event*, button.button) 
SCM_DEFINE_INUM_GETTER ("event:button:state", event_button_state, event_tag, 
                        SDL_Event*, button.state) 
SCM_DEFINE_INUM_GETTER ("event:button:x", event_button_x, event_tag, 
                        SDL_Event*, button.x) 
SCM_DEFINE_INUM_GETTER ("event:button:y", event_button_y, event_tag, 
                        SDL_Event*, button.y) 

SCM_DEFINE_INUM_GETTER ("event:jaxis:which", event_jaxis_which, event_tag, 
                        SDL_Event*, jaxis.which) 
SCM_DEFINE_INUM_GETTER ("event:jaxis:axis", event_jaxis_axis, event_tag, 
                        SDL_Event*, jaxis.axis) 
SCM_DEFINE_INUM_GETTER ("event:jaxis:value", event_jaxis_value, event_tag, 
                        SDL_Event*, jaxis.value) 

SCM_DEFINE_INUM_GETTER ("event:jbutton:which", event_jbutton_which, event_tag, 
                        SDL_Event*, jbutton.which) 
SCM_DEFINE_INUM_GETTER ("event:jbutton:button", event_jbutton_button, event_tag, 
                        SDL_Event*, jbutton.button) 
SCM_DEFINE_INUM_GETTER ("event:jbutton:state", event_jbutton_state, event_tag, 
                        SDL_Event*, jbutton.state) 

SCM_DEFINE_INUM_GETTER ("event:jball:which", event_jball_which, event_tag, 
                        SDL_Event*, jball.which) 
SCM_DEFINE_INUM_GETTER ("event:jball:ball", event_jball_ball, event_tag, 
                        SDL_Event*, jball.ball) 
SCM_DEFINE_INUM_GETTER ("event:jball:xrel", event_jball_xrel, event_tag, 
                        SDL_Event*, jball.xrel) 
SCM_DEFINE_INUM_GETTER ("event:jball:yrel", event_jball_yrel, event_tag, 
                        SDL_Event*, jball.yrel) 

SCM_DEFINE_INUM_GETTER ("event:jhat:which", event_jhat_which, event_tag, 
                        SDL_Event*, jhat.which) 
SCM_DEFINE_INUM_GETTER ("event:jhat:hat", event_jhat_hat, event_tag, 
                        SDL_Event*, jhat.hat) 
SCM_DEFINE_INUM_GETTER ("event:jhat:value", event_jhat_state, event_tag, 
                        SDL_Event*, jhat.value) 

SCM_DEFINE_INUM_GETTER ("event:resize:w", event_resize_w, event_tag, 
                        SDL_Event*, resize.w) 
SCM_DEFINE_INUM_GETTER ("event:resize:h", event_resize_h, event_tag, 
                        SDL_Event*, resize.h) 

SCM_DEFINE_INUM_GETTER ("event:user:code", event_user_code, event_tag, 
                        SDL_Event*, user.code) 
/* SCM_DEFINE_INUM_GETTER ("event:user:data1", event_user_data1, event_tag,  */
/*                         SDL_Event*, user.data1)  */
/* SCM_DEFINE_INUM_GETTER ("event:user:data2", event_user_data2, event_tag,  */
/*                         SDL_Event*, user.data2)  */

SCM_DEFINE_INUM_GETTER ("keysym:scancode", keysym_scancode, keysym_tag, 
                        SDL_keysym*, scancode) 
SCM_DEFINE_INUM_GETTER ("keysym:sym", keysym_sym, keysym_tag, 
                        SDL_keysym*, sym) 
SCM_DEFINE_INUM_GETTER ("keysym:mod", keysym_mod, keysym_tag, 
                        SDL_keysym*, mod) 
SCM_DEFINE_INUM_GETTER ("keysym:unicode", keysym_unicode, keysym_tag, 
                        SDL_keysym*, unicode) 

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
      SCM_ASSERT_SMOB (event, event_tag, SCM_ARG1, "poll-event");
      result = SDL_PollEvent ((SDL_Event*) SCM_CDR (event));
   }

   return SCM_MAKINUM (result);
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
      SCM_ASSERT_SMOB (event, event_tag, SCM_ARG1, "wait-event");
      result = SDL_WaitEvent ((SDL_Event*) SCM_CDR (event));
   }

   return SCM_MAKINUM (result);
}

/* extern DECLSPEC int SDL_PushEvent(SDL_Event *event); */
SCM push_event (SCM event)
{
   int result;

   SCM_ASSERT_SMOB (event, event_tag, SCM_ARG1, "push-event");

   result = SDL_PushEvent ((SDL_Event*) SCM_CDR (event));
   return SCM_MAKINUM (result);
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


/* Initialize glue */
void sdl_event_init (void)
{
   /* tags */
   event_tag   = scm_make_smob_type ("event",  sizeof (SDL_Event));
   keysym_tag  = scm_make_smob_type ("keysym", sizeof (SDL_keysym));

   /* event type constants */
/*    SCM_DEFINE_CONST ("event/active",            SDL_ACTIVEEVENT); */
/*    SCM_DEFINE_CONST ("event/key-down",          SDL_KEYDOWN); */
/*    SCM_DEFINE_CONST ("event/key-up",            SDL_KEYUP); */
/*    SCM_DEFINE_CONST ("event/mouse-motion",      SDL_MOUSEMOTION); */
/*    SCM_DEFINE_CONST ("event/mouse-button-down", SDL_MOUSEBUTTONDOWN); */
/*    SCM_DEFINE_CONST ("event/mouse-button-up",   SDL_MOUSEBUTTONUP); */
/*    SCM_DEFINE_CONST ("event/joy-axis-motion",   SDL_JOYAXISMOTION); */
/*    SCM_DEFINE_CONST ("event/joy-ball-motion",   SDL_JOYBALLMOTION); */
/*    SCM_DEFINE_CONST ("event/joy-hat-motion",    SDL_JOYHATMOTION); */
/*    SCM_DEFINE_CONST ("event/joy-button-down",   SDL_JOYBUTTONDOWN); */
/*    SCM_DEFINE_CONST ("event/joy-button-up",     SDL_JOYBUTTONUP); */
/*    SCM_DEFINE_CONST ("event/quit",              SDL_QUIT); */
/*    SCM_DEFINE_CONST ("event/sys-wm",            SDL_SYSWMEVENT); */
/*    SCM_DEFINE_CONST ("event/video-resize",      SDL_VIDEORESIZE); */
/*    SCM_DEFINE_CONST ("event/user",              SDL_USEREVENT); */

   /* keysyms */
/*    SCM_DEFINE_CONST ("key/backspace",  SDLK_BACKSPACE); */
/*    SCM_DEFINE_CONST ("key/tab",  SDLK_TAB); */
/*    SCM_DEFINE_CONST ("key/clear",  SDLK_CLEAR); */
/*    SCM_DEFINE_CONST ("key/return",  SDLK_RETURN); */
/*    SCM_DEFINE_CONST ("key/pause",  SDLK_PAUSE); */
/*    SCM_DEFINE_CONST ("key/escape",  SDLK_ESCAPE); */
/*    SCM_DEFINE_CONST ("key/space",  SDLK_SPACE); */
/*    SCM_DEFINE_CONST ("key/exclaim",  SDLK_EXCLAIM); */
/*    SCM_DEFINE_CONST ("key/quotedbl",  SDLK_QUOTEDBL); */
/*    SCM_DEFINE_CONST ("key/hash",  SDLK_HASH); */
/*    SCM_DEFINE_CONST ("key/dollar",  SDLK_DOLLAR); */
/*    SCM_DEFINE_CONST ("key/ampersand",  SDLK_AMPERSAND); */
/*    SCM_DEFINE_CONST ("key/quote",  SDLK_QUOTE); */
/*    SCM_DEFINE_CONST ("key/leftparen",  SDLK_LEFTPAREN); */
/*    SCM_DEFINE_CONST ("key/rightparen",  SDLK_RIGHTPAREN); */
/*    SCM_DEFINE_CONST ("key/asterisk",  SDLK_ASTERISK); */
/*    SCM_DEFINE_CONST ("key/plus",  SDLK_PLUS); */
/*    SCM_DEFINE_CONST ("key/comma",  SDLK_COMMA); */
/*    SCM_DEFINE_CONST ("key/minus",  SDLK_MINUS); */
/*    SCM_DEFINE_CONST ("key/period",  SDLK_PERIOD); */
/*    SCM_DEFINE_CONST ("key/slash",  SDLK_SLASH); */
/*    SCM_DEFINE_CONST ("key/0",  SDLK_0); */
/*    SCM_DEFINE_CONST ("key/1",  SDLK_1); */
/*    SCM_DEFINE_CONST ("key/2",  SDLK_2); */
/*    SCM_DEFINE_CONST ("key/3",  SDLK_3); */
/*    SCM_DEFINE_CONST ("key/4",  SDLK_4); */
/*    SCM_DEFINE_CONST ("key/5",  SDLK_5); */
/*    SCM_DEFINE_CONST ("key/6",  SDLK_6); */
/*    SCM_DEFINE_CONST ("key/7",  SDLK_7); */
/*    SCM_DEFINE_CONST ("key/8",  SDLK_8); */
/*    SCM_DEFINE_CONST ("key/9",  SDLK_9); */
/*    SCM_DEFINE_CONST ("key/colon",  SDLK_COLON); */
/*    SCM_DEFINE_CONST ("key/semicolon",  SDLK_SEMICOLON); */
/*    SCM_DEFINE_CONST ("key/less",  SDLK_LESS); */
/*    SCM_DEFINE_CONST ("key/equals",  SDLK_EQUALS); */
/*    SCM_DEFINE_CONST ("key/greater",  SDLK_GREATER); */
/*    SCM_DEFINE_CONST ("key/question",  SDLK_QUESTION); */
/*    SCM_DEFINE_CONST ("key/at",  SDLK_AT); */
/*    SCM_DEFINE_CONST ("key/leftbracket",  SDLK_LEFTBRACKET); */
/*    SCM_DEFINE_CONST ("key/backslash",  SDLK_BACKSLASH); */
/*    SCM_DEFINE_CONST ("key/rightbracket",  SDLK_RIGHTBRACKET); */
/*    SCM_DEFINE_CONST ("key/caret",  SDLK_CARET); */
/*    SCM_DEFINE_CONST ("key/underscore",  SDLK_UNDERSCORE); */
/*    SCM_DEFINE_CONST ("key/backquote",  SDLK_BACKQUOTE); */
/*    SCM_DEFINE_CONST ("key/a",  SDLK_a); */
/*    SCM_DEFINE_CONST ("key/b",  SDLK_b); */
/*    SCM_DEFINE_CONST ("key/c",  SDLK_c); */
/*    SCM_DEFINE_CONST ("key/d",  SDLK_d); */
/*    SCM_DEFINE_CONST ("key/e",  SDLK_e); */
/*    SCM_DEFINE_CONST ("key/f",  SDLK_f); */
/*    SCM_DEFINE_CONST ("key/g",  SDLK_g); */
/*    SCM_DEFINE_CONST ("key/h",  SDLK_h); */
/*    SCM_DEFINE_CONST ("key/i",  SDLK_i); */
/*    SCM_DEFINE_CONST ("key/j",  SDLK_j); */
/*    SCM_DEFINE_CONST ("key/k",  SDLK_k); */
/*    SCM_DEFINE_CONST ("key/l",  SDLK_l); */
/*    SCM_DEFINE_CONST ("key/m",  SDLK_m); */
/*    SCM_DEFINE_CONST ("key/n",  SDLK_n); */
/*    SCM_DEFINE_CONST ("key/o",  SDLK_o); */
/*    SCM_DEFINE_CONST ("key/p",  SDLK_p); */
/*    SCM_DEFINE_CONST ("key/q",  SDLK_q); */
/*    SCM_DEFINE_CONST ("key/r",  SDLK_r); */
/*    SCM_DEFINE_CONST ("key/s",  SDLK_s); */
/*    SCM_DEFINE_CONST ("key/t",  SDLK_t); */
/*    SCM_DEFINE_CONST ("key/u",  SDLK_u); */
/*    SCM_DEFINE_CONST ("key/v",  SDLK_v); */
/*    SCM_DEFINE_CONST ("key/w",  SDLK_w); */
/*    SCM_DEFINE_CONST ("key/x",  SDLK_x); */
/*    SCM_DEFINE_CONST ("key/y",  SDLK_y); */
/*    SCM_DEFINE_CONST ("key/z",  SDLK_z); */
/*    SCM_DEFINE_CONST ("key/delete",  SDLK_DELETE); */
/*    SCM_DEFINE_CONST ("key/kp0",  SDLK_KP0); */
/*    SCM_DEFINE_CONST ("key/kp1",  SDLK_KP1); */
/*    SCM_DEFINE_CONST ("key/kp2",  SDLK_KP2); */
/*    SCM_DEFINE_CONST ("key/kp3",  SDLK_KP3); */
/*    SCM_DEFINE_CONST ("key/kp4",  SDLK_KP4); */
/*    SCM_DEFINE_CONST ("key/kp5",  SDLK_KP5); */
/*    SCM_DEFINE_CONST ("key/kp6",  SDLK_KP6); */
/*    SCM_DEFINE_CONST ("key/kp7",  SDLK_KP7); */
/*    SCM_DEFINE_CONST ("key/kp8",  SDLK_KP8); */
/*    SCM_DEFINE_CONST ("key/kp9",  SDLK_KP9); */
/*    SCM_DEFINE_CONST ("key/kp_period",  SDLK_KP_PERIOD); */
/*    SCM_DEFINE_CONST ("key/kp_divide",  SDLK_KP_DIVIDE); */
/*    SCM_DEFINE_CONST ("key/kp_multiply",  SDLK_KP_MULTIPLY); */
/*    SCM_DEFINE_CONST ("key/kp_minus",  SDLK_KP_MINUS); */
/*    SCM_DEFINE_CONST ("key/kp_plus",  SDLK_KP_PLUS); */
/*    SCM_DEFINE_CONST ("key/kp_enter",  SDLK_KP_ENTER); */
/*    SCM_DEFINE_CONST ("key/kp_equals",  SDLK_KP_EQUALS); */
/*    SCM_DEFINE_CONST ("key/up",  SDLK_UP); */
/*    SCM_DEFINE_CONST ("key/down",  SDLK_DOWN); */
/*    SCM_DEFINE_CONST ("key/right",  SDLK_RIGHT); */
/*    SCM_DEFINE_CONST ("key/left",  SDLK_LEFT); */
/*    SCM_DEFINE_CONST ("key/insert",  SDLK_INSERT); */
/*    SCM_DEFINE_CONST ("key/home",  SDLK_HOME); */
/*    SCM_DEFINE_CONST ("key/end",  SDLK_END); */
/*    SCM_DEFINE_CONST ("key/pageup",  SDLK_PAGEUP); */
/*    SCM_DEFINE_CONST ("key/pagedown",  SDLK_PAGEDOWN); */
/*    SCM_DEFINE_CONST ("key/f1",  SDLK_F1); */
/*    SCM_DEFINE_CONST ("key/f2",  SDLK_F2); */
/*    SCM_DEFINE_CONST ("key/f3",  SDLK_F3); */
/*    SCM_DEFINE_CONST ("key/f4",  SDLK_F4); */
/*    SCM_DEFINE_CONST ("key/f5",  SDLK_F5); */
/*    SCM_DEFINE_CONST ("key/f6",  SDLK_F6); */
/*    SCM_DEFINE_CONST ("key/f7",  SDLK_F7); */
/*    SCM_DEFINE_CONST ("key/f8",  SDLK_F8); */
/*    SCM_DEFINE_CONST ("key/f9",  SDLK_F9); */
/*    SCM_DEFINE_CONST ("key/f10",  SDLK_F10); */
/*    SCM_DEFINE_CONST ("key/f11",  SDLK_F11); */
/*    SCM_DEFINE_CONST ("key/f12",  SDLK_F12); */
/*    SCM_DEFINE_CONST ("key/f13",  SDLK_F13); */
/*    SCM_DEFINE_CONST ("key/f14",  SDLK_F14); */
/*    SCM_DEFINE_CONST ("key/f15",  SDLK_F15); */
/*    SCM_DEFINE_CONST ("key/numlock",  SDLK_NUMLOCK); */
/*    SCM_DEFINE_CONST ("key/capslock",  SDLK_CAPSLOCK); */
/*    SCM_DEFINE_CONST ("key/scrollock",  SDLK_SCROLLOCK); */
/*    SCM_DEFINE_CONST ("key/rshift",  SDLK_RSHIFT); */
/*    SCM_DEFINE_CONST ("key/lshift",  SDLK_LSHIFT); */
/*    SCM_DEFINE_CONST ("key/rctrl",  SDLK_RCTRL); */
/*    SCM_DEFINE_CONST ("key/lctrl",  SDLK_LCTRL); */
/*    SCM_DEFINE_CONST ("key/ralt",  SDLK_RALT); */
/*    SCM_DEFINE_CONST ("key/lalt",  SDLK_LALT); */
/*    SCM_DEFINE_CONST ("key/rmeta",  SDLK_RMETA); */
/*    SCM_DEFINE_CONST ("key/lmeta",  SDLK_LMETA); */
/*    SCM_DEFINE_CONST ("key/lsuper",  SDLK_LSUPER); */
/*    SCM_DEFINE_CONST ("key/rsuper",  SDLK_RSUPER); */
/*    SCM_DEFINE_CONST ("key/mode",  SDLK_MODE); */
/*    SCM_DEFINE_CONST ("key/help",  SDLK_HELP); */
/*    SCM_DEFINE_CONST ("key/print",  SDLK_PRINT); */
/*    SCM_DEFINE_CONST ("key/sysreq",  SDLK_SYSREQ); */
/*    SCM_DEFINE_CONST ("key/break",  SDLK_BREAK); */
/*    SCM_DEFINE_CONST ("key/menu",  SDLK_MENU); */
/*    SCM_DEFINE_CONST ("key/power",  SDLK_POWER); */
/*    SCM_DEFINE_CONST ("key/euro",  SDLK_EURO); */

   /* modsysms */
/*    SCM_DEFINE_CONST ("mod/none", KMOD_NONE); */
/*    SCM_DEFINE_CONST ("mod/lshift", KMOD_LSHIFT); */
/*    SCM_DEFINE_CONST ("mod/rshift", KMOD_RSHIFT); */
/*    SCM_DEFINE_CONST ("mod/lctrl", KMOD_LCTRL); */
/*    SCM_DEFINE_CONST ("mod/rctrl", KMOD_RCTRL); */
/*    SCM_DEFINE_CONST ("mod/lalt", KMOD_LALT); */
/*    SCM_DEFINE_CONST ("mod/ralt", KMOD_RALT); */
/*    SCM_DEFINE_CONST ("mod/lmeta", KMOD_LMETA); */
/*    SCM_DEFINE_CONST ("mod/rmeta", KMOD_RMETA); */
/*    SCM_DEFINE_CONST ("mod/num", KMOD_NUM); */
/*    SCM_DEFINE_CONST ("mod/caps", KMOD_CAPS); */
/*    SCM_DEFINE_CONST ("mod/mode", KMOD_MODE); */
/*    SCM_DEFINE_CONST ("mod/reserved", KMOD_RESERVED); */

   /* functions */
   scm_make_gsubr ("make-event",  1, 0, 0, make_event);
   scm_make_gsubr ("make-keysym", 0, 2, 0, make_keysym);
   scm_make_gsubr ("poll-event",  0, 1, 0, poll_event);
   scm_make_gsubr ("wait-event",  0, 1, 0, wait_event);
   scm_make_gsubr ("push-event",  1, 0, 0, push_event);

   /* smob getters */
/*    scm_make_gsubr ("event:type", 1, 0, 0, event_type); */
/*    scm_make_gsubr ("event:active:gain", 1, 0, 0, event_active_gain); */
/*    scm_make_gsubr ("event:active:state", 1, 0, 0, event_active_state); */
/*    scm_make_gsubr ("event:key:state", 1, 0, 0, event_key_state); */
/*    scm_make_gsubr ("event:key:keysym:sym", 1, 0, 0, event_key_keysym_sym); */
/*    scm_make_gsubr ("event:key:keysym:mod", 1, 0, 0, event_key_keysym_mod); */
/*    scm_make_gsubr ("event:key:keysym:scancode", 1, 0, 0, event_key_keysym_scancode); */
/*    scm_make_gsubr ("event:key:keysym:unicode", 1, 0, 0, event_key_keysym_unicode); */
/*    scm_make_gsubr ("event:motion:state", 1, 0, 0, event_motion_state); */
/*    scm_make_gsubr ("event:motion:x", 1, 0, 0, event_motion_x); */
/*    scm_make_gsubr ("event:motion:y", 1, 0, 0, event_motion_y); */
/*    scm_make_gsubr ("event:motion:xrel", 1, 0, 0, event_motion_xrel); */
/*    scm_make_gsubr ("event:motion:yrel", 1, 0, 0, event_motion_yrel); */
/*    scm_make_gsubr ("event:button:button", 1, 0, 0, event_button_button); */
/*    scm_make_gsubr ("event:button:state", 1, 0, 0, event_button_state); */
/*    scm_make_gsubr ("event:button:x", 1, 0, 0, event_button_x); */
/*    scm_make_gsubr ("event:button:y", 1, 0, 0, event_button_y); */
/*    scm_make_gsubr ("event:jaxis:which", 1, 0, 0, event_jaxis_which); */
/*    scm_make_gsubr ("event:jaxis:axis", 1, 0, 0, event_jaxis_axis); */
/*    scm_make_gsubr ("event:jaxis:value", 1, 0, 0, event_jaxis_value); */
/*    scm_make_gsubr ("event:jbutton:which", 1, 0, 0, event_jbutton_which); */
/*    scm_make_gsubr ("event:jbutton:button", 1, 0, 0, event_jbutton_button); */
/*    scm_make_gsubr ("event:jbutton:state", 1, 0, 0, event_jbutton_state); */
/*    scm_make_gsubr ("event:jball:which", 1, 0, 0, event_jball_which); */
/*    scm_make_gsubr ("event:jball:ball", 1, 0, 0, event_jball_ball); */
/*    scm_make_gsubr ("event:jball:xrel", 1, 0, 0, event_jball_xrel); */
/*    scm_make_gsubr ("event:jball:yrel", 1, 0, 0, event_jball_yrel); */
/*    scm_make_gsubr ("event:jhat:which", 1, 0, 0, event_jhat_which); */
/*    scm_make_gsubr ("event:jhat:hat", 1, 0, 0, event_jhat_hat); */
/*    scm_make_gsubr ("event:jhat:value", 1, 0, 0, event_jhat_value); */
/*    scm_make_gsubr ("event:resize:w", 1, 0, 0, event_resize_w); */
/*    scm_make_gsubr ("event:resize:h", 1, 0, 0, event_resize_h); */
/*    scm_make_gsubr ("event:user:code", 1, 0, 0, event_user_code); */
/* /\*    scm_make_gsubr ("event:user:data1", 1, 0, 0, event_user_data1); *\/ */
/* /\*    scm_make_gsubr ("event:user:data2", 1, 0, 0, event_user_data2); *\/ */
/*    scm_make_gsubr ("keysym:scancode", 1, 0, 0, keysym_scancode); */
/*    scm_make_gsubr ("keysym:sym", 1, 0, 0, keysym_sym); */
/*    scm_make_gsubr ("keysym:mod", 1, 0, 0, keysym_mod); */
/*    scm_make_gsubr ("keysym:unicode", 1, 0, 0, keysym_unicode); */

   /* exported symbols */
   scm_c_export (
      /* event constants */
/*       "event/active",              "event/key-down", */
/*       "event/key-up",              "event/mouse-motion", */
/*       "event/mouse-button-down",   "event/mouse-button-up", */
/*       "event/joy-axis-motion",     "event/joy-ball-motion", */
/*       "event/joy-hat-motion",      "event/joy-button-down", */
/*       "event/joy-button-up",       "event/quit", */
/*       "event/sys-wm",              "event/video-resize", */
/*       "event/user", */
      /* keysyms */
/*       "key/backspace",             "key/tab", */
/*       "key/clear",                 "key/return", */
/*       "key/pause",                 "key/escape", */
/*       "key/space",                 "key/exclaim", */
/*       "key/quotedbl",              "key/hash", */
/*       "key/dollar",                "key/ampersand", */
/*       "key/quote",                 "key/leftparen", */
/*       "key/rightparen",            "key/asterisk", */
/*       "key/plus",                  "key/comma", */
/*       "key/minus",                 "key/period", */
/*       "key/slash",                 "key/0", */
/*       "key/1",                     "key/2", */
/*       "key/3",                     "key/4", */
/*       "key/5",                     "key/6", */
/*       "key/7",                     "key/8", */
/*       "key/9",                     "key/colon", */
/*       "key/semicolon",             "key/less", */
/*       "key/equals",                "key/greater", */
/*       "key/question",              "key/at", */
/*       "key/leftbracket",           "key/backslash", */
/*       "key/rightbracket",          "key/caret", */
/*       "key/underscore",            "key/backquote", */
/*       "key/a",                     "key/b", */
/*       "key/c",                     "key/d", */
/*       "key/e",                     "key/f", */
/*       "key/g",                     "key/h", */
/*       "key/i",                     "key/j", */
/*       "key/k",                     "key/l", */
/*       "key/m",                     "key/n", */
/*       "key/o",                     "key/p", */
/*       "key/q",                     "key/r", */
/*       "key/s",                     "key/t", */
/*       "key/u",                     "key/v", */
/*       "key/w",                     "key/x", */
/*       "key/y",                     "key/z", */
/*       "key/delete",                "key/kp0", */
/*       "key/kp1",                   "key/kp2", */
/*       "key/kp3",                   "key/kp4", */
/*       "key/kp5",                   "key/kp6", */
/*       "key/kp7",                   "key/kp8", */
/*       "key/kp9",                   "key/kp_period", */
/*       "key/kp_divide",             "key/kp_multiply", */
/*       "key/kp_minus",              "key/kp_plus", */
/*       "key/kp_enter",              "key/kp_equals", */
/*       "key/up",                    "key/down", */
/*       "key/right",                 "key/left", */
/*       "key/insert",                "key/home", */
/*       "key/end",                   "key/pageup", */
/*       "key/pagedown",              "key/f1", */
/*       "key/f2",                    "key/f3", */
/*       "key/f4",                    "key/f5", */
/*       "key/f6",                    "key/f7", */
/*       "key/f8",                    "key/f9", */
/*       "key/f10",                   "key/f11", */
/*       "key/f12",                   "key/f13", */
/*       "key/f14",                   "key/f15", */
/*       "key/numlock",               "key/capslock", */
/*       "key/scrollock",             "key/rshift", */
/*       "key/lshift",                "key/rctrl", */
/*       "key/lctrl",                 "key/ralt", */
/*       "key/lalt",                  "key/rmeta", */
/*       "key/lmeta",                 "key/lsuper", */
/*       "key/rsuper",                "key/mode", */
/*       "key/help",                  "key/print", */
/*       "key/sysreq",                "key/break", */
/*       "key/menu",                  "key/power", */
/*       "key/euro", */
      /* modsyms */
/*       "mod/none",     "mod/lshift",     "mod/rshift", */
/*       "mod/lctrl",    "mod/rctrl",      "mod/lalt", */
/*       "mod/ralt",     "mod/lmeta",      "mod/rmeta", */
/*       "mod/num",      "mod/caps",       "mod/mode", */
/*       "mod/reserved", */
      /* smob getters */
      "event:type",                "event:active:gain", 
      "event:active:state",        "event:key:state", 
      "event:key:keysym:sym",      "event:key:keysym:mod", 
      "event:key:keysym:scancode", "event:key:keysym:unicode", 
      "event:motion:state",        "event:motion:x", 
      "event:motion:y",            "event:motion:xrel", 
      "event:motion:yrel",         "event:button:button", 
      "event:button:state",        "event:button:x", 
      "event:button:y",            "event:jaxis:which", 
      "event:jaxis:axis",          "event:jaxis:value", 
      "event:jbutton:which",       "event:jbutton:button", 
      "event:jbutton:state",       "event:jball:which", 
      "event:jball:ball",          "event:jball:xrel", 
      "event:jball:yrel",          "event:jhat:which", 
      "event:jhat:hat",            "event:jhat:value", 
      "event:resize:w",            "event:resize:h", 
      "event:user:code",           "event:user:data1", 
      "event:user:data2",          "keysym:scancode", 
      "keysym:sym",                "keysym:mod", 
      "keysym:unicode", 
      /* SDL functions */
      "make-event",  "make-keysym",  "poll-event",  "wait-event",
      "push-event",
      NULL);
}

