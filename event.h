/*******************************************************************
 *  event.h -- SDL event handling for Guile                        *
 *                                                                 *
 *  Created:    <2001-05-27 13:43:48 foof>                         *
 *  Time-stamp: <2001-06-10 22:59:11 foof>                         *
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

#ifndef _GUILE_SDL_EVENT_H
#define _GUILE_SDL_EVENT_H

/* guile headers */
#include <libguile.h>
/* sdl headers */
#include <SDL/SDL.h>
#include <SDL/SDL_events.h>
#include <SDL/SDL_keysym.h>

/* tags for SDL smobs */
extern long event_tag;
extern long keysym_tag;

/* constructors */
SCM make_event (SCM s_event_type);
/* SCM make_keysym (SCM s_sym, SCM s_mod); */
scm_sizet free_event (SCM event);

/* event getters */
SCM event_type (SCM s_event);
SCM event_active_gain (SCM s_event);
SCM event_active_state (SCM s_event);
SCM event_key_state (SCM s_event);
/* SCM event_key_keysym (SCM s_event); */
SCM event_key_keysym_sym (SCM s_event);
SCM event_key_keysym_mod (SCM s_event);
SCM event_key_keysym_scancode (SCM s_event);
SCM event_key_keysym_unicode (SCM s_event);
SCM event_motion_state (SCM s_event);
SCM event_motion_x (SCM s_event);
SCM event_motion_y (SCM s_event);
SCM event_motion_xrel (SCM s_event);
SCM event_motion_yrel (SCM s_event);
SCM event_button_button (SCM s_event);
SCM event_button_state (SCM s_event);
SCM event_button_x (SCM s_event);
SCM event_button_y (SCM s_event);
SCM event_jaxis_which (SCM s_event);
SCM event_jaxis_axis (SCM s_event);
SCM event_jaxis_value (SCM s_event);
SCM event_jball_which (SCM s_event);
SCM event_jball_ball (SCM s_event);
SCM event_jball_xrel (SCM s_event);
SCM event_jball_yrel (SCM s_event);
SCM event_jhat_which (SCM s_event);
SCM event_jhat_hat (SCM s_event);
SCM event_jhat_value (SCM s_event);
SCM event_jbutton_which (SCM s_event);
SCM event_jbutton_button (SCM s_event);
SCM event_jbutton_state (SCM s_event);
SCM event_resize_w (SCM s_event);
SCM event_resize_h (SCM s_event);
/* SCM event_quit (SCM s_event); */
SCM event_user_code (SCM s_event);
/* SCM event_user_data1 (SCM s_event); */
/* SCM event_user_data2 (SCM s_event); */
/* SCM event_syswm (SCM s_event); */

/* keysym getters */
/* SCM keysym_scancode (SCM s_key); */
/* SCM keysym_sym (SCM s_key); */
/* SCM keysym_mod (SCM s_key); */
/* SCM keysym_unicode (SCM s_key); */

/* SDL event functions */

/* extern DECLSPEC void SDL_PumpEvents(void); */
SCM pump_events (void);

/* extern DECLSPEC int SDL_PeepEvents(SDL_Event *events, int numevents, */
/* 				SDL_eventaction action, Uint32 mask); */
SCM peep_events (SCM events, SCM numevents, SCM action, SCM mask);

/* extern DECLSPEC int SDL_PollEvent(SDL_Event *event); */
SCM poll_event (SCM event);

/* extern DECLSPEC int SDL_WaitEvent(SDL_Event *event); */
SCM wait_event (SCM event);

/* extern DECLSPEC int SDL_PushEvent(SDL_Event *event); */
SCM push_event (SCM event);

/* extern DECLSPEC void SDL_SetEventFilter(SDL_EventFilter filter); */
SCM set_event_filter (SCM filter);

/* extern DECLSPEC SDL_EventFilter SDL_GetEventFilter(void); */
SCM get_event_filter (SCM filter);

/* extern DECLSPEC Uint8 SDL_EventState(Uint8 type, int state); */
SCM event_state (SCM type, SCM state);


/* Initialize glue */
void sdl_event_init (void);

#endif /* ! _GUILE_SDL_EVENT_H */

