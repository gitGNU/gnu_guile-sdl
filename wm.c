/*******************************************************************
 *  wm.c -- SDL Window Manager functions for Guile                 *
 *                                                                 *
 *  Created:    <2001-06-10 21:42:23 foof>                         *
 *  Time-stamp: <2001-06-18 01:09:34 foof>                         *
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

#include "wm.h"
#include "video.h"

SCM
wm_set_caption (SCM title, SCM icon)
{
   SCM_ASSERT ((SCM_NIMP (title) && SCM_STRINGP (title)),
               title, SCM_ARG1, "sdl-set-caption");

   SCM_ASSERT ((SCM_NIMP (icon) && SCM_STRINGP (icon)),
               icon, SCM_ARG1, "sdl-set-caption");

   SDL_WM_SetCaption (SCM_CHARS (title), SCM_CHARS (icon));

   return SCM_UNSPECIFIED;
}

SCM
wm_get_caption (void)
{
   char *title;
   char *icon;

   SDL_WM_GetCaption (&title, &icon);

   return SCM_LIST2 (scm_makfrom0str (title), scm_makfrom0str (icon));
}

SCM
wm_set_icon (SCM icon)
{
   SDL_Surface *surface;

   SCM_ASSERT_SMOB (icon, surface_tag, SCM_ARG1, "sdl-set-icon");
   surface = (SDL_Surface*) SCM_SMOB_DATA (icon);

   /* set w/ a NULL mask for now */
   SDL_WM_SetIcon (surface, NULL);

   return SCM_UNSPECIFIED;
}

SCM
wm_iconify_window (void)
{
   return SDL_WM_IconifyWindow () ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM
wm_toggle_full_screen (SCM s_surface)
{
   SDL_Surface *surface;

   if (s_surface == SCM_UNDEFINED) {
      surface = SDL_GetVideoSurface ();
   } else {
      SCM_ASSERT_SMOB (s_surface, surface_tag, SCM_ARG1, "sdl-toggle-full-screen");
      surface = (SDL_Surface*) SCM_SMOB_DATA (s_surface);
   }

   return SDL_WM_ToggleFullScreen (surface) ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM
wm_grab_input (SCM s_mode)
{
   int mode = SDL_GRAB_QUERY;

   if (s_mode != SCM_UNDEFINED) {
      SCM_ASSERT (scm_exact_p (s_mode), s_mode, SCM_ARG1, "sdl-grab-input");
      mode = scm_num2long (s_mode, SCM_ARG1, "scm_num2long");
   }

   return scm_long2num (SDL_WM_GrabInput (mode));
}

void
sdl_wm_init (void)
{
   /* functions */
   scm_c_define_gsubr ("sdl-set-caption",         2, 0, 0, wm_set_caption);
   scm_c_define_gsubr ("sdl-get-caption",         0, 0, 0, wm_get_caption);
   scm_c_define_gsubr ("sdl-set-icon",            1, 0, 0, wm_set_icon);
   scm_c_define_gsubr ("sdl-iconify-window",      0, 0, 0, wm_iconify_window);
   scm_c_define_gsubr ("sdl-toggle-full-screen",  0, 1, 0, wm_toggle_full_screen);
   scm_c_define_gsubr ("sdl-grab-input",          0, 1, 0, wm_grab_input);
   
   /* exported symbols */
   scm_c_export (
      "sdl-set-caption",
      "sdl-get-caption",
      "sdl-set-icon",
      "sdl-iconify-window",
      "sdl-toggle-full-screen",
      "sdl-grab-input",
      NULL);
}

