/* misc.c --- Miscellaneous SDL functions for Guile
 *
 *	Copyright (C) 2005 Thien-Thi Nguyen
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

#include <guile/gh.h>
#include <SDL/SDL.h>
#include <SDL/SDL_active.h>
#include <SDL/SDL_syswm.h>

#include "config.h"
#include "sym.h"


DECLARE_SYM(mousefocus, "mousefocus");
DECLARE_SYM(inputfocus, "inputfocus");
DECLARE_SYM(active, "active");

GH_DEFPROC (get_app_state, "get-app-state", 0, 0, 0,
            (void),
            "Return the current state of the application, a list of symbols.\n"
            "The list may include: `mousefocus', `inputfocus', `active'.")
{
  Uint8 state = SDL_GetAppState ();
  SCM rv = SCM_EOL;

  if (state & SDL_APPMOUSEFOCUS) rv = gh_cons (SYM (mousefocus), rv);
  if (state & SDL_APPINPUTFOCUS) rv = gh_cons (SYM (inputfocus), rv);
  if (state & SDL_APPACTIVE)     rv = gh_cons (SYM (active), rv);

  return rv;
}


DECLARE_SYM(x11, "x11");

GH_DEFPROC (get_wm_info, "get-wm-info", 0, 0, 0,
            (void),
            "Return information on the window manager, as a list of the\n"
            "form: (VERSION SUBSYSTEM DISPLAY WINDOW FSWINDOW WMWINDOW).\n"
            "VERSION is a sub-list of form: (MAJOR MINOR PATCH), where\n"
            "element is an integer.  SUBSYSTEM is either the symbol\n"
            "@code{x11}, or #f.  DISPLAY is a pointer (machine address)\n"
            "of the X11 Display structure, converted to an integer.\n"
            "WINDOW, FSWINDOW and WMWINDOW are Window identifiers (also\n"
            "integers).")
{
#define FUNC_NAME s_get_wm_info
  SDL_SysWMinfo *info = (SDL_SysWMinfo *) malloc (sizeof (SDL_SysWMinfo));
  int result = SDL_GetWMInfo (info);
  SCM rv = SCM_BOOL_F;

  if (result)
    {
#define PUSH(x)  rv = gh_cons (x, rv)
      rv = SCM_EOL;
      PUSH (gh_ulong2scm (info->info.x11.wmwindow));
      PUSH (gh_ulong2scm (info->info.x11.fswindow));
      PUSH (gh_ulong2scm (info->info.x11.window));
      PUSH (gh_ulong2scm ((unsigned long) info->info.x11.display));
      PUSH (SDL_SYSWM_X11 == info->subsystem ? SYM (x11) : SCM_BOOL_F);
      PUSH (gh_list (gh_int2scm (info->version.major),
                     gh_int2scm (info->version.minor),
                     gh_int2scm (info->version.patch),
                     SCM_UNDEFINED));
#undef PUSH
    }

  free (info);
  return rv;
#undef FUNC_NAME
}


void
gsdl_init_misc (void)
{
#include "misc.x"
}

/* misc.c ends here */
