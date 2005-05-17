/* misc.c --- Miscellaneous SDL functions for Guile
 *
 *	Copyright (C) 2005 Thien-Thi Nguyen
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
#include <SDL/SDL.h>
#include <SDL/SDL_active.h>

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


void
gsdl_init_misc (void)
{
#include "misc.x"
}

/* misc.c ends here */
