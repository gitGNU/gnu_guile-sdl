/* sdlrect.c --- SDL Rect functions for Guile
 *
 * 	Copyright (C) 2003,2004 Thien-Thi Nguyen
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
#include <SDL/SDL.h>
#include <SDL/SDL_image.h>

#include "config.h"
#include "argcheck.h"
#include "sdlsmobs.h"
#include "wholefns.h"


/* smob functions */

static
SCM
mark_rect (SCM rect)
{
  return rect;
}

static
size_t
free_rect (SCM rect)
{
  free (SMOBGET (rect, SDL_Rect *));
  /* return sizeof (SDL_Rect); */
  return 0;
}

static
int
print_rect (SCM smob, SCM port, scm_print_state *pstate)
{
  SDL_Rect *rect = SMOBGET (smob, SDL_Rect *);

  scm_puts           ("#<SDL-Rect ", port);
  scm_display (gh_int2scm (rect->w), port);
  scm_puts                     ("x", port);
  scm_display (gh_int2scm (rect->h), port);
  scm_puts                     ("+", port);
  scm_display (gh_int2scm (rect->x), port);
  scm_puts                     ("+", port);
  scm_display (gh_int2scm (rect->y), port);
  scm_puts                     (">", port);

  /* non-zero means success */
  return 1;
}


GH_DEFPROC (make_rect, "make-rect", 4, 0, 0,
            (SCM x, SCM y, SCM width, SCM height),
            "Return a rectangle object with location @var{x},@var{y}\n"
            "and dimensions @var{width} by @var{height}.")
{
#define FUNC_NAME s_make_rect
  SDL_Rect *rect;

  ASSERT_EXACT (x, ARGH1);
  ASSERT_EXACT (y, ARGH2);
  ASSERT_EXACT (width, ARGH3);
  ASSERT_EXACT (height, ARGH4);

  rect = (SDL_Rect *) scm_must_malloc (sizeof (SDL_Rect), FUNC_NAME);
  rect->x = gh_scm2long (x);
  rect->y = gh_scm2long (y);
  rect->w = gh_scm2ulong (width);
  rect->h = gh_scm2ulong (height);

  RETURN_NEW_RECT (rect);
#undef FUNC_NAME
}


/* rect getters and setters */

#define NUMBER_GETTER(f)                        \
  GSDL_NUMBER_GETTER ("rect:" #f,               \
                      rect_ ## f,               \
                      rect_tag, SDL_Rect *,     \
                      f)

#define NUMBER_SETTER(f)                        \
  GSDL_NUMBER_SETTER ("rect:set-" #f "!",       \
                      rect_set_ ## f,           \
                      rect_tag, SDL_Rect *,     \
                      f)

#define NUMBER_GETSET(f) \
  NUMBER_GETTER (f)      \
  NUMBER_SETTER (f)

NUMBER_GETSET(x)
NUMBER_GETSET(y)
NUMBER_GETSET(w)
NUMBER_GETSET(h)


void
gsdl_init_rect (void)
{
  rect_tag = scm_make_smob_type ("SDL-Rect", sizeof (SDL_Rect));
  scm_set_smob_mark  (rect_tag, mark_rect);
  scm_set_smob_free  (rect_tag, free_rect);
  scm_set_smob_print (rect_tag, print_rect);

#include "sdlrect.x"
}

/* sdlrect.c ends here */
