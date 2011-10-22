/* sdlrect.c --- SDL Rect functions for Guile
 *
 * Copyright (C) 2003, 2004, 2005, 2007, 2009, 2011 Thien-Thi Nguyen
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
#include <stdio.h>
#include <SDL/SDL.h>
#include <SDL/SDL_image.h>

/* smob functions */

static
size_t
free_rect (SCM rect)
{
  free (UNPACK_RECT (rect));
  return sizeof (SDL_Rect);
}

static
int
print_rect (SCM smob, SCM port, scm_print_state *pstate)
{
  SDL_Rect *rect = SMOBGET (smob, SDL_Rect *);
  char buf[64];

  snprintf (buf, 64, "#<SDL-Rect %ux%u%+d%+d>",
            rect->w, rect->h, rect->x, rect->y);
  scm_puts (buf, port);
  return 1;
}


PRIMPROC
(rect_p, "rect?", 1, 0, 0,
 (SCM obj),
 doc: /***********
Return #t iff @var{obj} is an SDL-rectangle object.  */)
{
#define FUNC_NAME s_rect_p
  RETURN_BOOL
    (SCM_SMOB_PREDICATE (rect_tag, obj));
#undef FUNC_NAME
}

PRIMPROC
(make_rect, "make-rect", 4, 0, 0,
 (SCM x, SCM y, SCM width, SCM height),
 doc: /***********
Return a rectangle object with location @var{x},@var{y}
and dimensions @var{width} by @var{height}.  */)
{
#define FUNC_NAME s_make_rect
  SDL_Rect *rect;

  ASSERT_EXACT (x, 1);
  ASSERT_EXACT (y, 2);
  ASSERT_EXACT (width, 3);
  ASSERT_EXACT (height, 4);

  if ((rect = (SDL_Rect *) scm_must_malloc (sizeof (SDL_Rect), FUNC_NAME)))
    {
      rect->x = C_LONG (x);
      rect->y = C_LONG (y);
      rect->w = C_ULONG (width);
      rect->h = C_ULONG (height);
    }

  RETURN_NEW_RECT (rect);
#undef FUNC_NAME
}


/* rect getters and setters */

#define NUMBER_GETTER(f)                        \
  GSDL_NUMBER_GETTER ("rect:" #f,               \
                      rect_ ## f,               \
                      rect_tag, SDL_Rect *,     \
                      f)

#define NUMBER_SETTER(f,conv)                   \
  GSDL_NUMBER_SETTER ("rect:set-" #f "!",       \
                      rect_set_ ## f,           \
                      rect_tag, SDL_Rect *,     \
                      f, conv)

#define NUMBER_GETSET(f,c2s)                    \
  NUMBER_GETTER (f)                             \
  NUMBER_SETTER (f, c2s)

NUMBER_GETSET (x, C_INT)
NUMBER_GETSET (y, C_INT)
NUMBER_GETSET (w, C_ULONG)
NUMBER_GETSET (h, C_ULONG)


void
gsdl_init_rect (void)
{
  rect_tag = scm_make_smob_type ("SDL-Rect", sizeof (SDL_Rect));
  scm_set_smob_free  (rect_tag, free_rect);
  scm_set_smob_print (rect_tag, print_rect);

#include "sdlrect.x"
}

/* sdlrect.c ends here */
