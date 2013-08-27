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
#include "SDL_image.h"

/* smob functions */

static
size_t
free_rect (SCM rect)
{
  SDL_Rect *crect = UNPACK_RECT (rect);

  GCFREE (crect, rect_nick);
  return GCRV (crect);
}

static
int
print_rect (SCM smob, SCM port, UNUSED scm_print_state *pstate)
{
  SDL_Rect *rect = UNPACK_RECT (smob);
  char buf[64];

  snprintf (buf, 64, "#<%s %ux%u%+d%+d>", rect_nick,
            rect->w, rect->h, rect->x, rect->y);
  scm_puts (buf, port);
  return 1;
}


PRIMPROC
(rect_p, "rect?", 1, 0, 0,
 (SCM obj),
 doc: /***********
Return @code{#t} iff @var{obj} is an SDL-rectangle object.  */)
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

  ASSERT_INTEGER (x, 1);
  ASSERT_INTEGER (y, 2);
  ASSERT_INTEGER (width, 3);
  ASSERT_INTEGER (height, 4);

  if ((rect = GCMALLOC_RECT ()))
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
                      rect, Rect,               \
                      f)

#define NUMBER_SETTER(f,conv)                   \
  GSDL_NUMBER_SETTER ("rect:set-" #f "!",       \
                      rect_set_ ## f,           \
                      rect, Rect,               \
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
  DEFSMOB (rect_tag, rect_nick,
           NULL,
           free_rect,
           print_rect);

#include "sdlrect.x"
}

/* sdlrect.c ends here */
