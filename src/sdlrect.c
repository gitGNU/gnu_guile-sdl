/* sdlrect.c --- SDL Rect functions for Guile
 *
 * 	Copyright (C) 2003,2004,2005 Thien-Thi Nguyen
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
#include <SDL/SDL_image.h>

#include "config.h"
#include "argcheck.h"
#include "sdlsmobs.h"
#include "wholefns.h"


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

  scm_puts      ("#<SDL-Rect ", port);
  scm_intprint                          (rect->w, 10, port);
  scm_putc      ('x', port);
  scm_intprint                          (rect->h, 10, port);
  scm_putc      ('+', port);
  scm_intprint                          (rect->x, 10, port);
  scm_putc      ('+', port);
  scm_intprint                          (rect->y, 10, port);
  scm_putc      ('>', port);

  /* non-zero means success */
  return 1;
}


GH_DEFPROC (rect_p, "rect?", 1, 0, 0,
            (SCM obj),
            "Return #t iff @var{obj} is an SDL-rectangle object.")
{
#define FUNC_NAME s_rect_p
  RETURN_BOOL
    (SCM_SMOB_PREDICATE (rect_tag, obj));
#undef FUNC_NAME
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

  if ((rect = (SDL_Rect *) scm_must_malloc (sizeof (SDL_Rect), FUNC_NAME)))
    {
      rect->x = gh_scm2long (x);
      rect->y = gh_scm2long (y);
      rect->w = gh_scm2ulong (width);
      rect->h = gh_scm2ulong (height);
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

NUMBER_GETSET (x, gh_scm2int)
NUMBER_GETSET (y, gh_scm2int)
NUMBER_GETSET (w, gh_scm2ulong)
NUMBER_GETSET (h, gh_scm2ulong)


void
gsdl_init_rect (void)
{
  rect_tag = scm_make_smob_type ("SDL-Rect", sizeof (SDL_Rect));
  scm_set_smob_free  (rect_tag, free_rect);
  scm_set_smob_print (rect_tag, print_rect);

#include "sdlrect.x"
}

/* sdlrect.c ends here */
