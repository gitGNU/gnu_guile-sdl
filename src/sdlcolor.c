/* sdlcolor.c --- SDL Color functions for Guile
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

#include "config.h"
#include "argcheck.h"
#include "sdlsmobs.h"
#include "wholefns.h"

static
SCM
mark_color (SCM color)
{
  return color;
}

static
size_t
free_color (SCM color)
{
  free (UNPACK_COLOR (color));
  /* return sizeof (SDL_Color); */
  return 0;
}

static
int
print_color (SCM color, SCM port, scm_print_state *pstate)
{
  SDL_Color *ccolor = UNPACK_COLOR (color);

  scm_puts      ("#<SDL-Color ", port);
  scm_intprint                          (ccolor->r, 10, port);
  scm_putc      (' ', port);
  scm_intprint                          (ccolor->g, 10, port);
  scm_putc      (' ', port);
  scm_intprint                          (ccolor->b, 10, port);
  scm_putc      ('>', port);

  /* Non-zero means success.  */
  return 1;
}


/* Functions */

GH_DEFPROC (make_color, "make-color", 3, 0, 0,
            (SCM r, SCM g, SCM b),
            "Return a color object with @var{r}, @var{g},\n"
            "and @var{b} components.")
{
#define FUNC_NAME s_make_color
  SDL_Color *color;

  ASSERT_EXACT (r, ARGH1);
  ASSERT_EXACT (g, ARGH2);
  ASSERT_EXACT (b, ARGH3);

  color = (SDL_Color *) scm_must_malloc (sizeof (SDL_Color), FUNC_NAME);
  color->r = gh_scm2int (r);
  color->g = gh_scm2int (g);
  color->b = gh_scm2int (b);

  RETURN_NEW_COLOR (color);
#undef FUNC_NAME
}


/* Color getters and setters */

#define NUMBER_GETTER(f)                        \
  GSDL_NUMBER_GETTER ("color:" #f,              \
                      color_ ## f,              \
                      color_tag, SDL_Color *,   \
                      f)

#define NUMBER_SETTER(f)                        \
  GSDL_NUMBER_SETTER ("color:set-" #f "!",      \
                      color_set_ ## f,          \
                      color_tag, SDL_Color *,   \
                      f, gh_scm2ulong)

#define NUMBER_GETSET(f) \
  NUMBER_GETTER (f)      \
  NUMBER_SETTER (f)

NUMBER_GETSET(r)
NUMBER_GETSET(g)
NUMBER_GETSET(b)


void
gsdl_init_color (void)
{
  color_tag = scm_make_smob_type ("SDL-Color", sizeof (SDL_Color));
  scm_set_smob_mark  (color_tag, mark_color);
  scm_set_smob_free  (color_tag, free_color);
  scm_set_smob_print (color_tag, print_color);

#include "sdlcolor.x"
}

/* sdlcolor.c ends here */
