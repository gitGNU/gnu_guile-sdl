/* sdlcolor.c --- SDL Color functions for Guile
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

static
size_t
free_color (SCM color)
{
  SDL_Color *ccolor = UNPACK_COLOR (color);

  GCFREE (ccolor, color_nick);
  return GCRV (ccolor);
}

static
int
print_color (SCM color, SCM port, UNUSED scm_print_state *pstate)
{
  SDL_Color *ccolor = UNPACK_COLOR (color);
  char buf[32];

  snprintf (buf, 32, "#<%s %d %d %d>", color_nick,
            ccolor->r, ccolor->g, ccolor->b);
  scm_puts (buf, port);
  return 1;
}


/* Functions */

PRIMPROC
(color_p, "color?", 1, 0, 0,
 (SCM obj),
 doc: /***********
Return @code{#t} iff @var{obj} is an SDL-Color object.  */)
{
#define FUNC_NAME s_color_p
  RETURN_BOOL
    (SCM_SMOB_PREDICATE (color_tag, obj));
#undef FUNC_NAME
}

PRIMPROC
(make_color, "make-color", 3, 0, 0,
 (SCM r, SCM g, SCM b),
 doc: /***********
Return a color object with @var{r}, @var{g},
and @var{b} components.  */)
{
#define FUNC_NAME s_make_color
  SDL_Color *color;

  ASSERT_INTEGER (r, 1);
  ASSERT_INTEGER (g, 2);
  ASSERT_INTEGER (b, 3);

  if ((color = GCMALLOC (sizeof (SDL_Color), color_nick)))
    {
      color->r = C_INT (r);
      color->g = C_INT (g);
      color->b = C_INT (b);
    }

  RETURN_NEW_COLOR (color);
#undef FUNC_NAME
}


/* Color getters and setters */

#define NUMBER_GETTER(f)                        \
  GSDL_NUMBER_GETTER ("color:" #f,              \
                      color_ ## f,              \
                      color, Color,             \
                      f)

#define NUMBER_SETTER(f)                        \
  GSDL_NUMBER_SETTER ("color:set-" #f "!",      \
                      color_set_ ## f,          \
                      color, Color,             \
                      f, C_ULONG)

#define NUMBER_GETSET(f) \
  NUMBER_GETTER (f)      \
  NUMBER_SETTER (f)

NUMBER_GETSET(r)
NUMBER_GETSET(g)
NUMBER_GETSET(b)


void
gsdl_init_color (void)
{
  DEFSMOB (color_tag, color_nick,
           NULL,
           free_color,
           print_color);

#include "sdlcolor.x"
}

/* sdlcolor.c ends here */
