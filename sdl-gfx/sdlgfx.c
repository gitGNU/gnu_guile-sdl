/* sdlgfx.c --- Additional Graphics functions for Guile SDL
 *
 * 	Copyright (C) 2003,2004 Thien-Thi Nguyen
 * 	Copyright (C) 2001 Alex Shinn
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

#include "SDL_gfxPrimitives.h"

#include "config.h"
#include "argcheck.h"
#include "sdlsmobs.h"
#include "retval.h"
#include "bool.h"

GH_USE_MODULE (sdlsup, "(sdl sdl)"); /* for various gsdl_* C funcs */


GH_DEFPROC (draw_point, "draw-point", 4, 0, 0,
            (SCM surface, SCM x, SCM y, SCM color),
            "On @var{surface}, draw a point at location\n"
            "@var{x},@var{y} with color @var{color}.")
{
#define FUNC_NAME s_draw_point
  ASSERT_SURFACE (surface, ARGH1);
  ASSERT_EXACT (x, ARGH2);
  ASSERT_EXACT (y, ARGH3);
  ASSERT_EXACT (color, ARGH4);

  RETURN_INT
    (pixelColor (UNPACK_SURFACE (surface),
                 gh_scm2long (x), gh_scm2long (y),
                 gh_scm2ulong (color)));
#undef FUNC_NAME
}


GH_DEFPROC (draw_line, "draw-line", 6, 0, 0,
            (SCM surface, SCM x1, SCM y1,
             SCM x2, SCM y2, SCM color),
            "On @var{surface}, draw a line segment from\n"
            "@var{x1},@var{y1} to @var{x2},@var{y2},\n"
            "with color @var{color}.")
{
#define FUNC_NAME s_draw_line
  ASSERT_SURFACE (surface, ARGH1);
  ASSERT_EXACT (x1, ARGH2);
  ASSERT_EXACT (y1, ARGH3);
  ASSERT_EXACT (x2, ARGH4);
  ASSERT_EXACT (y2, ARGH5);
  ASSERT_EXACT (color, ARGH6);

  RETURN_INT
    (lineColor (UNPACK_SURFACE (surface),
                gh_scm2long (x1), gh_scm2long (y1),
                gh_scm2long (x2), gh_scm2long (y2),
                gh_scm2ulong (color)));
#undef FUNC_NAME
}


GH_DEFPROC (draw_aa_line, "draw-aa-line", 6, 0, 0,
            (SCM surface, SCM x1, SCM y1,
             SCM x2, SCM y2, SCM color),
            "On @var{surface}, draw an anti-aliased line segment from\n"
            "@var{x1},@var{y1} to @var{x2},@var{y2},\n"
            "with color @var{color}.")
{
#define FUNC_NAME s_draw_aa_line
  ASSERT_SURFACE (surface, ARGH1);
  ASSERT_EXACT (x1, ARGH2);
  ASSERT_EXACT (y1, ARGH3);
  ASSERT_EXACT (x2, ARGH4);
  ASSERT_EXACT (y2, ARGH5);
  ASSERT_EXACT (color, ARGH6);

  RETURN_INT
    (aalineColor (UNPACK_SURFACE (surface),
                  gh_scm2long (x1), gh_scm2long (y1),
                  gh_scm2long (x2), gh_scm2long (y2),
                  gh_scm2ulong (color)));
#undef FUNC_NAME
}


GH_DEFPROC (draw_rectangle, "draw-rectangle", 6, 1, 0,
            (SCM surface, SCM x1, SCM y1, SCM x2, SCM y2,
             SCM color, SCM fill),
            "On @var{surface}, draw a rectangle with opposite points\n"
            "@var{x1},@var{y1} and @var{x2},@var{y2},\n"
            "with color @var{color}.\n"
            "Optional arg @var{fill} means to fill the rectangle as well.")
{
#define FUNC_NAME s_draw_rectangle
  ASSERT_SURFACE (surface, ARGH1);
  ASSERT_EXACT (x1, ARGH2);
  ASSERT_EXACT (y1, ARGH3);
  ASSERT_EXACT (x2, ARGH4);
  ASSERT_EXACT (y2, ARGH5);
  ASSERT_EXACT (color, ARGH6);
  UNBOUND_MEANS_FALSE (fill);

  RETURN_INT
    ((EXACTLY_FALSEP (fill)
      ? rectangleColor
      : boxColor) (UNPACK_SURFACE (surface),
                   gh_scm2long (x1), gh_scm2long (y1),
                   gh_scm2long (x2), gh_scm2long (y2),
                   gh_scm2ulong (color)));
#undef FUNC_NAME
}


GH_DEFPROC (draw_circle, "draw-circle", 5, 1, 0,
            (SCM surface, SCM x, SCM y, SCM r,
             SCM color, SCM fill),
            "On @var{surface}, draw a circle with center @var{x},@var{y}\n"
            "and radius @var{r}, with color @var{color}.\n"
            "Optional arg @var{fill} means to fill the circle as well.")
{
#define FUNC_NAME s_draw_circle
  ASSERT_SURFACE (surface, ARGH1);
  ASSERT_EXACT (x, ARGH2);
  ASSERT_EXACT (y, ARGH3);
  ASSERT_EXACT (r, ARGH4);
  ASSERT_EXACT (color, ARGH5);
  UNBOUND_MEANS_FALSE (fill);

  RETURN_INT
    ((EXACTLY_FALSEP (fill)
      ? circleColor
      : filledCircleColor) (UNPACK_SURFACE (surface),
                            gh_scm2long (x), gh_scm2long (y),
                            gh_scm2long (r),
                            gh_scm2ulong (color)));
#undef FUNC_NAME
}


GH_DEFPROC (draw_aa_circle, "draw-aa-circle", 5, 0, 0,
            (SCM surface, SCM x, SCM y, SCM r, SCM color),
            "On @var{surface}, draw an anti-aliased circle with center\n"
            "@var{x},@var{y} and radius @var{r}, with color @var{color}.")
{
#define FUNC_NAME s_draw_aa_circle
  ASSERT_SURFACE (surface, ARGH1);
  ASSERT_EXACT (x, ARGH2);
  ASSERT_EXACT (y, ARGH3);
  ASSERT_EXACT (r, ARGH4);
  ASSERT_EXACT (color, ARGH5);

  RETURN_INT
    (aacircleColor (UNPACK_SURFACE (surface),
                    gh_scm2long (x), gh_scm2long (y),
                    gh_scm2long (r),
                    gh_scm2ulong (color)));
#undef FUNC_NAME
}


GH_DEFPROC (draw_ellipse, "draw-ellipse", 6, 1, 0,
            (SCM surface, SCM x, SCM y, SCM rx, SCM ry,
             SCM color, SCM fill),
            "On @var{surface}, draw an ellipse with center @var{x},@var{y}\n"
            "x-radius @var{rx}, y-radius @var{ry}, with color @var{color}.\n"
            "Optional arg @var{fill} means to fill the ellipse as well.")
{
#define FUNC_NAME s_draw_ellipse
  ASSERT_SURFACE (surface, ARGH1);
  ASSERT_EXACT (x, ARGH2);
  ASSERT_EXACT (y, ARGH3);
  ASSERT_EXACT (rx, ARGH4);
  ASSERT_EXACT (ry, ARGH5);
  ASSERT_EXACT (color, ARGH6);
  UNBOUND_MEANS_FALSE (fill);

  RETURN_INT
    ((EXACTLY_FALSEP (fill)
      ? ellipseColor
      : filledEllipseColor) (UNPACK_SURFACE (surface),
                             gh_scm2long (x), gh_scm2long (y),
                             gh_scm2long (rx), gh_scm2long (ry),
                             gh_scm2ulong (color)));
#undef FUNC_NAME
}


GH_DEFPROC (draw_aa_ellipse, "draw-aa-ellipse", 6, 0, 0,
            (SCM surface, SCM x, SCM y, SCM rx, SCM ry, SCM color),
            "On @var{surface}, draw an anti-aliased ellipse with center\n"
            "@var{x},@var{y}, x-radius @var{rx}, y-radius @var{ry}, with\n"
            "color @var{color}.")
{
#define FUNC_NAME s_draw_aa_ellipse
  ASSERT_SURFACE (surface, ARGH1);
  ASSERT_EXACT (x, ARGH2);
  ASSERT_EXACT (y, ARGH3);
  ASSERT_EXACT (rx, ARGH4);
  ASSERT_EXACT (ry, ARGH5);
  ASSERT_EXACT (color, ARGH6);

  RETURN_INT
    (aaellipseColor (UNPACK_SURFACE (surface),
                     gh_scm2long (x), gh_scm2long (y),
                     gh_scm2long (rx), gh_scm2long (ry),
                     gh_scm2ulong (color)));
#undef FUNC_NAME
}


GH_DEFPROC (draw_polygon, "draw-polygon", 4, 1, 0,
            (SCM surface, SCM vx, SCM vy, SCM color, SCM fill),
            "On @var{surface}, draw a polygon whose points are specified\n"
            "by corresponding pairs from the uniform vectors\n"
            "@var{vx} and @var{vy}, in color @var{color}.  Optional\n"
            "arg @var{fill} means to fill the polygon as well.")
{
#define FUNC_NAME s_draw_polygon
  int ret;
  Sint16 *cvx, *cvy;

  ASSERT_SURFACE (surface, ARGH1);
  ASSERT_VECTOR (vx, ARGH2);
  ASSERT_VECTOR (vy, ARGH3);
  ASSERT_EXACT (color, ARGH4);
  UNBOUND_MEANS_FALSE (fill);

  cvx = (Sint16 *) gh_scm2shorts (vx, NULL);
  cvy = (Sint16 *) gh_scm2shorts (vy, NULL);

  ret = (EXACTLY_FALSEP (fill)
         ? polygonColor
         : filledPolygonColor) (UNPACK_SURFACE (surface),
                                cvx, cvy,
                                gh_uniform_vector_length (vx),
                                gh_scm2ulong (color));
  free (cvx);
  free (cvy);
  RETURN_INT (ret);
#undef FUNC_NAME
}


GH_DEFPROC (draw_character, "draw-character", 5, 0, 0,
            (SCM surface, SCM x, SCM y, SCM c, SCM color),
            "On @var{surface} at position @var{x},@var{y},\n"
            "draw char @var{c} with @var{color} (a number).")
{
#define FUNC_NAME s_draw_character
  ASSERT_SURFACE (surface, ARGH1);
  ASSERT_EXACT (x, ARGH2);
  ASSERT_EXACT (y, ARGH3);
  ASSERT_CHAR (c, ARGH4);
  ASSERT_EXACT (color, ARGH5);

  RETURN_INT
    (characterColor (UNPACK_SURFACE (surface),
                     gh_scm2long (x), gh_scm2long (y),
                     gh_scm2char (c), gh_scm2ulong (color)));
#undef FUNC_NAME
}


GH_DEFPROC (draw_string, "draw-string", 5, 0, 0,
            (SCM surface, SCM x, SCM y, SCM text, SCM color),
            "On @var{surface} at position @var{x},@var{y},\n"
            "draw string @var{text} with @var{color} (a number).")
{
#define FUNC_NAME s_draw_string
  ASSERT_SURFACE (surface, ARGH1);
  ASSERT_EXACT (x, ARGH2);
  ASSERT_EXACT (y, ARGH3);
  ASSERT_STRING (text, ARGH4);
  ASSERT_EXACT (color, ARGH5);

  RETURN_INT
    (stringColor (UNPACK_SURFACE (surface),
                  gh_scm2long (x), gh_scm2long (y),
                  SCM_CHARS (text),
                  gh_scm2ulong (color)));
#undef FUNC_NAME
}



/* dispatch */

static
void
init_module (void)
{
#include "sdlgfx.x"
}

GH_MODULE_LINK_FUNC ("sdl gfx", sdl_gfx, init_module)

/* sdlgfx.c ends here */
