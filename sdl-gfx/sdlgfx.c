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
            (SCM s_surface, SCM s_x, SCM s_y, SCM s_color),
            "On @var{surface}, draw a point at location\n"
            "@var{x},@var{y} with color @var{color}.")
#define FUNC_NAME s_draw_point
{
  ASSERT_SURFACE (s_surface, ARGH1);
  ASSERT_EXACT (s_x, ARGH2);
  ASSERT_EXACT (s_y, ARGH3);
  ASSERT_EXACT (s_color, ARGH4);

  RETURN_INT
    (pixelColor (UNPACK_SURFACE (s_surface),
                 gh_scm2long (s_x), gh_scm2long (s_y),
                 gh_scm2ulong (s_color)));
}
#undef FUNC_NAME


GH_DEFPROC (draw_line, "draw-line", 6, 0, 0,
            (SCM s_surface, SCM s_x1, SCM s_y1,
             SCM s_x2, SCM s_y2, SCM s_color),
            "On @var{surface}, draw a line segment from\n"
            "@var{x1},@var{y1} to @var{x2},@var{y2},\n"
            "with color @var{color}.")
#define FUNC_NAME s_draw_line
{
  ASSERT_SURFACE (s_surface, ARGH1);
  ASSERT_EXACT (s_x1, ARGH2);
  ASSERT_EXACT (s_y1, ARGH3);
  ASSERT_EXACT (s_x2, ARGH4);
  ASSERT_EXACT (s_y2, ARGH5);
  ASSERT_EXACT (s_color, ARGH6);

  RETURN_INT
    (lineColor (UNPACK_SURFACE (s_surface),
                gh_scm2long (s_x1), gh_scm2long (s_y1),
                gh_scm2long (s_x2), gh_scm2long (s_y2),
                gh_scm2ulong (s_color)));
}
#undef FUNC_NAME


GH_DEFPROC (draw_aa_line, "draw-aa-line", 6, 0, 0,
            (SCM s_surface, SCM s_x1, SCM s_y1,
             SCM s_x2, SCM s_y2, SCM s_color),
            "On @var{surface}, draw an anti-aliased line segment from\n"
            "@var{x1},@var{y1} to @var{x2},@var{y2},\n"
            "with color @var{color}.")
#define FUNC_NAME s_draw_aa_line
{
  ASSERT_SURFACE (s_surface, ARGH1);
  ASSERT_EXACT (s_x1, ARGH2);
  ASSERT_EXACT (s_y1, ARGH3);
  ASSERT_EXACT (s_x2, ARGH4);
  ASSERT_EXACT (s_y2, ARGH5);
  ASSERT_EXACT (s_color, ARGH6);

  RETURN_INT
    (aalineColor (UNPACK_SURFACE (s_surface),
                  gh_scm2long (s_x1), gh_scm2long (s_y1),
                  gh_scm2long (s_x2), gh_scm2long (s_y2),
                  gh_scm2ulong (s_color)));
}
#undef FUNC_NAME


GH_DEFPROC (draw_rectangle, "draw-rectangle", 6, 1, 0,
            (SCM s_surface, SCM s_x1, SCM s_y1, SCM s_x2, SCM s_y2,
             SCM s_color, SCM s_fill),
            "On @var{surface}, draw rectangle with opposite points\n"
            "@var{x1},@var{y1} and @var{x2},@var{y2},\n"
            "with color @var{color}.\n"
            "Optional arg @var{fill} means to fill the rectangle as well.")
#define FUNC_NAME s_draw_rectangle
{
  ASSERT_SURFACE (s_surface, ARGH1);
  ASSERT_EXACT (s_x1, ARGH2);
  ASSERT_EXACT (s_y1, ARGH3);
  ASSERT_EXACT (s_x2, ARGH4);
  ASSERT_EXACT (s_y2, ARGH5);
  ASSERT_EXACT (s_color, ARGH6);
  UNBOUND_MEANS_FALSE (s_fill);

  RETURN_INT
    ((EXACTLY_FALSEP (s_fill)
      ? rectangleColor
      : boxColor) (UNPACK_SURFACE (s_surface),
                   gh_scm2long (s_x1), gh_scm2long (s_y1),
                   gh_scm2long (s_x2), gh_scm2long (s_y2),
                   gh_scm2ulong (s_color)));
}
#undef FUNC_NAME


GH_DEFPROC (draw_circle, "draw-circle", 5, 1, 0,
            (SCM s_surface, SCM s_x, SCM s_y, SCM s_r,
             SCM s_color, SCM s_fill),
            "On @var{surface}, draw circle with center @var{x},@var{y}\n"
            "and radius @var{r}, with color @var{color}.\n"
            "Optional arg @var{fill} means to fill the circle as well.")
#define FUNC_NAME s_draw_circle
{
  ASSERT_SURFACE (s_surface, ARGH1);
  ASSERT_EXACT (s_x, ARGH2);
  ASSERT_EXACT (s_y, ARGH3);
  ASSERT_EXACT (s_r, ARGH4);
  ASSERT_EXACT (s_color, ARGH5);
  UNBOUND_MEANS_FALSE (s_fill);

  RETURN_INT
    ((EXACTLY_FALSEP (s_fill)
      ? circleColor
      : filledCircleColor) (UNPACK_SURFACE (s_surface),
                            gh_scm2long (s_x), gh_scm2long (s_y),
                            gh_scm2long (s_r),
                            gh_scm2ulong (s_color)));
}
#undef FUNC_NAME


GH_DEFPROC (draw_ellipse, "draw-ellipse", 6, 1, 0,
            (SCM s_surface, SCM s_x, SCM s_y, SCM s_rx, SCM s_ry,
             SCM s_color, SCM s_fill),
            "On @var{surface}, draw ellipse with center @var{x},@var{y}\n"
            "x-radius @var{rx}, y-radius @var{ry}, with color @var{color}.\n"
            "Optional arg @var{fill} means to fill the ellipse as well.")
#define FUNC_NAME s_draw_ellipse
{
  ASSERT_SURFACE (s_surface, ARGH1);
  ASSERT_EXACT (s_x, ARGH2);
  ASSERT_EXACT (s_y, ARGH3);
  ASSERT_EXACT (s_rx, ARGH4);
  ASSERT_EXACT (s_ry, ARGH5);
  ASSERT_EXACT (s_color, ARGH6);
  UNBOUND_MEANS_FALSE (s_fill);

  RETURN_INT
    ((EXACTLY_FALSEP (s_fill)
      ? ellipseColor
      : filledEllipseColor) (UNPACK_SURFACE (s_surface),
                             gh_scm2long (s_x), gh_scm2long (s_y),
                             gh_scm2long (s_rx), gh_scm2long (s_ry),
                             gh_scm2ulong (s_color)));
}
#undef FUNC_NAME


GH_DEFPROC (draw_polygon, "draw-polygon", 4, 1, 0,
            (SCM s_surface, SCM s_vx, SCM s_vy, SCM s_color, SCM s_fill),
            "On @var{surface}, draw a polygon whose points are specified\n"
            "by corresponding pairs from the uniform vectors\n"
            "@var{vx} and @var{vy}, in color @var{color}.  Optional\n"
            "arg @var{fill} means to fill the polygon as well.")
#define FUNC_NAME s_draw_polygon
{
  int ret;
  Sint16 *vx, *vy;

  ASSERT_SURFACE (s_surface, ARGH1);
  ASSERT_VECTOR (s_vx, ARGH2);
  ASSERT_VECTOR (s_vy, ARGH3);
  ASSERT_EXACT (s_color, ARGH4);
  UNBOUND_MEANS_FALSE (s_fill);

  vx = (Sint16 *) gh_scm2shorts (s_vx, NULL);
  vy = (Sint16 *) gh_scm2shorts (s_vy, NULL);

  ret = (EXACTLY_FALSEP (s_fill)
         ? polygonColor
         : filledPolygonColor) (UNPACK_SURFACE (s_surface),
                                vx, vy,
                                gh_uniform_vector_length (s_vx),
                                gh_scm2ulong (s_color));
  free (vx);
  free (vy);
  RETURN_INT (ret);
}
#undef FUNC_NAME


GH_DEFPROC (draw_character, "draw-character", 5, 0, 0,
            (SCM s_surface, SCM s_x, SCM s_y, SCM s_char, SCM s_color),
            "On @var{surface} at position @var{x},@var{y},\n"
            "draw char @var{c} with @var{color} (a number).")
#define FUNC_NAME s_draw_character
{
  ASSERT_SURFACE (s_surface, ARGH1);
  ASSERT_EXACT (s_x, ARGH2);
  ASSERT_EXACT (s_y, ARGH3);
  ASSERT_CHAR (s_char, ARGH4);
  ASSERT_EXACT (s_color, ARGH5);

  RETURN_INT
    (characterColor (UNPACK_SURFACE (s_surface),
                     gh_scm2long (s_x), gh_scm2long (s_y),
                     gh_scm2char (s_char), gh_scm2ulong (s_color)));
}
#undef FUNC_NAME


GH_DEFPROC (draw_string, "draw-string", 5, 0, 0,
            (SCM s_surface, SCM s_x, SCM s_y, SCM s_string, SCM s_color),
            "On @var{surface} at position @var{x},@var{y},\n"
            "draw string @var{text} with @var{color} (a number).")
#define FUNC_NAME s_draw_string
{
  ASSERT_SURFACE (s_surface, ARGH1);
  ASSERT_EXACT (s_x, ARGH2);
  ASSERT_EXACT (s_y, ARGH3);
  ASSERT_STRING (s_string, ARGH4);
  ASSERT_EXACT (s_color, ARGH5);

  RETURN_INT
    (stringColor (UNPACK_SURFACE (s_surface),
                  gh_scm2long (s_x), gh_scm2long (s_y),
                  SCM_CHARS (s_string),
                  gh_scm2ulong (s_color)));
}
#undef FUNC_NAME



/* dispatch */

static
void
init_module (void)
{
#include "sdlgfx.x"
}

GH_MODULE_LINK_FUNC ("sdl gfx", sdl_gfx, init_module)

/* sdlgfx.c ends here */
