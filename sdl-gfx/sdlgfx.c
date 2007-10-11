/* sdlgfx.c --- Additional Graphics functions for Guile SDL
 *
 * 	Copyright (C) 2003,2004,2005,2007 Thien-Thi Nguyen
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

#include "config.h"
#include "argcheck.h"
#include "sdlsmobs.h"
#include "retval.h"
#include "bool.h"

GH_USE_MODULE (sdlsup, "(sdl sdl)"); /* for various gsdl_* C funcs */


/*
 * gfxPrimitives
 */

#include "SDL_gfxPrimitives.h"


GH_DEFPROC
(draw_point, "draw-point", 4, 0, 0,
 (SCM surface, SCM x, SCM y, SCM color),
 doc: /***********
On @var{surface}, draw a point at location
@var{x},@var{y} with color @var{color}.  */)
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


GH_DEFPROC
(draw_hline, "draw-hline", 5, 0, 0,
 (SCM surface, SCM x1, SCM x2, SCM y, SCM color),
 doc: /***********
On @var{surface}, draw a horizontal line segment
from @var{x1},@var{y} to @var{x2},@var{y},
with color @var{color}.  */)
{
#define FUNC_NAME s_draw_hline
  ASSERT_SURFACE (surface, ARGH1);
  ASSERT_EXACT (x1, ARGH2);
  ASSERT_EXACT (x2, ARGH3);
  ASSERT_EXACT (y, ARGH4);
  ASSERT_EXACT (color, ARGH5);

  RETURN_INT
    (hlineColor (UNPACK_SURFACE (surface),
                 gh_scm2long (x1), gh_scm2long (x2), gh_scm2long (y),
                 gh_scm2ulong (color)));
#undef FUNC_NAME
}


GH_DEFPROC
(draw_vline, "draw-vline", 5, 0, 0,
 (SCM surface, SCM x, SCM y1, SCM y2, SCM color),
 doc: /***********
On @var{surface}, draw a vertical line segment
from @var{x},@var{y1} to @var{x},@var{y2},
with color @var{color}.  */)
{
#define FUNC_NAME s_draw_vline
  ASSERT_SURFACE (surface, ARGH1);
  ASSERT_EXACT (x, ARGH2);
  ASSERT_EXACT (y1, ARGH3);
  ASSERT_EXACT (y2, ARGH4);
  ASSERT_EXACT (color, ARGH5);

  RETURN_INT
    (vlineColor (UNPACK_SURFACE (surface),
                 gh_scm2long (x), gh_scm2long (y1), gh_scm2long (y2),
                 gh_scm2ulong (color)));
#undef FUNC_NAME
}


GH_DEFPROC
(draw_rectangle, "draw-rectangle", 6, 1, 0,
 (SCM surface, SCM x1, SCM y1, SCM x2, SCM y2,
  SCM color, SCM fill),
 doc: /***********
On @var{surface}, draw a rectangle with opposite points
@var{x1},@var{y1} and @var{x2},@var{y2},
with color @var{color}.
Optional arg @var{fill} means to fill the rectangle as well.  */)
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


GH_DEFPROC
(draw_line, "draw-line", 6, 0, 0,
 (SCM surface, SCM x1, SCM y1,
  SCM x2, SCM y2, SCM color),
 doc: /***********
On @var{surface}, draw a line segment from
@var{x1},@var{y1} to @var{x2},@var{y2},
with color @var{color}.  */)
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


GH_DEFPROC
(draw_aa_line, "draw-aa-line", 6, 0, 0,
 (SCM surface, SCM x1, SCM y1,
  SCM x2, SCM y2, SCM color),
 doc: /***********
On @var{surface}, draw an anti-aliased line segment from
@var{x1},@var{y1} to @var{x2},@var{y2},
with color @var{color}.  */)
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


GH_DEFPROC
(draw_circle, "draw-circle", 5, 1, 0,
 (SCM surface, SCM x, SCM y, SCM r,
  SCM color, SCM fill),
 doc: /***********
On @var{surface}, draw a circle with center @var{x},@var{y}
and radius @var{r}, with color @var{color}.
Optional arg @var{fill} means to fill the circle as well.  */)
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


GH_DEFPROC
(draw_aa_circle, "draw-aa-circle", 5, 0, 0,
 (SCM surface, SCM x, SCM y, SCM r, SCM color),
 doc: /***********
On @var{surface}, draw an anti-aliased circle with center
@var{x},@var{y} and radius @var{r}, with color @var{color}.  */)
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


GH_DEFPROC
(draw_ellipse, "draw-ellipse", 6, 1, 0,
 (SCM surface, SCM x, SCM y, SCM rx, SCM ry,
  SCM color, SCM fill),
 doc: /***********
On @var{surface}, draw an ellipse with center @var{x},@var{y}
x-radius @var{rx}, y-radius @var{ry}, with color @var{color}.
Optional arg @var{fill} means to fill the ellipse as well.  */)
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


GH_DEFPROC
(draw_aa_ellipse, "draw-aa-ellipse", 6, 0, 0,
 (SCM surface, SCM x, SCM y, SCM rx, SCM ry, SCM color),
 doc: /***********
On @var{surface}, draw an anti-aliased ellipse with center
@var{x},@var{y}, x-radius @var{rx}, y-radius @var{ry}, with
color @var{color}.  */)
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


GH_DEFPROC
(draw_pie_slice, "draw-pie-slice", 7, 1, 0,
 (SCM surface, SCM x, SCM y, SCM rad,
  SCM start, SCM end, SCM color, SCM fill),
 doc: /***********
On @var{surface}, draw a pie slice with center
@var{x},@var{y} and radius @var{rad}, going from
@var{start} to @var{end} (degrees), with color @var{color}.
Optional arg @var{fill} means to fill the slice as well.  */)
{
#define FUNC_NAME s_draw_pie_slice
  ASSERT_SURFACE (surface, ARGH1);
  ASSERT_EXACT (x, ARGH2);
  ASSERT_EXACT (y, ARGH3);
  ASSERT_EXACT (rad, ARGH4);
  ASSERT_EXACT (start, ARGH5);
  ASSERT_EXACT (end, ARGH6);
  ASSERT_EXACT (color, ARGH7);

  UNBOUND_MEANS_FALSE (fill);

  RETURN_INT
    ((EXACTLY_FALSEP (fill)
      ? pieColor
      : filledPieColor) (UNPACK_SURFACE (surface),
                         gh_scm2long (x), gh_scm2long (y), gh_scm2long (rad),
                         gh_scm2long (start), gh_scm2long (end),
                         gh_scm2ulong (color)));
#undef FUNC_NAME
}


GH_DEFPROC
(draw_trigon, "draw-trigon", 8, 1, 0,
 (SCM surface, SCM x1, SCM y1, SCM x2, SCM y2,
  SCM x3, SCM y3, SCM color, SCM fill),
 doc: /***********
On @var{surface}, draw a triangle with vertices at
@var{x1},@var{y1}, @var{x2},@var{y2} and @var{x3},@var{y3},
with color @var{color}.  Optional arg @var{fill} means to
fill the triangle as well.  */)
{
#define FUNC_NAME s_draw_trigon
  ASSERT_SURFACE (surface, ARGH1);
  ASSERT_EXACT (x1, ARGH2);
  ASSERT_EXACT (y1, ARGH3);
  ASSERT_EXACT (x2, ARGH4);
  ASSERT_EXACT (y2, ARGH5);
  ASSERT_EXACT (x3, ARGH6);
  ASSERT_EXACT (y3, ARGH7);
  ASSERT_EXACT (color, ARGH8);

  UNBOUND_MEANS_FALSE (fill);

  RETURN_INT
    ((EXACTLY_FALSEP (fill)
      ? trigonColor
      : filledTrigonColor) (UNPACK_SURFACE (surface),
                            gh_scm2long (x1), gh_scm2long (y1),
                            gh_scm2long (x2), gh_scm2long (y2),
                            gh_scm2long (x3), gh_scm2long (y3),
                            gh_scm2ulong (color)));
#undef FUNC_NAME
}


GH_DEFPROC
(draw_aa_trigon, "draw-aa-trigon", 8, 0, 0,
 (SCM surface, SCM x1, SCM y1, SCM x2, SCM y2,
  SCM x3, SCM y3, SCM color),
 doc: /***********
On @var{surface}, draw an anti-aliased triangle with vertices at
@var{x1},@var{y1}, @var{x2},@var{y2} and @var{x3},@var{y3},
with color @var{color}.  */)
{
#define FUNC_NAME s_draw_aa_trigon
  ASSERT_SURFACE (surface, ARGH1);
  ASSERT_EXACT (x1, ARGH2);
  ASSERT_EXACT (y1, ARGH3);
  ASSERT_EXACT (x2, ARGH4);
  ASSERT_EXACT (y2, ARGH5);
  ASSERT_EXACT (x3, ARGH6);
  ASSERT_EXACT (y3, ARGH7);
  ASSERT_EXACT (color, ARGH8);

  RETURN_INT
    (aatrigonColor (UNPACK_SURFACE (surface),
                    gh_scm2long (x1), gh_scm2long (y1),
                    gh_scm2long (x2), gh_scm2long (y2),
                    gh_scm2long (x3), gh_scm2long (y3),
                    gh_scm2ulong (color)));
#undef FUNC_NAME
}


GH_DEFPROC
(draw_polygon, "draw-polygon", 4, 1, 0,
 (SCM surface, SCM vx, SCM vy, SCM color, SCM fill),
 doc: /***********
On @var{surface}, draw a polygon whose points are specified
by corresponding pairs from the uniform vectors
@var{vx} and @var{vy}, in color @var{color}.  Optional
arg @var{fill} means to fill the polygon as well.  */)
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


GH_DEFPROC
(draw_aa_polygon, "draw-aa-polygon", 4, 0, 0,
 (SCM surface, SCM vx, SCM vy, SCM color),
 doc: /***********
On @var{surface}, draw an anti-aliased polygon whose points
are specified by corresponding pairs from the uniform vectors
@var{vx} and @var{vy}, in color @var{color}.  */)
{
#define FUNC_NAME s_draw_aa_polygon
  int ret;
  Sint16 *cvx, *cvy;

  ASSERT_SURFACE (surface, ARGH1);
  ASSERT_VECTOR (vx, ARGH2);
  ASSERT_VECTOR (vy, ARGH3);
  ASSERT_EXACT (color, ARGH4);

  cvx = (Sint16 *) gh_scm2shorts (vx, NULL);
  cvy = (Sint16 *) gh_scm2shorts (vy, NULL);

  ret = aapolygonColor (UNPACK_SURFACE (surface),
                        cvx, cvy,
                        gh_uniform_vector_length (vx),
                        gh_scm2ulong (color));
  free (cvx);
  free (cvy);
  RETURN_INT (ret);
#undef FUNC_NAME
}


GH_DEFPROC
(draw_textured_polygon, "draw-textured-polygon", 6, 0, 0,
 (SCM surface, SCM vx, SCM vy, SCM texture, SCM tdx, SCM tdy),
 doc: /***********
On @var{surface}, draw a polygon whose points are specified
by corresponding pairs from the uniform vectors @var{vx}
and @var{vy}, filling from @var{texture} (a surface) with
offset @var{tdx}, @var{tdy}.  */)
{
#define FUNC_NAME s_draw_textured_polygon
  int ret;
  Sint16 *cvx, *cvy;

  ASSERT_SURFACE (surface, ARGH1);
  ASSERT_VECTOR (vx, ARGH2);
  ASSERT_VECTOR (vy, ARGH3);
  ASSERT_SURFACE (texture, ARGH4);
  ASSERT_EXACT (tdx, ARGH5);
  ASSERT_EXACT (tdy, ARGH6);

  cvx = (Sint16 *) gh_scm2shorts (vx, NULL);
  cvy = (Sint16 *) gh_scm2shorts (vy, NULL);

  ret = texturedPolygon (UNPACK_SURFACE (surface),
                         cvx, cvy,
                         gh_uniform_vector_length (vx),
                         UNPACK_SURFACE (texture),
                         gh_scm2int (tdx), gh_scm2int (tdy));
  free (cvx);
  free (cvy);
  RETURN_INT (ret);
#undef FUNC_NAME
}


GH_DEFPROC
(draw_bezier, "draw-bezier", 5, 0, 0,
 (SCM surface, SCM vx, SCM vy, SCM s, SCM color),
 doc: /***********
On @var{surface}, draw a bezier curve whose points are
specified by corresponding pairs from the uniform vectors
@var{vx} and @var{vy}, with @var{s} steps in color @var{color}.  */)
{
#define FUNC_NAME s_draw_bezier
  int ret;
  Sint16 *cvx, *cvy;

  ASSERT_SURFACE (surface, ARGH1);
  ASSERT_VECTOR (vx, ARGH2);
  ASSERT_VECTOR (vy, ARGH3);
  ASSERT_EXACT (s, ARGH4);
  ASSERT_EXACT (color, ARGH5);

  cvx = (Sint16 *) gh_scm2shorts (vx, NULL);
  cvy = (Sint16 *) gh_scm2shorts (vy, NULL);

  ret = bezierColor (UNPACK_SURFACE (surface),
                     cvx, cvy,
                     gh_uniform_vector_length (vx),
                     gh_scm2long (s),
                     gh_scm2ulong (color));
  free (cvx);
  free (cvy);
  RETURN_INT (ret);
#undef FUNC_NAME
}


GH_DEFPROC
(draw_character, "draw-character", 5, 0, 0,
 (SCM surface, SCM x, SCM y, SCM c, SCM color),
 doc: /***********
On @var{surface} at position @var{x},@var{y},
draw char @var{c} with @var{color} (a number).  */)
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


GH_DEFPROC
(draw_string, "draw-string", 5, 0, 0,
 (SCM surface, SCM x, SCM y, SCM text, SCM color),
 doc: /***********
On @var{surface} at position @var{x},@var{y},
draw string @var{text} with @var{color} (a number).  */)
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



/*
 * rotozoom
 */

#include "SDL_rotozoom.h"


GH_DEFPROC
(roto_zoom_surface, "roto-zoom-surface", 2, 2, 0,
 (SCM surface,
  SCM angle,
  SCM zoom,
  SCM smooth),
 doc: /***********
Return a new surface made from rotating @var{surface}
by @var{angle} degrees.  Optional third arg @var{zoom}
(default value 1.0) changes the size as well.  Optional
fourth arg @var{smooth} turns on anti-aliasing.  */)
{
#define FUNC_NAME s_roto_zoom_surface
  SDL_Surface *csurface, *new_surface;
  double cangle = 0.0, czoom = 1.0;

  ASSERT_SURFACE (surface, ARGH1);
  csurface = SMOBGET (surface, SDL_Surface *);

  ASSERT_NUMBER (angle, ARGH2);
  cangle = gh_scm2double (angle);

  UNBOUND_MEANS_FALSE (zoom);
  if (NOT_FALSEP (zoom))
    {
      ASSERT_NUMBER (zoom, ARGH3);
      czoom = gh_scm2double (zoom);
    }

  UNBOUND_MEANS_FALSE (smooth);

  new_surface = rotozoomSurface (csurface, cangle, czoom, NOT_FALSEP (smooth));

  RETURN_NEW_SURFACE (new_surface);
#undef FUNC_NAME
}


GH_DEFPROC
(roto_zoom_surface_xy, "roto-zoom-surface-xy", 2, 3, 0,
 (SCM surface,
  SCM angle,
  SCM zoomx, SCM zoomy,
  SCM smooth),
 doc: /***********
Return a new surface made from rotating @var{surface}
by @var{angle} degrees.  Optional third and fourth args
@var{zoomx} and @var{zoomy} (default value 1.0 for both)
changes the size as well.  Optional fifth arg @var{smooth}
turns on anti-aliasing.  */)
{
#define FUNC_NAME s_roto_zoom_surface_xy
  SDL_Surface *csurface, *new_surface;
  double cangle = 0.0, czoomx = 1.0, czoomy = 1.0;

  ASSERT_SURFACE (surface, ARGH1);
  csurface = SMOBGET (surface, SDL_Surface *);

  ASSERT_NUMBER (angle, ARGH2);
  cangle = gh_scm2double (angle);

  UNBOUND_MEANS_FALSE (zoomx);
  if (NOT_FALSEP (zoomx))
    {
      ASSERT_NUMBER (zoomx, ARGH3);
      czoomx = gh_scm2double (zoomx);
    }
  UNBOUND_MEANS_FALSE (zoomy);
  if (NOT_FALSEP (zoomy))
    {
      ASSERT_NUMBER (zoomy, ARGH3);
      czoomy = gh_scm2double (zoomy);
    }

  UNBOUND_MEANS_FALSE (smooth);

  new_surface = rotozoomSurfaceXY (csurface, cangle, czoomx, czoomy,
                                   NOT_FALSEP (smooth));

  RETURN_NEW_SURFACE (new_surface);
#undef FUNC_NAME
}


GH_DEFPROC
(zoom_surface, "zoom-surface", 2, 2, 0,
 (SCM surface,
  SCM zoomx,
  SCM zoomy,
  SCM smooth),
 doc: /***********
Return a new scaled copy of @var{surface}.
@var{zoomx} and @var{zoomy} specify the scaling factor.
If omitted, @var{zoomy} defaults to @var{zoomx}.
Optional fourth arg @var{smooth} turns on anti-aliasing.  */)
{
#define FUNC_NAME s_zoom_surface
  SDL_Surface *csurface, *new_surface;
  double czoomx = 1.0, czoomy = 1.0;

  ASSERT_SURFACE (surface, ARGH1);
  csurface = SMOBGET (surface, SDL_Surface *);

  ASSERT_NUMBER (zoomx, ARGH2);
  czoomx = gh_scm2double (zoomx);

  UNBOUND_MEANS_FALSE (zoomy);
  if (NOT_FALSEP (zoomy))
    {
      ASSERT_NUMBER (zoomy, ARGH3);
      czoomy = gh_scm2double (zoomy);
    }
  else
    czoomy = czoomx;

  UNBOUND_MEANS_FALSE (smooth);

  new_surface = zoomSurface (csurface, czoomx, czoomy, NOT_FALSEP (smooth));

  RETURN_NEW_SURFACE (new_surface);
#undef FUNC_NAME
}


GH_DEFPROC
(shrink_surface, "shrink-surface", 3, 0, 0,
 (SCM surface,
  SCM factorx,
  SCM factory),
 doc: /***********
Return a new shrunken copy of @var{surface}.
@var{factorx} and @var{factory} are positive integers specifying
the inverse scaling factor.  For example, 2 means half size,
3 means one-third size, etc.

The returned surface is antialiased by ``averaging the source
box RGBA or Y information'' and is in 32-bit RGBA format.  */)
{
#define FUNC_NAME s_shrink_surface
  SDL_Surface *csurface, *new_surface;
  unsigned int cfactorx, cfactory;

  ASSERT_SURFACE (surface, ARGH1);
  csurface = SMOBGET (surface, SDL_Surface *);

  ASSERT_EXACT (factorx, ARGH2);
  cfactorx = gh_scm2ulong (factorx);

  ASSERT_NUMBER (factory, ARGH3);
  cfactory = gh_scm2ulong (factory);

  new_surface = shrinkSurface (csurface, cfactorx, cfactory);

  RETURN_NEW_SURFACE (new_surface);
#undef FUNC_NAME
}



/*
 * framerate
 */

#include "SDL_framerate.h"

static long fpsmgr_tag;

#define ASSERT_FPSMGR(obj,n)  ASSERT_SMOB (obj, fpsmgr_tag, n)
#define UNPACK_FPSMGR(smob)   (SMOBGET (smob, FPSmanager *))

static
size_t
free_fpsmgr (SCM fpsmgr)
{
  free (UNPACK_FPSMGR (fpsmgr));
  return sizeof (FPSmanager);
}

static
int
print_fpsmgr (SCM fpsmgr, SCM port, scm_print_state *pstate)
{
  FPSmanager *m = UNPACK_FPSMGR (fpsmgr);
  int chz = SDL_getFramerate (m);

  scm_puts      ("#<FPS-manager ", port);
  scm_intprint                                  (chz, 10, port);
  scm_puts      ("Hz>", port);

  /* Non-zero means success.  */
  return 1;
}


GH_DEFPROC
(make_fps_manager, "make-fps-manager", 0, 1, 0,
 (SCM n),
 doc: /***********
Return a FPS manager object to be passed as the first
arg to @code{fps-manager-set!}, @code{fps-manager-get} and
@code{fps-manager-delay!}.
Optional arg @var{n} specifies the value in Hz to
initialize the object (default 30 if not specified).  */)
{
#define FUNC_NAME s_make_fps_manager
  FPSmanager *m;

  UNBOUND_MEANS_FALSE (n);
  if ((m = (FPSmanager *) malloc (sizeof (FPSmanager))))
    {
      SDL_initFramerate (m);
      if (NOT_FALSEP (n))
        {
          ASSERT_EXACT (n, ARGH1);
          SDL_setFramerate (m, gh_scm2int (n));
        }
    }

  NEWSMOB_OR_FALSE (fpsmgr_tag, m);
#undef FUNC_NAME
}


GH_DEFPROC
(fps_manager_set_x, "fps-manager-set!", 2, 0, 0,
 (SCM mgr, SCM n),
 doc: /***********
Arrange for FPS manager @var{mgr} to try to maintain a
frame rate of @var{n} Hz.  Return #f if not successful.  */)
{
#define FUNC_NAME s_fps_manager_set_x
  ASSERT_FPSMGR (mgr, ARGH1);
  ASSERT_EXACT (n, ARGH2);

  RETURN_TRUE_IF_0
    (SDL_setFramerate (UNPACK_FPSMGR (mgr), gh_scm2int (n)));
#undef FUNC_NAME
}


GH_DEFPROC
(fps_manager_get, "fps-manager-get", 1, 0, 0,
 (SCM mgr),
 doc: /***********
Return the frame rate of FPS manager @var{mgr} in Hz,
or #f if unsuccessful.  */)
{
#define FUNC_NAME s_fps_manager_get
  int ret;

  ASSERT_FPSMGR (mgr, ARGH1);

  ret = SDL_getFramerate (UNPACK_FPSMGR (mgr));
  return (0 > ret
          ? SCM_BOOL_F
          : gh_int2scm (ret));
#undef FUNC_NAME
}


GH_DEFPROC
(fps_manager_delay_x, "fps-manager-delay!", 1, 0, 0,
 (SCM mgr),
 doc: /***********
Request an appropriate delay from FPS manager @var{mgr}.  */)
{
#define FUNC_NAME s_fps_manager_delay_x
  ASSERT_FPSMGR (mgr, ARGH1);

  SDL_framerateDelay (UNPACK_FPSMGR (mgr));

  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}



/*
 * RGBA extras
 */

#include "SDL_gfxBlitFunc.h"


GH_DEFPROC
(set_pixel_alpha_x, "set-pixel-alpha!", 2, 0, 0,
 (SCM surface, SCM alpha),
 doc: /***********
If @var{surface} is 32-bit, set each pixel's alpha value to
@var{alpha}, an integer 0-255, inclusive, and return @code{#t}.
Otherwise, do nothing and return @code{#f}.  */)
{
#define FUNC_NAME s_set_pixel_alpha_x
  ASSERT_SURFACE (surface, ARGH1);
  ASSERT_EXACT (alpha, ARGH2);

  return (1 == SDL_gfxSetAlpha (UNPACK_SURFACE (surface),
                                (Uint8) gh_scm2ulong (alpha))
          ? SCM_BOOL_T
          : SCM_BOOL_F);
#undef FUNC_NAME
}


GH_DEFPROC
(blit_rgba, "blit-rgba", 4, 0, 0,
 (SCM src, SCM srect, SCM dst, SCM drect),
 doc: /***********
Blit from 32-bit surface @var{src} rectangle @var{srect}
to 32-bit surface @var{dst} rectangle @var{drect}.
Return @code{#t} if there are no problems.

Note that unlike @code{blit-surface} (@pxref{Video}),
all arguments must be fully specified.  This restriction
may be relaxed in the future.  */)
{
#define FUNC_NAME s_blit_rgba
  ASSERT_SURFACE (src, ARGH1);
  ASSERT_RECT (srect, ARGH2);
  ASSERT_SURFACE (dst, ARGH3);
  ASSERT_RECT (drect, ARGH4);

  RETURN_TRUE_IF_0 (SDL_gfxBlitRGBA (UNPACK_SURFACE (src),
                                     UNPACK_RECT (srect),
                                     UNPACK_SURFACE (dst),
                                     UNPACK_RECT (drect)));
#undef FUNC_NAME
}



/* dispatch */

static
void
init_module (void)
{
  fpsmgr_tag = scm_make_smob_type ("FPS-manager", sizeof (FPSmanager));
  scm_set_smob_free  (fpsmgr_tag, free_fpsmgr);
  scm_set_smob_print (fpsmgr_tag, print_fpsmgr);

#include "sdlgfx.x"
}

GH_MODULE_LINK_FUNC ("sdl gfx", sdl_gfx, init_module)

/* sdlgfx.c ends here */
