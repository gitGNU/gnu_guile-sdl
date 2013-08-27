/* sdlgfx.c --- Additional Graphics functions for Guile SDL
 *
 * Copyright (C) 2003, 2004, 2005, 2007, 2008,
 *   2009, 2011, 2013 Thien-Thi Nguyen
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

#define GUILE_SDL_OPTIONAL_MODULE  1
#include "guile-sdl.h"
#include <stdio.h>
#include <alloca.h>
#include "snuggle/finangle.h"
#include "b-uv.h"

IMPORT_MODULE (sdlsup, "(sdl sdl)");
SELECT_MODULE_VAR (obtw, sdlsup, "%%Guile-SDL-obtw");
#if ! GI_LEVEL_1_8
IMPORT_SRFI4 ();
SELECT_UVEC_PREDICATE (s16);
#define scm_s16vector_p(obj)  CALL1 (s16v_p, obj)
#endif

DEFINE_STRUCT_AND_COPY_FUNC (s16, Sint16)
#define ASSERT_UVEC_S16(obj,n)  ASSERT_UVEC (s16, obj, n)
#define S16_STUFF(v)            STUFF (s16, v)
#define GET_S16_PARTICULARS(v)  GET_PARTICULARS (s16, v)
#define HOWDY_S16(v)            HOWDY (s16, Sint16, v)

#define STUFF2(one,two)  S16_STUFF (one); S16_STUFF (two)
#define HOWDY2(one,two)  HOWDY_S16 (one); HOWDY_S16 (two)
#define LATER2(one,two)  LATER (one); LATER (two)


#define ASSERT_DOUBLE_COPY(name,pos)  do        \
    {                                           \
      ASSERT_NUMBER (name, pos);                \
      c ## name = C_DOUBLE (name);              \
    }                                           \
  while (0)

#define IF_BOUND_ASSERT_DOUBLE_COPY(name,pos)   \
  if (BOUNDP (name) && NOT_FALSEP (name))       \
    ASSERT_DOUBLE_COPY (name, pos);


/*
 * gfxPrimitives
 */

#include "SDL_gfx/SDL_gfxPrimitives.h"


PRIMPROC
(draw_point, "draw-point", 4, 0, 0,
 (SCM surface, SCM x, SCM y, SCM color),
 doc: /***********
On @var{surface}, draw a point at location
@var{x},@var{y} with color @var{color}.  */)
{
#define FUNC_NAME s_draw_point
  ASSERT_SURFACE (surface, 1);
  ASSERT_INTEGER (x, 2);
  ASSERT_INTEGER (y, 3);
  ASSERT_INTEGER (color, 4);

  RETURN_INT
    (pixelColor (UNPACK_SURFACE (surface),
                 C_LONG (x), C_LONG (y), C_ULONG (color)));
#undef FUNC_NAME
}


PRIMPROC
(draw_hline, "draw-hline", 5, 0, 0,
 (SCM surface, SCM x1, SCM x2, SCM y, SCM color),
 doc: /***********
On @var{surface}, draw a horizontal line segment
from @var{x1},@var{y} to @var{x2},@var{y},
with color @var{color}.  */)
{
#define FUNC_NAME s_draw_hline
  ASSERT_SURFACE (surface, 1);
  ASSERT_INTEGER (x1, 2);
  ASSERT_INTEGER (x2, 3);
  ASSERT_INTEGER (y, 4);
  ASSERT_INTEGER (color, 5);

  RETURN_INT
    (hlineColor (UNPACK_SURFACE (surface), C_LONG (x1), C_LONG (x2),
                 C_LONG (y), C_ULONG (color)));
#undef FUNC_NAME
}


PRIMPROC
(draw_vline, "draw-vline", 5, 0, 0,
 (SCM surface, SCM x, SCM y1, SCM y2, SCM color),
 doc: /***********
On @var{surface}, draw a vertical line segment
from @var{x},@var{y1} to @var{x},@var{y2},
with color @var{color}.  */)
{
#define FUNC_NAME s_draw_vline
  ASSERT_SURFACE (surface, 1);
  ASSERT_INTEGER (x, 2);
  ASSERT_INTEGER (y1, 3);
  ASSERT_INTEGER (y2, 4);
  ASSERT_INTEGER (color, 5);

  RETURN_INT
    (vlineColor (UNPACK_SURFACE (surface), C_LONG (x),
                 C_LONG (y1), C_LONG (y2), C_ULONG (color)));
#undef FUNC_NAME
}


PRIMPROC
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
  ASSERT_SURFACE (surface, 1);
  ASSERT_INTEGER (x1, 2);
  ASSERT_INTEGER (y1, 3);
  ASSERT_INTEGER (x2, 4);
  ASSERT_INTEGER (y2, 5);
  ASSERT_INTEGER (color, 6);
  UNBOUND_MEANS_FALSE (fill);

  RETURN_INT
    ((EXACTLY_FALSEP (fill)
      ? rectangleColor
      : boxColor) (UNPACK_SURFACE (surface),
                   C_LONG (x1), C_LONG (y1),
                   C_LONG (x2), C_LONG (y2),
                   C_ULONG (color)));
#undef FUNC_NAME
}


PRIMPROC
(draw_rounded_rectangle, "draw-rounded-rectangle", 7, 1, 0,
 (SCM surface, SCM x1, SCM y1, SCM x2, SCM y2,
  SCM rad, SCM color, SCM fill),
 doc: /***********
On @var{surface}, draw a rectangle with opposite points
@var{x1},@var{y1} and @var{x2},@var{y2},
with rounded corners radius @var{rad} in color @var{color}.
Optional arg @var{fill} means to fill the rectangle as well.  */)
{
#define FUNC_NAME s_draw_rounded_rectangle
  ASSERT_SURFACE (surface, 1);
  ASSERT_INTEGER (x1, 2);
  ASSERT_INTEGER (y1, 3);
  ASSERT_INTEGER (x2, 4);
  ASSERT_INTEGER (y2, 5);
  ASSERT_INTEGER (rad, 6);
  ASSERT_INTEGER (color, 7);
  UNBOUND_MEANS_FALSE (fill);

  RETURN_INT
    ((EXACTLY_FALSEP (fill)
      ? roundedRectangleColor
      : roundedBoxColor) (UNPACK_SURFACE (surface),
                          C_LONG (x1), C_LONG (y1),
                          C_LONG (x2), C_LONG (y2),
                          C_LONG (rad),
                          C_ULONG (color)));
#undef FUNC_NAME
}


PRIMPROC
(draw_line, "draw-line", 6, 0, 0,
 (SCM surface, SCM x1, SCM y1,
  SCM x2, SCM y2, SCM color),
 doc: /***********
On @var{surface}, draw a line segment from
@var{x1},@var{y1} to @var{x2},@var{y2},
with color @var{color}.  */)
{
#define FUNC_NAME s_draw_line
  ASSERT_SURFACE (surface, 1);
  ASSERT_INTEGER (x1, 2);
  ASSERT_INTEGER (y1, 3);
  ASSERT_INTEGER (x2, 4);
  ASSERT_INTEGER (y2, 5);
  ASSERT_INTEGER (color, 6);

  RETURN_INT
    (lineColor (UNPACK_SURFACE (surface),
                C_LONG (x1), C_LONG (y1),
                C_LONG (x2), C_LONG (y2),
                C_ULONG (color)));
#undef FUNC_NAME
}


PRIMPROC
(draw_aa_line, "draw-aa-line", 6, 0, 0,
 (SCM surface, SCM x1, SCM y1,
  SCM x2, SCM y2, SCM color),
 doc: /***********
On @var{surface}, draw an anti-aliased line segment from
@var{x1},@var{y1} to @var{x2},@var{y2},
with color @var{color}.  */)
{
#define FUNC_NAME s_draw_aa_line
  ASSERT_SURFACE (surface, 1);
  ASSERT_INTEGER (x1, 2);
  ASSERT_INTEGER (y1, 3);
  ASSERT_INTEGER (x2, 4);
  ASSERT_INTEGER (y2, 5);
  ASSERT_INTEGER (color, 6);

  RETURN_INT
    (aalineColor (UNPACK_SURFACE (surface),
                  C_LONG (x1), C_LONG (y1),
                  C_LONG (x2), C_LONG (y2),
                  C_ULONG (color)));
#undef FUNC_NAME
}


PRIMPROC
(draw_thick_line, "draw-thick-line", 7, 0, 0,
 (SCM surface, SCM x1, SCM y1, SCM x2, SCM y2,
  SCM width, SCM color),
 doc: /***********
On @var{surface}, draw a line segment from
@var{x1},@var{y1} to @var{x2},@var{y2},
with thickness @var{width} in color @var{color}.  */)
{
#define FUNC_NAME s_draw_thick_line
  ASSERT_SURFACE (surface, 1);
  ASSERT_INTEGER (x1, 2);
  ASSERT_INTEGER (y1, 3);
  ASSERT_INTEGER (x2, 4);
  ASSERT_INTEGER (y2, 5);
  ASSERT_INTEGER (width, 6);
  ASSERT_INTEGER (color, 7);

  RETURN_INT
    (thickLineColor (UNPACK_SURFACE (surface),
                     C_LONG (x1), C_LONG (y1),
                     C_LONG (x2), C_LONG (y2),
                     C_ULONG (width), C_ULONG (color)));
#undef FUNC_NAME
}


PRIMPROC
(draw_circle, "draw-circle", 5, 1, 0,
 (SCM surface, SCM x, SCM y, SCM r,
  SCM color, SCM fill),
 doc: /***********
On @var{surface}, draw a circle with center @var{x},@var{y}
and radius @var{r}, with color @var{color}.
Optional arg @var{fill} means to fill the circle as well.  */)
{
#define FUNC_NAME s_draw_circle
  ASSERT_SURFACE (surface, 1);
  ASSERT_INTEGER (x, 2);
  ASSERT_INTEGER (y, 3);
  ASSERT_INTEGER (r, 4);
  ASSERT_INTEGER (color, 5);
  UNBOUND_MEANS_FALSE (fill);

  RETURN_INT
    ((EXACTLY_FALSEP (fill)
      ? circleColor
      : filledCircleColor) (UNPACK_SURFACE (surface), C_LONG (x), C_LONG (y),
                            C_LONG (r), C_ULONG (color)));
#undef FUNC_NAME
}


PRIMPROC
(draw_arc, "draw-arc", 7, 0, 0,
 (SCM surface, SCM x, SCM y, SCM r,
  SCM start, SCM end, SCM color),
 doc: /***********
On @var{surface}, draw arc with center @var{x},@var{y}
and radius @var{r}, going from @var{start} to @var{end} (degrees),
with color @var{color}.

If @var{start} is greater than @var{end}, the effective range of
the arc is taken to be @var{end} to @var{start} (that is, these
arguments are internally reversed).  */)
{
#define FUNC_NAME s_draw_arc
  ASSERT_SURFACE (surface, 1);
  ASSERT_INTEGER (x, 2);
  ASSERT_INTEGER (y, 3);
  ASSERT_INTEGER (r, 4);
  ASSERT_INTEGER (start, 5);
  ASSERT_INTEGER (end, 6);
  ASSERT_INTEGER (color, 7);

  RETURN_INT
    (arcColor (UNPACK_SURFACE (surface), C_LONG (x), C_LONG (y),
               C_LONG (r), C_LONG (start), C_LONG (end), C_ULONG (color)));
#undef FUNC_NAME
}


PRIMPROC
(draw_aa_circle, "draw-aa-circle", 5, 0, 0,
 (SCM surface, SCM x, SCM y, SCM r, SCM color),
 doc: /***********
On @var{surface}, draw an anti-aliased circle with center
@var{x},@var{y} and radius @var{r}, with color @var{color}.  */)
{
#define FUNC_NAME s_draw_aa_circle
  ASSERT_SURFACE (surface, 1);
  ASSERT_INTEGER (x, 2);
  ASSERT_INTEGER (y, 3);
  ASSERT_INTEGER (r, 4);
  ASSERT_INTEGER (color, 5);

  RETURN_INT
    (aacircleColor (UNPACK_SURFACE (surface), C_LONG (x), C_LONG (y),
                    C_LONG (r), C_ULONG (color)));
#undef FUNC_NAME
}


PRIMPROC
(draw_ellipse, "draw-ellipse", 6, 1, 0,
 (SCM surface, SCM x, SCM y, SCM rx, SCM ry,
  SCM color, SCM fill),
 doc: /***********
On @var{surface}, draw an ellipse with center @var{x},@var{y}
x-radius @var{rx}, y-radius @var{ry}, with color @var{color}.
Optional arg @var{fill} means to fill the ellipse as well.  */)
{
#define FUNC_NAME s_draw_ellipse
  ASSERT_SURFACE (surface, 1);
  ASSERT_INTEGER (x, 2);
  ASSERT_INTEGER (y, 3);
  ASSERT_INTEGER (rx, 4);
  ASSERT_INTEGER (ry, 5);
  ASSERT_INTEGER (color, 6);
  UNBOUND_MEANS_FALSE (fill);

  RETURN_INT
    ((EXACTLY_FALSEP (fill)
      ? ellipseColor
      : filledEllipseColor) (UNPACK_SURFACE (surface), C_LONG (x), C_LONG (y),
                             C_LONG (rx), C_LONG (ry), C_ULONG (color)));
#undef FUNC_NAME
}


PRIMPROC
(draw_aa_ellipse, "draw-aa-ellipse", 6, 0, 0,
 (SCM surface, SCM x, SCM y, SCM rx, SCM ry, SCM color),
 doc: /***********
On @var{surface}, draw an anti-aliased ellipse with center
@var{x},@var{y}, x-radius @var{rx}, y-radius @var{ry}, with
color @var{color}.  */)
{
#define FUNC_NAME s_draw_aa_ellipse
  ASSERT_SURFACE (surface, 1);
  ASSERT_INTEGER (x, 2);
  ASSERT_INTEGER (y, 3);
  ASSERT_INTEGER (rx, 4);
  ASSERT_INTEGER (ry, 5);
  ASSERT_INTEGER (color, 6);

  RETURN_INT
    (aaellipseColor (UNPACK_SURFACE (surface), C_LONG (x), C_LONG (y),
                     C_LONG (rx), C_LONG (ry), C_ULONG (color)));
#undef FUNC_NAME
}


PRIMPROC
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
  ASSERT_SURFACE (surface, 1);
  ASSERT_INTEGER (x, 2);
  ASSERT_INTEGER (y, 3);
  ASSERT_INTEGER (rad, 4);
  ASSERT_INTEGER (start, 5);
  ASSERT_INTEGER (end, 6);
  ASSERT_INTEGER (color, 7);

  UNBOUND_MEANS_FALSE (fill);

  RETURN_INT
    ((EXACTLY_FALSEP (fill)
      ? pieColor
      : filledPieColor) (UNPACK_SURFACE (surface),
                         C_LONG (x), C_LONG (y), C_LONG (rad),
                         C_LONG (start), C_LONG (end), C_ULONG (color)));
#undef FUNC_NAME
}


PRIMPROC
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
  ASSERT_SURFACE (surface, 1);
  ASSERT_INTEGER (x1, 2);
  ASSERT_INTEGER (y1, 3);
  ASSERT_INTEGER (x2, 4);
  ASSERT_INTEGER (y2, 5);
  ASSERT_INTEGER (x3, 6);
  ASSERT_INTEGER (y3, 7);
  ASSERT_INTEGER (color, 8);

  UNBOUND_MEANS_FALSE (fill);

  RETURN_INT
    ((EXACTLY_FALSEP (fill)
      ? trigonColor
      : filledTrigonColor) (UNPACK_SURFACE (surface),
                            C_LONG (x1), C_LONG (y1),
                            C_LONG (x2), C_LONG (y2),
                            C_LONG (x3), C_LONG (y3),
                            C_ULONG (color)));
#undef FUNC_NAME
}


PRIMPROC
(draw_aa_trigon, "draw-aa-trigon", 8, 0, 0,
 (SCM surface, SCM x1, SCM y1, SCM x2, SCM y2,
  SCM x3, SCM y3, SCM color),
 doc: /***********
On @var{surface}, draw an anti-aliased triangle with vertices at
@var{x1},@var{y1}, @var{x2},@var{y2} and @var{x3},@var{y3},
with color @var{color}.  */)
{
#define FUNC_NAME s_draw_aa_trigon
  ASSERT_SURFACE (surface, 1);
  ASSERT_INTEGER (x1, 2);
  ASSERT_INTEGER (y1, 3);
  ASSERT_INTEGER (x2, 4);
  ASSERT_INTEGER (y2, 5);
  ASSERT_INTEGER (x3, 6);
  ASSERT_INTEGER (y3, 7);
  ASSERT_INTEGER (color, 8);

  RETURN_INT
    (aatrigonColor (UNPACK_SURFACE (surface),
                    C_LONG (x1), C_LONG (y1),
                    C_LONG (x2), C_LONG (y2),
                    C_LONG (x3), C_LONG (y3),
                    C_ULONG (color)));
#undef FUNC_NAME
}


PRIMPROC
(draw_polygon, "draw-polygon", 4, 1, 0,
 (SCM surface, SCM vx, SCM vy, SCM color, SCM fill),
 doc: /***********
On @var{surface}, draw a polygon whose points are specified
by corresponding pairs from the s16 uniform vectors
@var{vx} and @var{vy}, in color @var{color}.  Optional
arg @var{fill} means to fill the polygon as well.  */)
{
#define FUNC_NAME s_draw_polygon
  STUFF2 (vx, vy);
  int rv;

  ASSERT_SURFACE (surface, 1);
  ASSERT_UVEC_S16 (vx, 2);
  ASSERT_UVEC_S16 (vy, 3);
  ASSERT_INTEGER (color, 4);
  UNBOUND_MEANS_FALSE (fill);

  HOWDY2 (vx, vy);
  rv = (EXACTLY_FALSEP (fill)
        ? polygonColor
        : filledPolygonColor) (UNPACK_SURFACE (surface),
                               VBITS (vx), VBITS (vy), VLEN (vx),
                               C_ULONG (color));
  LATER2 (vy, vx);
  RETURN_INT (rv);
#undef FUNC_NAME
}


PRIMPROC
(draw_aa_polygon, "draw-aa-polygon", 4, 0, 0,
 (SCM surface, SCM vx, SCM vy, SCM color),
 doc: /***********
On @var{surface}, draw an anti-aliased polygon whose points
are specified by corresponding pairs from the s16 uniform vectors
@var{vx} and @var{vy}, in color @var{color}.  */)
{
#define FUNC_NAME s_draw_aa_polygon
  STUFF2 (vx, vy);
  int rv;

  ASSERT_SURFACE (surface, 1);
  ASSERT_UVEC_S16 (vx, 2);
  ASSERT_UVEC_S16 (vy, 3);
  ASSERT_INTEGER (color, 4);

  HOWDY2 (vx, vy);
  rv = aapolygonColor (UNPACK_SURFACE (surface),
                       VBITS (vx), VBITS (vy), VLEN (vx),
                       C_ULONG (color));
  LATER2 (vy, vx);
  RETURN_INT (rv);
#undef FUNC_NAME
}


PRIMPROC
(draw_textured_polygon, "draw-textured-polygon", 6, 0, 0,
 (SCM surface, SCM vx, SCM vy, SCM texture, SCM tdx, SCM tdy),
 doc: /***********
On @var{surface}, draw a polygon whose points are specified
by corresponding pairs from the s16 uniform vectors @var{vx}
and @var{vy}, filling from @var{texture} (a surface) with
offset @var{tdx}, @var{tdy}.  */)
{
#define FUNC_NAME s_draw_textured_polygon
  STUFF2 (vx, vy);
  int rv;

  ASSERT_SURFACE (surface, 1);
  ASSERT_UVEC_S16 (vx, 2);
  ASSERT_UVEC_S16 (vy, 3);
  ASSERT_SURFACE (texture, 4);
  ASSERT_INTEGER (tdx, 5);
  ASSERT_INTEGER (tdy, 6);

  HOWDY2 (vx, vy);
  rv = texturedPolygon (UNPACK_SURFACE (surface),
                        VBITS (vx), VBITS (vy), VLEN (vx),
                        UNPACK_SURFACE (texture),
                        C_INT (tdx), C_INT (tdy));
  LATER2 (vy, vx);
  RETURN_INT (rv);
#undef FUNC_NAME
}


PRIMPROC
(draw_bezier, "draw-bezier", 5, 0, 0,
 (SCM surface, SCM vx, SCM vy, SCM s, SCM color),
 doc: /***********
On @var{surface}, draw a bezier curve whose points are
specified by corresponding pairs from the s16 uniform vectors
@var{vx} and @var{vy}, with @var{s} steps in color @var{color}.  */)
{
#define FUNC_NAME s_draw_bezier
  STUFF2 (vx, vy);
  int rv;

  ASSERT_SURFACE (surface, 1);
  ASSERT_UVEC_S16 (vx, 2);
  ASSERT_UVEC_S16 (vy, 3);
  ASSERT_INTEGER (s, 4);
  ASSERT_INTEGER (color, 5);

  HOWDY2 (vx, vy);
  rv = bezierColor (UNPACK_SURFACE (surface),
                    VBITS (vx), VBITS (vy), VLEN (vx),
                    C_LONG (s), C_ULONG (color));
  LATER2 (vy, vx);
  RETURN_INT (rv);
#undef FUNC_NAME
}


PRIMPROC
(draw_character, "draw-character", 5, 0, 0,
 (SCM surface, SCM x, SCM y, SCM c, SCM color),
 doc: /***********
On @var{surface} at position @var{x},@var{y},
draw char @var{c} with @var{color} (a number).  */)
{
#define FUNC_NAME s_draw_character
  ASSERT_SURFACE (surface, 1);
  ASSERT_INTEGER (x, 2);
  ASSERT_INTEGER (y, 3);
  ASSERT_CHAR (c, 4);
  ASSERT_INTEGER (color, 5);

  RETURN_INT
    (characterColor (UNPACK_SURFACE (surface), C_LONG (x), C_LONG (y),
                     C_CHAR (c), C_ULONG (color)));
#undef FUNC_NAME
}


PRIMPROC
(draw_string, "draw-string", 5, 0, 0,
 (SCM surface, SCM x, SCM y, SCM text, SCM color),
 doc: /***********
On @var{surface} at position @var{x},@var{y},
draw string @var{text} with @var{color} (a number).  */)
{
#define FUNC_NAME s_draw_string
  range_t ctext;
  int rv;

  ASSERT_SURFACE (surface, 1);
  ASSERT_INTEGER (x, 2);
  ASSERT_INTEGER (y, 3);
  ASSERT_STRING (text, 4);
  ASSERT_INTEGER (color, 5);

  FINANGLE (text);
  rv = stringColor (UNPACK_SURFACE (surface), C_LONG (x), C_LONG (y),
                    RS (text), C_ULONG (color));
  UNFINANGLE (text);
  RETURN_INT (rv);
#undef FUNC_NAME
}


static SCM frot_enum;
#include "k/frot.c"

PRIMPROC
(font_rotation_x, "font-rotation!", 1, 0, 0,
 (SCM rotation),
 doc: /***********
Set the rotation for glyphs drawn by @code{draw-character} and
@code{draw-string} to @var{rotation} (an integer or symbol), one of:

@example
0  none
1  clockwise
2  upside-down
3  counter-clockwise
@end example  */)
{
#define FUNC_NAME s_font_rotation_x
  DECLINIT_SYM2NUM_CC       (1, frot_enum);
  int crotation = ENUM2LONG (1, rotation);

  gfxPrimitivesSetFontRotation (crotation);
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}



/*
 * rotozoom
 */

#include "SDL_gfx/SDL_rotozoom.h"


PRIMPROC
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

  ASSERT_SURFACE (surface, 1);
  csurface = UNPACK_SURFACE (surface);

  ASSERT_DOUBLE_COPY (angle, 2);
  IF_BOUND_ASSERT_DOUBLE_COPY (zoom, 3);
  UNBOUND_MEANS_FALSE (smooth);

  new_surface = rotozoomSurface (csurface, cangle, czoom, NOT_FALSEP (smooth));

  RETURN_NEW_SURFACE (new_surface);
#undef FUNC_NAME
}


PRIMPROC
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

  ASSERT_SURFACE (surface, 1);
  csurface = UNPACK_SURFACE (surface);

  ASSERT_DOUBLE_COPY (angle, 2);
  IF_BOUND_ASSERT_DOUBLE_COPY (zoomx, 3);
  IF_BOUND_ASSERT_DOUBLE_COPY (zoomy, 4);
  UNBOUND_MEANS_FALSE (smooth);

  new_surface = rotozoomSurfaceXY (csurface, cangle, czoomx, czoomy,
                                   NOT_FALSEP (smooth));

  RETURN_NEW_SURFACE (new_surface);
#undef FUNC_NAME
}


PRIMPROC
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

  ASSERT_SURFACE (surface, 1);
  csurface = UNPACK_SURFACE (surface);

  ASSERT_DOUBLE_COPY (zoomx, 2);
  czoomy = czoomx;
  IF_BOUND_ASSERT_DOUBLE_COPY (zoomy, 3);
  UNBOUND_MEANS_FALSE (smooth);

  new_surface = zoomSurface (csurface, czoomx, czoomy, NOT_FALSEP (smooth));

  RETURN_NEW_SURFACE (new_surface);
#undef FUNC_NAME
}


PRIMPROC
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

  ASSERT_SURFACE (surface, 1);
  csurface = UNPACK_SURFACE (surface);

  ASSERT_INTEGER (factorx, 2);
  cfactorx = C_ULONG (factorx);

  ASSERT_NUMBER (factory, 3);
  cfactory = C_ULONG (factory);

  new_surface = shrinkSurface (csurface, cfactorx, cfactory);

  RETURN_NEW_SURFACE (new_surface);
#undef FUNC_NAME
}



/*
 * framerate
 */

#include "SDL_gfx/SDL_framerate.h"

static smob_tag_t fpsmgr_tag;

#define fpsmgr_nick "SDL-FPS-Manager"

#define ASSERT_FPSMGR(obj,n)  ASSERT_SMOB (obj, fpsmgr, n)
#define UNPACK_FPSMGR(smob)   (SMOBGET (smob, FPSmanager *))

static
size_t
free_fpsmgr (SCM fpsmgr)
{
  FPSmanager *cfpsmgr = UNPACK_FPSMGR (fpsmgr);

  GCFREE (cfpsmgr, fpsmgr_nick);
  return GCRV (cfpsmgr);
}

static
int
print_fpsmgr (SCM fpsmgr, SCM port, UNUSED scm_print_state *pstate)
{
  FPSmanager *m = UNPACK_FPSMGR (fpsmgr);
  char buf[24];

  snprintf (buf, 24, "#<FPS-manager %dHz>", SDL_getFramerate (m));
  scm_puts (buf, port);
  return 1;
}


PRIMPROC
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
  if ((m = GCMALLOC (sizeof (FPSmanager), fpsmgr_nick)))
    {
      SDL_initFramerate (m);
      if (NOT_FALSEP (n))
        {
          ASSERT_INTEGER (n, 1);
          SDL_setFramerate (m, C_INT (n));
        }
    }

  NEWSMOB_OR_FALSE (fpsmgr_tag, m);
#undef FUNC_NAME
}


PRIMPROC
(fps_manager_set_x, "fps-manager-set!", 2, 0, 0,
 (SCM mgr, SCM n),
 doc: /***********
Arrange for FPS manager @var{mgr} to try to maintain a
frame rate of @var{n} Hz.  Return @code{#f} if not successful.  */)
{
#define FUNC_NAME s_fps_manager_set_x
  ASSERT_FPSMGR (mgr, 1);
  ASSERT_INTEGER (n, 2);

  RETURN_TRUE_IF_0
    (SDL_setFramerate (UNPACK_FPSMGR (mgr), C_INT (n)));
#undef FUNC_NAME
}


PRIMPROC
(fps_manager_get, "fps-manager-get", 1, 0, 0,
 (SCM mgr),
 doc: /***********
Return the frame rate of FPS manager @var{mgr} in Hz,
or @code{#f} if unsuccessful.  */)
{
#define FUNC_NAME s_fps_manager_get
  int ret;

  ASSERT_FPSMGR (mgr, 1);

  ret = SDL_getFramerate (UNPACK_FPSMGR (mgr));
  return (0 > ret
          ? SCM_BOOL_F
          : NUM_INT (ret));
#undef FUNC_NAME
}


PRIMPROC
(fps_manager_delay_x, "fps-manager-delay!", 1, 0, 0,
 (SCM mgr),
 doc: /***********
Request an appropriate delay from FPS manager @var{mgr}.  */)
{
#define FUNC_NAME s_fps_manager_delay_x
  ASSERT_FPSMGR (mgr, 1);

  SDL_framerateDelay (UNPACK_FPSMGR (mgr));

  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}



/*
 * RGBA extras
 */

#include "SDL_gfx/SDL_gfxBlitFunc.h"


PRIMPROC
(set_pixel_alpha_x, "set-pixel-alpha!", 2, 0, 0,
 (SCM surface, SCM alpha),
 doc: /***********
If @var{surface} is 32-bit, set each pixel's alpha value to
@var{alpha}, an integer 0-255, inclusive, and return @code{#t}.
Otherwise, do nothing and return @code{#f}.  */)
{
#define FUNC_NAME s_set_pixel_alpha_x
  ASSERT_SURFACE (surface, 1);
  ASSERT_INTEGER (alpha, 2);

  return (1 == SDL_gfxSetAlpha (UNPACK_SURFACE (surface),
                                (Uint8) C_ULONG (alpha))
          ? SCM_BOOL_T
          : SCM_BOOL_F);
#undef FUNC_NAME
}


PRIMPROC
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
  ASSERT_SURFACE (src, 1);
  ASSERT_RECT (srect, 2);
  ASSERT_SURFACE (dst, 3);
  ASSERT_RECT (drect, 4);

  RETURN_TRUE_IF_0 (SDL_gfxBlitRGBA (UNPACK_SURFACE (src),
                                     UNPACK_RECT (srect),
                                     UNPACK_SURFACE (dst),
                                     UNPACK_RECT (drect)));
#undef FUNC_NAME
}



/*
 * image filter
 */

#include "SDL_gfx/SDL_imageFilter.h"

PRIMPROC
(imfi_mmx_p, "imfi-mmx?", 0, 1, 0,
 (SCM setting),
 doc: /***********
If @var{setting} is @code{#t}, enable @sc{mmx} instructions
for the image filter procs (if possible); if @code{#f}, disable;
otherwise do nothing.  Return the (boolean) value of the setting
afterwards.  */)
{
  if (BOUNDP (setting))
    {
      if (EXACTLY_TRUEP (setting))
        SDL_imageFilterMMXon ();
      if (EXACTLY_FALSEP (setting))
        SDL_imageFilterMMXoff ();
    }

  RETURN_BOOL (SDL_imageFilterMMXdetect ());
}

static int
check_3_surfaces (unsigned char **pa, SDL_Surface *a,
                  unsigned char **pb, SDL_Surface *b,
                  unsigned char **pc, SDL_Surface *c,
                  int *len)
{
  if (a->format != b->format || a->format != c->format ||
      a->w      != b->w      || a->w      != c->w      ||
      a->h      != b->h      || a->h      != c->h)
    return -1;
  else
    {
      *pa = (unsigned char *) a->pixels;
      *pb = (unsigned char *) b->pixels;
      *pc = (unsigned char *) c->pixels;

      *len = a->format->BytesPerPixel * a->w * a->h;
      return 0;
    }
}

#define S1S2D_DECL_AND_CHECK()                          \
  unsigned char *s1, *s2, *d;                           \
  int len;                                              \
                                                        \
  ASSERT_SURFACE (src1, 1);                             \
  ASSERT_SURFACE (src2, 2);                             \
  ASSERT_SURFACE (dst,  3);                             \
  if (0 > check_3_surfaces (&s1, UNPACK_SURFACE (src1), \
                            &s2, UNPACK_SURFACE (src2), \
                            &d,  UNPACK_SURFACE (dst),  \
                            &len))                      \
    return BOOL_FALSE

static int
check_2_surfaces (unsigned char **pa, SDL_Surface *a,
                  unsigned char **pb, SDL_Surface *b,
                  int *len)
{
  if (a->format != b->format ||
      a->w      != b->w      ||
      a->h      != b->h)
    return -1;
  else
    {
      *pa = (unsigned char *) a->pixels;
      *pb = (unsigned char *) b->pixels;

      *len = a->format->BytesPerPixel * a->w * a->h;
      return 0;
    }
}

#define SD_DECL_AND_CHECK()                             \
  unsigned char *s, *d;                                 \
  int len;                                              \
                                                        \
  ASSERT_SURFACE (src, 1);                              \
  ASSERT_SURFACE (dst, 2);                              \
  if (0 > check_2_surfaces (&s, UNPACK_SURFACE (src),   \
                            &d, UNPACK_SURFACE (dst),   \
                            &len))                      \
    return BOOL_FALSE

PRIMPROC
(imfi_add, "imfi-add", 3, 0, 0,
 (SCM src1, SCM src2, SCM dst),
 doc: /***********
D = saturation255 (S1 + S2).  */)
{
#define FUNC_NAME s_imfi_add
  S1S2D_DECL_AND_CHECK ();
  RETURN_TRUE_IF_0 (SDL_imageFilterAdd (s1, s2, d, len));
#undef FUNC_NAME
}


PRIMPROC
(imfi_mean, "imfi-mean", 3, 0, 0,
 (SCM src1, SCM src2, SCM dst),
 doc: /***********
D = S1/2 + S2/2.  */)
{
#define FUNC_NAME s_imfi_mean
  S1S2D_DECL_AND_CHECK ();
  RETURN_TRUE_IF_0 (SDL_imageFilterMean (s1, s2, d, len));
#undef FUNC_NAME
}


PRIMPROC
(imfi_sub, "imfi-sub", 3, 0, 0,
 (SCM src1, SCM src2, SCM dst),
 doc: /***********
D = saturation0 (S1 - S2).  */)
{
#define FUNC_NAME s_imfi_sub
  S1S2D_DECL_AND_CHECK ();
  RETURN_TRUE_IF_0 (SDL_imageFilterSub (s1, s2, d, len));
#undef FUNC_NAME
}


PRIMPROC
(imfi_absdiff, "imfi-abs-diff", 3, 0, 0,
 (SCM src1, SCM src2, SCM dst),
 doc: /***********
D = | S1 - S2 |.  */)
{
#define FUNC_NAME s_imfi_absdiff
  S1S2D_DECL_AND_CHECK ();
  RETURN_TRUE_IF_0 (SDL_imageFilterAbsDiff (s1, s2, d, len));
#undef FUNC_NAME
}


PRIMPROC
(imfi_mult, "imfi-mult", 3, 0, 0,
 (SCM src1, SCM src2, SCM dst),
 doc: /***********
D = saturation (S1 * S2).  */)
{
#define FUNC_NAME s_imfi_mult
  S1S2D_DECL_AND_CHECK ();
  RETURN_TRUE_IF_0 (SDL_imageFilterMult (s1, s2, d, len));
#undef FUNC_NAME
}


PRIMPROC
(imfi_mulnor, "imfi-mulnor", 3, 0, 0,
 (SCM src1, SCM src2, SCM dst),
 doc: /***********
D = S1 * S2 (non-MMX).  */)
{
#define FUNC_NAME s_imfi_mulnor
  S1S2D_DECL_AND_CHECK ();
  RETURN_TRUE_IF_0 (SDL_imageFilterMultNor (s1, s2, d, len));
#undef FUNC_NAME
}


PRIMPROC
(imfi_muldiv2, "imfi-muldiv2", 3, 0, 0,
 (SCM src1, SCM src2, SCM dst),
 doc: /***********
D = saturation255 (S1/2 * S2).  */)
{
#define FUNC_NAME s_imfi_muldiv2
  S1S2D_DECL_AND_CHECK ();
  RETURN_TRUE_IF_0 (SDL_imageFilterMultDivby2 (s1, s2, d, len));
#undef FUNC_NAME
}


PRIMPROC
(imfi_muldiv4, "imfi-muldiv4", 3, 0, 0,
 (SCM src1, SCM src2, SCM dst),
 doc: /***********
D = saturation255 (S1/2 * S2/2).  */)
{
#define FUNC_NAME s_imfi_muldiv4
  S1S2D_DECL_AND_CHECK ();
  RETURN_TRUE_IF_0 (SDL_imageFilterMultDivby4 (s1, s2, d, len));
#undef FUNC_NAME
}


PRIMPROC
(imfi_logand, "imfi-logand", 3, 0, 0,
 (SCM src1, SCM src2, SCM dst),
 doc: /***********
D = S1 & S2.  */)
{
#define FUNC_NAME s_imfi_logand
  S1S2D_DECL_AND_CHECK ();
  RETURN_TRUE_IF_0 (SDL_imageFilterBitAnd (s1, s2, d, len));
#undef FUNC_NAME
}


PRIMPROC
(imfi_logior, "imfi-logior", 3, 0, 0,
 (SCM src1, SCM src2, SCM dst),
 doc: /***********
D = S1 | S2.  */)
{
#define FUNC_NAME s_imfi_logior
  S1S2D_DECL_AND_CHECK ();
  RETURN_TRUE_IF_0 (SDL_imageFilterBitOr (s1, s2, d, len));
#undef FUNC_NAME
}


PRIMPROC
(imfi_div, "imfi-div", 3, 0, 0,
 (SCM src1, SCM src2, SCM dst),
 doc: /***********
D = S1 / S2 (non-MMX).  */)
{
#define FUNC_NAME s_imfi_div
  S1S2D_DECL_AND_CHECK ();
  RETURN_TRUE_IF_0 (SDL_imageFilterDiv (s1, s2, d, len));
#undef FUNC_NAME
}


PRIMPROC
(imfi_not, "imfi-not", 2, 0, 0,
 (SCM src, SCM dst),
 doc: /***********
D = !S.  */)
{
#define FUNC_NAME s_imfi_not
  SD_DECL_AND_CHECK ();
  RETURN_TRUE_IF_0 (SDL_imageFilterBitNegation (s, d, len));
#undef FUNC_NAME
}


PRIMPROC
(imfi_add_c, "imfi-add-c", 3, 0, 0,
 (SCM src, SCM dst, SCM c),
 doc: /***********
D = saturation255 (S + C).  */)
{
#define FUNC_NAME s_imfi_add_c
  unsigned int cc;
  SD_DECL_AND_CHECK ();
  ASSERT_NUMBER (c, 3);
  cc = C_INT (c);
  RETURN_TRUE_IF_0 (~0xffUL & cc
                    ? SDL_imageFilterAddUint (s, d, len, cc)
                    : SDL_imageFilterAddByte (s, d, len, cc));
#undef FUNC_NAME
}


PRIMPROC
(imfi_add_c_to_half, "imfi-add-c-to-half", 3, 0, 0,
 (SCM src, SCM dst, SCM c),
 doc: /***********
D = saturation255 (S/2 + C).  */)
{
#define FUNC_NAME s_imfi_add_c_to_half
  unsigned int cc;
  SD_DECL_AND_CHECK ();
  ASSERT_NUMBER (c, 3);
  cc = C_INT (c);
  RETURN_TRUE_IF_0 (SDL_imageFilterAddByteToHalf (s, d, len, cc));
#undef FUNC_NAME
}


PRIMPROC
(imfi_sub_c, "imfi-sub-c", 3, 0, 0,
 (SCM src, SCM dst, SCM c),
 doc: /***********
D = saturation0 (S - C).  */)
{
#define FUNC_NAME s_imfi_sub_c
  unsigned int cc;
  SD_DECL_AND_CHECK ();
  ASSERT_NUMBER (c, 3);
  cc = C_INT (c);
  RETURN_TRUE_IF_0 (~0xffUL & cc
                    ? SDL_imageFilterSubUint (s, d, len, cc)
                    : SDL_imageFilterSubByte (s, d, len, cc));
#undef FUNC_NAME
}


PRIMPROC
(imfi_ashr, "imfi-ashr", 3, 0, 0,
 (SCM src, SCM dst, SCM n),
 doc: /***********
D = saturation0 (S >> N).  */)
{
#define FUNC_NAME s_imfi_ashr
  unsigned int cn;
  SD_DECL_AND_CHECK ();
  ASSERT_NUMBER (n, 3);
  cn = C_INT (n);
  RETURN_TRUE_IF_0 (SDL_imageFilterShiftRight (s, d, len, cn));
#undef FUNC_NAME
}


PRIMPROC
(imfi_lshr, "imfi-lshr", 3, 0, 0,
 (SCM src, SCM dst, SCM n),
 doc: /***********
D = saturation0 ((uint) S >> N).  */)
{
#define FUNC_NAME s_imfi_lshr
  unsigned int cn;
  SD_DECL_AND_CHECK ();
  ASSERT_NUMBER (n, 3);
  cn = C_INT (n);
  RETURN_TRUE_IF_0 (SDL_imageFilterShiftRightUint (s, d, len, cn));
#undef FUNC_NAME
}


PRIMPROC
(imfi_mul_c, "imfi-mul-c", 3, 0, 0,
 (SCM src, SCM dst, SCM c),
 doc: /***********
D = saturation255 (S * C).  */)
{
#define FUNC_NAME s_imfi_mul_c
  unsigned int cc;
  SD_DECL_AND_CHECK ();
  ASSERT_NUMBER (c, 3);
  cc = C_INT (c);
  RETURN_TRUE_IF_0 (SDL_imageFilterMultByByte (s, d, len, cc));
#undef FUNC_NAME
}


PRIMPROC
(imfi_ashr_mul_c, "imfi-ashr-mul-c", 4, 0, 0,
 (SCM src, SCM dst, SCM n, SCM c),
 doc: /***********
D = saturation255 ((S >> N) * C).  */)
{
#define FUNC_NAME s_imfi_ashr_mul_c
  unsigned int cn, cc;
  SD_DECL_AND_CHECK ();
  ASSERT_NUMBER (n, 3);
  ASSERT_NUMBER (c, 4);
  cn = C_INT (n);
  cc = C_INT (c);
  RETURN_TRUE_IF_0
    (SDL_imageFilterShiftRightAndMultByByte (s, d, len, cn, cc));
#undef FUNC_NAME
}


PRIMPROC
(imfi_bshl, "imfi-bshl", 3, 0, 0,
 (SCM src, SCM dst, SCM n),
 doc: /***********
D = (S << N).  */)
{
#define FUNC_NAME s_imfi_bshl
  unsigned int cn;
  SD_DECL_AND_CHECK ();
  ASSERT_NUMBER (n, 3);
  cn = C_INT (n);
  RETURN_TRUE_IF_0 (SDL_imageFilterShiftLeftByte (s, d, len, cn));
#undef FUNC_NAME
}


PRIMPROC
(imfi_lshl, "imfi-lshl", 3, 0, 0,
 (SCM src, SCM dst, SCM n),
 doc: /***********
D = ((uint) S << N).  */)
{
#define FUNC_NAME s_imfi_lshl
  unsigned int cn;
  SD_DECL_AND_CHECK ();
  ASSERT_NUMBER (n, 3);
  cn = C_INT (n);
  RETURN_TRUE_IF_0 (SDL_imageFilterShiftLeftUint (s, d, len, cn));
#undef FUNC_NAME
}


PRIMPROC
(imfi_ashl, "imfi-ashl", 3, 0, 0,
 (SCM src, SCM dst, SCM n),
 doc: /***********
D = saturation255 (S << N).  */)
{
#define FUNC_NAME s_imfi_ashl
  unsigned int cn;
  SD_DECL_AND_CHECK ();
  ASSERT_NUMBER (n, 3);
  cn = C_INT (n);
  RETURN_TRUE_IF_0 (SDL_imageFilterShiftLeft (s, d, len, cn));
#undef FUNC_NAME
}


PRIMPROC
(imfi_binarize, "imfi-binarize", 3, 0, 0,
 (SCM src, SCM dst, SCM t),
 doc: /***********
D = (S < T ? 0 : 255).  */)
{
#define FUNC_NAME s_imfi_binarize
  unsigned int ct;
  SD_DECL_AND_CHECK ();
  ASSERT_NUMBER (t, 3);
  ct = C_INT (t);
  RETURN_TRUE_IF_0 (SDL_imageFilterBinarizeUsingThreshold (s, d, len, ct));
#undef FUNC_NAME
}


PRIMPROC
(imfi_clip, "imfi-clip", 4, 0, 0,
 (SCM src, SCM dst, SCM tmin, SCM tmax),
 doc: /***********
D = (Tmin <= S <= Tmax) ? 255 : 0.  */)
{
#define FUNC_NAME s_imfi_clip
  unsigned int ctmin, ctmax;
  SD_DECL_AND_CHECK ();
  ASSERT_NUMBER (tmin, 3);
  ASSERT_NUMBER (tmax, 4);
  ctmin = C_INT (tmin);
  ctmax = C_INT (tmax);
  RETURN_TRUE_IF_0 (SDL_imageFilterClipToRange (s, d, len, ctmin, ctmax));
#undef FUNC_NAME
}


PRIMPROC
(imfi_normalize_linear, "imfi-normalize-linear", 6, 0, 0,
 (SCM src, SCM dst, SCM cmin, SCM cmax, SCM nmin, SCM nmax),
 doc: /***********
D = saturation255 ((Nmax - Nmin) / (Cmax - Cmin) * (S - Cmin) + Nmin).  */)
{
#define FUNC_NAME s_imfi_normalize_linear
  unsigned int ccmin, ccmax, cnmin, cnmax;
  SD_DECL_AND_CHECK ();
  ASSERT_NUMBER (cmin, 3);
  ASSERT_NUMBER (cmax, 4);
  ASSERT_NUMBER (nmin, 5);
  ASSERT_NUMBER (nmax, 6);
  ccmin = C_INT (cmin);
  ccmax = C_INT (cmax);
  cnmin = C_INT (nmin);
  cnmax = C_INT (nmax);
  RETURN_TRUE_IF_0
    (SDL_imageFilterNormalizeLinear (s, d, len, ccmin, ccmax, cnmin, cnmax));
#undef FUNC_NAME
}


/* Funcs for which there is no C implementation.
 *
 * ConvolveKernel3x3Divide
 * ConvolveKernel5x5Divide
 * ConvolveKernel7x7Divide
 * ConvolveKernel9x9Divide
 * ConvolveKernel3x3ShiftRight
 * ConvolveKernel5x5ShiftRight
 * ConvolveKernel7x7ShiftRight
 * ConvolveKernel9x9ShiftRight
 * SobelX
 * SobelXShiftRight
 * AlignStack
 * RestoreStack
 */



/* dispatch */

static
void
init_module (void)
{
  DEFSMOB (fpsmgr_tag, "SDL-FPS-Manager",
           NULL,
           free_fpsmgr,
           print_fpsmgr);

#include "sdlgfx.x"

  btw = UNPACK_POINTER (CALL0 (obtw));

  {
    kp_init_t allp[] = {
      { &frot_enum, &frot_kp }
    };

    REGISTER_KP_V (allp);
  }
}

MOD_INIT_LINK_THUNK ("sdl gfx", sdl_gfx, init_module)

/* sdlgfx.c ends here */
