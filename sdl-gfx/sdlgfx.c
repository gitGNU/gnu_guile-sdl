/*******************************************************************
 *  gfx.c -- Additional Graphics functions for Guile SDL           *
 *                                                                 *
 *  Created:    <2001-06-03 02:00:32 foof>                         *
 *  Time-stamp: <2001-08-04 19:43:42 foof>                         *
 *                                                                 *
 *  This program is free software; you can redistribute it and/or  *
 * modify it under the terms of the GNU General Public License as  *
 * published by the Free Software Foundation; either version 2 of  *
 * the License, or (at your option) any later version.             *
 *                                                                 *
 * This program is distributed in the hope that it will be useful, *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of  *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the   *
 * GNU General Public License for more details.                    *
 *                                                                 *
 * You should have received a copy of the GNU General Public       *
 * License along with this program; if not, write to the Free      *
 * Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,  *
 * MA 02111-1307 USA                                               *
 ******************************************************************/

#include <guile/gh.h>
#include <libguile.h>
#include <SDL_gfxPrimitives.h>
#include "sdlsmobs.h"
#include "sdlgfx.h"


SCM_DEFINE( sdl_draw_point, "sdl-draw-point", 4, 0, 0,
            (SCM s_surface,
             SCM s_x,
             SCM s_y,
             SCM s_color),
"Draws a single point on a surface.")
#define FUNC_NAME s_sdl_draw_point
{
  SDL_Surface *surface;
  int ret;

  SCM_ASSERT_SMOB (s_surface, surface_tag, SCM_ARG1, "sdl-draw-point");
  SCM_ASSERT (gh_exact_p (s_x), s_x, SCM_ARG2, "sdl-draw-point");
  SCM_ASSERT (gh_exact_p (s_y), s_y, SCM_ARG3, "sdl-draw-point");
  SCM_ASSERT (gh_exact_p (s_color), s_color, SCM_ARG4, "sdl-draw-point");
  
  surface = (SDL_Surface *) SCM_CDR (s_surface);

  ret = pixelColor (surface, gh_scm2long (s_x), gh_scm2long (s_y),
		    gh_scm2ulong (s_color));

  return gh_long2scm (ret);
}
#undef FUNC_NAME


SCM_DEFINE( sdl_draw_line, "sdl-draw-line", 6, 0, 0,
            (SCM s_surface,
             SCM s_x1,
             SCM s_y1,
             SCM s_x2,
             SCM s_y2,
             SCM s_color),
"Draws a line segment on a surface.")
#define FUNC_NAME s_sdl_draw_line
{
  SDL_Surface *surface;
  int ret;
  
  SCM_ASSERT_SMOB (s_surface, surface_tag, SCM_ARG1, "sdl-draw-line");
  SCM_ASSERT (gh_exact_p (s_x1), s_x1, SCM_ARG2, "sdl-draw-line");
  SCM_ASSERT (gh_exact_p (s_y1), s_y1, SCM_ARG3, "sdl-draw-line");
  SCM_ASSERT (gh_exact_p (s_x2), s_x2, SCM_ARG4, "sdl-draw-line");
  SCM_ASSERT (gh_exact_p (s_y2), s_y2, SCM_ARG5, "sdl-draw-line");
  SCM_ASSERT (gh_exact_p (s_color), s_color, SCM_ARG6, "sdl-draw-line");
  
  surface = (SDL_Surface *) SCM_CDR (s_surface);
  
  ret = lineColor (surface, 
		   gh_scm2long (s_x1), gh_scm2long (s_y1),
		   gh_scm2long (s_x2), gh_scm2long (s_y2),
		   gh_scm2ulong (s_color));
  
  return gh_long2scm (ret);
}
#undef FUNC_NAME


SCM_DEFINE( sdl_draw_aa_line, "sdl-draw-aa-line", 6, 0, 0,
            (SCM s_surface,
             SCM s_x1,
             SCM s_y1,
             SCM s_x2,
             SCM s_y2,
             SCM s_color),
"Draws an anti-aliased line segment on a surface.")
#define FUNC_NAME s_sdl_draw_aa_line
{
  SDL_Surface *surface;
  int ret;
  
  SCM_ASSERT_SMOB (s_surface, surface_tag, SCM_ARG1, "sdl-draw-aa-line");
  SCM_ASSERT (gh_exact_p (s_x1), s_x1, SCM_ARG2, "sdl-draw-aa-line");
  SCM_ASSERT (gh_exact_p (s_y1), s_y1, SCM_ARG3, "sdl-draw-aa-line");
  SCM_ASSERT (gh_exact_p (s_x2), s_x2, SCM_ARG4, "sdl-draw-aa-line");
  SCM_ASSERT (gh_exact_p (s_y2), s_y2, SCM_ARG5, "sdl-draw-aa-line");
  SCM_ASSERT (gh_exact_p (s_color), s_color, SCM_ARG6, "sdl-draw-aa-line");
  
  surface = (SDL_Surface *) SCM_CDR (s_surface);
  
  ret = aalineColor (surface, 
		     gh_scm2long (s_x1), gh_scm2long (s_y1),
		     gh_scm2long (s_x2), gh_scm2long (s_y2),
		     gh_scm2ulong (s_color));
  
  return gh_long2scm (ret);
}
#undef FUNC_NAME


SCM_DEFINE( sdl_draw_rectangle, "sdl-draw-rectangle", 6, 1, 0,
            (SCM s_surface,
             SCM s_x1,
             SCM s_y1,
             SCM s_x2,
             SCM s_y2,
             SCM s_color,
             SCM s_fill),
"Draws a rectangle (optionally filled) on a surface.")
#define FUNC_NAME s_sdl_draw_rectangle
{
  SDL_Surface *surface;
  int ret;
  
  SCM_ASSERT_SMOB (s_surface, surface_tag, SCM_ARG1, "sdl-draw-rectangle");
  SCM_ASSERT (gh_exact_p (s_x1), s_x1, SCM_ARG2, "sdl-draw-rectangle");
  SCM_ASSERT (gh_exact_p (s_y1), s_y1, SCM_ARG3, "sdl-draw-rectangle");
  SCM_ASSERT (gh_exact_p (s_x2), s_x2, SCM_ARG4, "sdl-draw-rectangle");
  SCM_ASSERT (gh_exact_p (s_y2), s_y2, SCM_ARG5, "sdl-draw-rectangle");
  SCM_ASSERT (gh_exact_p (s_color), s_color, SCM_ARG6, "sdl-draw-rectangle");
  
  surface = (SDL_Surface *) SCM_CDR (s_surface);
  
  if ((s_fill == SCM_UNDEFINED) || (s_fill == SCM_BOOL_F)) {
    ret = rectangleColor (surface, 
			  gh_scm2long (s_x1), gh_scm2long (s_y1),
			  gh_scm2long (s_x2), gh_scm2long (s_y2),
			  gh_scm2ulong (s_color));
  }
  else {
    ret = boxColor (surface, 
		    gh_scm2long (s_x1), gh_scm2long (s_y1),
		    gh_scm2long (s_x2), gh_scm2long (s_y2),
		    gh_scm2ulong (s_color));
  }
  
  return gh_long2scm (ret);
}
#undef FUNC_NAME


SCM_DEFINE( sdl_draw_circle, "sdl-draw-circle", 5, 1, 0,
            (SCM s_surface,
             SCM s_x,
             SCM s_y,
             SCM s_r,
             SCM s_color,
             SCM s_fill),
"Draws a circle (optionally filled) on a surface.")
#define FUNC_NAME s_sdl_draw_circle
{
  SDL_Surface *surface;
  int ret;
  
  SCM_ASSERT_SMOB (s_surface, surface_tag, SCM_ARG1, "sdl-draw-circle");
  SCM_ASSERT (gh_exact_p (s_x), s_x, SCM_ARG2, "sdl-draw-circle");
  SCM_ASSERT (gh_exact_p (s_y), s_y, SCM_ARG3, "sdl-draw-circle");
  SCM_ASSERT (gh_exact_p (s_r), s_r, SCM_ARG4, "sdl-draw-circle");
  SCM_ASSERT (gh_exact_p (s_color), s_color, SCM_ARG5, "sdl-draw-circle");
  
  surface = (SDL_Surface *) SCM_CDR (s_surface);
  
  if ((s_fill == SCM_UNDEFINED) || (s_fill == SCM_BOOL_F)) {
    ret = circleColor (surface, 
		       gh_scm2long (s_x), gh_scm2long (s_y),
		       gh_scm2long (s_r),
		       gh_scm2ulong (s_color));
  }
  else {
    ret = filledCircleColor (surface, 
			     gh_scm2long (s_x), gh_scm2long (s_y),
			     gh_scm2long (s_r),
			     gh_scm2ulong (s_color));
  }
  
  return gh_long2scm (ret);
}
#undef FUNC_NAME


SCM_DEFINE( sdl_draw_ellipse, "sdl-draw-ellipse", 6, 1, 0,
            (SCM s_surface,
             SCM s_x,
             SCM s_y,
             SCM s_rx,
             SCM s_ry,
             SCM s_color,
             SCM s_fill),
"Draws an ellipse (optionally filled) on a surface.")
#define FUNC_NAME s_sdl_draw_ellipse
{
  SDL_Surface *surface;
  int ret;
    
  SCM_ASSERT (SMOB_SURFACEP(s_surface), s_surface, SCM_ARG1,
	      "sdl-draw-ellipse");
  SCM_ASSERT (gh_exact_p (s_x), s_x, SCM_ARG2, "sdl-draw-ellipse");
  SCM_ASSERT (gh_exact_p (s_y), s_y, SCM_ARG3, "sdl-draw-ellipse");
  SCM_ASSERT (gh_exact_p (s_rx), s_rx, SCM_ARG4, "sdl-draw-ellipse");
  SCM_ASSERT (gh_exact_p (s_ry), s_ry, SCM_ARG5, "sdl-draw-ellipse");
  SCM_ASSERT (gh_exact_p (s_color), s_color, SCM_ARG6, "sdl-draw-ellipse");
  
  surface = (SDL_Surface *) SCM_CDR (s_surface);
  
  if ((s_fill == SCM_UNDEFINED) || (s_fill == SCM_BOOL_F)) {
    ret = ellipseColor (surface, 
			gh_scm2long (s_x), gh_scm2long (s_y),
			gh_scm2long (s_rx), gh_scm2long (s_ry),
			gh_scm2ulong (s_color));
  }
  else {
    ret = filledEllipseColor (surface, 
			      gh_scm2long (s_x), gh_scm2long (s_y),
			      gh_scm2long (s_rx), gh_scm2long (s_ry),
			      gh_scm2ulong (s_color));
  }
  
  return gh_long2scm (ret);
}
#undef FUNC_NAME


SCM_DEFINE( sdl_draw_polygon, "sdl-draw-polygon", 4, 1, 0,
            (SCM s_surface,
             SCM s_vx,
             SCM s_vy,
             SCM s_color,
             SCM s_fill),
"Draws a polygon (optionally filled) on a surface.")
#define FUNC_NAME s_sdl_draw_polygon
{
  SDL_Surface *surface;
  int ret;
  Sint16 *vx, *vy;
  int n;
  
  SCM_ASSERT (SMOB_SURFACEP(s_surface), s_surface, SCM_ARG1,
	      "sdl-draw-polygon");
  SCM_ASSERT (gh_vector_p (s_vx), s_vx, SCM_ARG2, "sdl-draw-polygon");
  SCM_ASSERT (gh_vector_p (s_vy), s_vy, SCM_ARG3, "sdl-draw-polygon");
  SCM_ASSERT (gh_exact_p (s_color), s_color, SCM_ARG4, "sdl-draw-polygon");
  
  surface = (SDL_Surface *) SCM_CDR (s_surface);

  n = gh_uniform_vector_length (s_vx);
  vx = (Sint16 *) malloc (n * sizeof (Sint16));

  n = gh_uniform_vector_length (s_vy);
  vy = (Sint16 *) malloc (n * sizeof (Sint16));

  gh_scm2shorts (s_vx, vx);
  gh_scm2shorts (s_vy, vy);

  if ((s_fill == SCM_UNDEFINED) || (s_fill == SCM_BOOL_F)) {
    ret = polygonColor (surface, vx, vy, n, gh_scm2ulong (s_color));
  }
  else {
    ret = filledPolygonColor (surface, vx, vy, n, gh_scm2ulong (s_color));
  }
  
  free (vx);
  free (vy);
  return gh_long2scm (ret);
}
#undef FUNC_NAME


/* SCM_DEFINE( sdl_draw_character, "sdl-draw-character", 5, 0, 0, */
/*             (SCM s_surface, */
/*              SCM s_x, */
/*              SCM s_y, */
/*              SCM s_char, */
/*              SCM s_color), */
/* "Draws a character on a surface.") */
/* #define FUNC_NAME s_sdl_draw_character */
/* { */
/*   SDL_Surface *surface; */
/*   int ret; */
  
/*   SCM_ASSERT (SMOB_SURFACEP(s_surface), s_surface, SCM_ARG1,  */
/* 	      "sdl-draw-character"); */
/*   SCM_ASSERT (gh_exact_p (s_x), s_x, SCM_ARG2, "sdl-draw-character"); */
/*   SCM_ASSERT (gh_exact_p (s_y), s_y, SCM_ARG3, "sdl-draw-character"); */
/*   SCM_ASSERT (gh_char_p  (s_char), s_char, SCM_ARG4, "sdl-draw-character"); */
/*   SCM_ASSERT (gh_exact_p (s_color), s_color, SCM_ARG5, "sdl-draw-character"); */
  
/*   surface = (SDL_Surface *) SCM_CDR (s_surface); */

/*   ret = characterColor (surface, gh_scm2long (s_x), gh_scm2long (s_y), */
/* 			gh_scm2char (s_char), gh_scm2ulong (s_color)); */
  
/*   return gh_long2scm (ret); */
/* } */
/* #undef FUNC_NAME */


/* SCM_DEFINE( sdl_draw_string, "sdl-draw-string", 5, 0, 0, */
/*             (SCM s_surface, */
/*              SCM s_x, */
/*              SCM s_y, */
/*              SCM s_string, */
/*              SCM s_color), */
/* "Draws a string on a surface.") */
/* #define FUNC_NAME s_sdl_draw_string */
/* { */
/*   SDL_Surface *surface; */
/*   int ret; */
/*   char *str; */
  
/*   SCM_ASSERT (SMOB_SURFACEP(s_surface), s_surface, SCM_ARG1,  */
/* 	      "sdl-draw-string"); */
/*   SCM_ASSERT (gh_exact_p (s_x), s_x, SCM_ARG2, "sdl-draw-string"); */
/*   SCM_ASSERT (gh_exact_p (s_y), s_y, SCM_ARG3, "sdl-draw-string"); */
/*   SCM_ASSERT (gh_string_p  (s_string), s_string, SCM_ARG4, "sdl-draw-string"); */
/*   SCM_ASSERT (gh_exact_p (s_color), s_color, SCM_ARG5, "sdl-draw-string"); */
  
/*   surface = (SDL_Surface *) SCM_CDR (s_surface); */
/*   str = gh_scm2newstr (s_string, NULL); */

/*   ret = stringColor (surface, gh_scm2long (s_x), gh_scm2long (s_y), */
/* 		     str, gh_scm2ulong (s_color)); */
  
/*   free (str); */
/*   return gh_long2scm (ret); */
/* } */
/* #undef FUNC_NAME */

void
sdl_gfx_init (void)
{
   /* Primitives */
/*    scm_c_define_gsubr ("sdl-draw-point", 4, 0, 0, sdl_draw_point);	 */
/*    scm_c_define_gsubr ("sdl-draw-line", 6, 0, 0, sdl_draw_line); */
/*    scm_c_define_gsubr ("sdl-draw-aa-line", 6, 0, 0, sdl_draw_aa_line); */

   /* Fillable primitives */
/*    scm_c_define_gsubr ("sdl-draw-rectangle", 7, 0, 0, sdl_draw_rectangle); */
/*    scm_c_define_gsubr ("sdl-draw-circle", 6, 0, 0, sdl_draw_circle); */
/*    scm_c_define_gsubr ("sdl-draw-ellipse", 7, 0, 0, sdl_draw_ellipse);   */
/*    scm_c_define_gsubr ("sdl-draw-polygon", 5, 0, 0, sdl_draw_polygon);     */

   /* exported symbols */
   scm_c_export (
      "sdl-draw-point",
      "sdl-draw-line",
      "sdl-draw-aa-line",
      "sdl-draw-rectangle",
      "sdl-draw-circle",
      "sdl-draw-ellipse",
      "sdl-draw-polygon",
/*       "sdl-draw-character", */
/*       "sdl-draw-string", */
      NULL);

#ifndef SCM_MAGIC_SNARFER
#include "sdlgfx.x"
#endif

}

