/*******************************************************************
 *  sdlrect.c -- SDL Rect functions for Guile                      *
 *                                                                 *
 *  Copyright (C) 2001 Alex Shinn                                  *
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

/* guile headers */
#include <libguile.h>
/* sdl headers */
#include <SDL/SDL.h>
#include <SDL/SDL_image.h>
/* scm util headers */
#include "sdlenums.h"
#include "sdlsmobs.h"
#include "sdlrect.h"

/* tags for SDL smobs */
long rect_tag;

/* smob functions */

size_t
free_rect (SCM rect)
{
  /* printf ("free_rect(%p)\n", rect); */
  free ((SDL_Rect*) SCM_SMOB_DATA (rect));
  /* return sizeof (SDL_Rect); */
  return 0;
}

int
print_rect (SCM rect_smob, SCM port, scm_print_state *pstate)
{
  SDL_Rect *rect = (SDL_Rect *) SCM_SMOB_DATA (rect_smob);
  SCM x,y,w,h;

  x = scm_long2num (rect->x);
  y = scm_long2num (rect->y);
  w = scm_long2num (rect->w);
  h = scm_long2num (rect->h);

  scm_puts ("#<SDL-Rect ", port);
  scm_display(w, port);
  scm_puts ("x", port);
  scm_display(h, port);
  scm_puts ("+", port);
  scm_display(x, port);
  scm_puts ("+", port);
  scm_display(y, port);
  scm_puts (">", port);

  /* non-zero means success */
  return 1;
}

SCM
make_rect (SCM s_x, SCM s_y, SCM s_w, SCM s_h)
{
  SDL_Rect *rect;

  SCM_ASSERT (scm_exact_p (s_x),  s_x,  SCM_ARG1, "sdl-make-rect");
  SCM_ASSERT (scm_exact_p (s_y),  s_y,  SCM_ARG2, "sdl-make-rect");
  SCM_ASSERT (scm_exact_p (s_w),  s_w,  SCM_ARG3, "sdl-make-rect");
  SCM_ASSERT (scm_exact_p (s_h),  s_h,  SCM_ARG4, "sdl-make-rect");

  rect = (SDL_Rect *) scm_must_malloc (sizeof (SDL_Rect), "sdl-make-rect");
  rect->x = scm_num2long (s_x, SCM_ARG1, "sdl-make-rect");
  rect->y = scm_num2long (s_y, SCM_ARG2, "sdl-make-rect");
  rect->w = scm_num2long (s_w, SCM_ARG3, "sdl-make-rect");
  rect->h = scm_num2long (s_h, SCM_ARG4, "sdl-make-rect");

  SCM_RETURN_NEWSMOB (rect_tag, rect);
}

/* rect getters */
SCM_DEFINE_NUMBER_GETTER ("sdl-rect:x", rect_x, rect_tag, SDL_Rect*, x)
SCM_DEFINE_NUMBER_GETTER ("sdl-rect:y", rect_y, rect_tag, SDL_Rect*, y)
SCM_DEFINE_NUMBER_GETTER ("sdl-rect:w", rect_w, rect_tag, SDL_Rect*, w)
SCM_DEFINE_NUMBER_GETTER ("sdl-rect:h", rect_h, rect_tag, SDL_Rect*, h)

/* rect setters */
SCM_DEFINE_NUMBER_SETTER ("sdl-rect:set-x!", rect_set_x, rect_tag, SDL_Rect*, x)
SCM_DEFINE_NUMBER_SETTER ("sdl-rect:set-y!", rect_set_y, rect_tag, SDL_Rect*, y)
SCM_DEFINE_NUMBER_SETTER ("sdl-rect:set-w!", rect_set_w, rect_tag, SDL_Rect*, w)
SCM_DEFINE_NUMBER_SETTER ("sdl-rect:set-h!", rect_set_h, rect_tag, SDL_Rect*, h)


void
sdl_init_rect (void)
{
  /* smobs */
  rect_tag = scm_make_smob_type ("SDL-Rect", sizeof(SDL_Rect));
  scm_set_smob_free (rect_tag, free_rect);
  scm_set_smob_print (rect_tag, print_rect);

  /* rect functions */
  scm_c_define_gsubr ("sdl-make-rect",          4, 0, 0, make_rect);
  scm_c_define_gsubr ("sdl-rect:x",             1, 0, 0, rect_x);
  scm_c_define_gsubr ("sdl-rect:y",             1, 0, 0, rect_y);
  scm_c_define_gsubr ("sdl-rect:w",             1, 0, 0, rect_w);
  scm_c_define_gsubr ("sdl-rect:h",             1, 0, 0, rect_h);
  scm_c_define_gsubr ("sdl-rect:set-x!",        2, 0, 0, rect_set_x);
  scm_c_define_gsubr ("sdl-rect:set-y!",        2, 0, 0, rect_set_y);
  scm_c_define_gsubr ("sdl-rect:set-w!",        2, 0, 0, rect_set_w);
  scm_c_define_gsubr ("sdl-rect:set-h!",        2, 0, 0, rect_set_h);

  /* exported symbols */
  scm_c_export (
    "sdl-make-rect",   "sdl-rect:x",      "sdl-rect:y",    "sdl-rect:w",
    "sdl-rect:h",      "sdl-rect:set-x!", "sdl-rect:set-y!",
    "sdl-rect:set-w!", "sdl-rect:set-h!",
    NULL);
}

