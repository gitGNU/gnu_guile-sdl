/*******************************************************************
 *  sdlcolor.c -- SDL Color functions for Guile                    *
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
/* scm util headers */
#include "sdlenums.h"
#include "sdlsmobs.h"
#include "sdlsurface.h"

/* tags for SDL smobs */
long color_tag;

/* smob function */
size_t
free_color (SCM s_color)
{
  /* printf ("free_color(%p)\n", color); */
  free ((SDL_Color*) SCM_SMOB_DATA (s_color));
  return sizeof (SDL_Color);
}

int
print_color (SCM s_color, SCM port, scm_print_state *pstate)
{
  SDL_Color *color = (SDL_Color *) SCM_SMOB_DATA (s_color);
  SCM r,g,b;

  r = scm_long2num (color->r);
  g = scm_long2num (color->g);
  b = scm_long2num (color->b);

  scm_puts ("#<SDL-Color r=", port);
  scm_display(r, port);
  scm_puts (" g=", port);
  scm_display(g, port);
  scm_puts (" b=", port);
  scm_display(b, port);
  scm_puts (">", port);

  /* non-zero means success */
  return 1;
}


/* functions */

SCM
make_color (SCM s_r, SCM s_g, SCM s_b)
{
  SDL_Color *color;

  SCM_ASSERT (scm_exact_p (s_r),  s_r,  SCM_ARG1, "sdl-make-color");
  SCM_ASSERT (scm_exact_p (s_g),  s_g,  SCM_ARG2, "sdl-make-color");
  SCM_ASSERT (scm_exact_p (s_b),  s_b,  SCM_ARG3, "sdl-make-color");

  color = (SDL_Color *) scm_must_malloc (sizeof (SDL_Color), "sdl-make-color");
  color->r = scm_num2long (s_r, SCM_ARG1, "sdl-make-color");
  color->g = scm_num2long (s_g, SCM_ARG2, "sdl-make-color");
  color->b = scm_num2long (s_b, SCM_ARG3, "sdl-make-color");

  SCM_RETURN_NEWSMOB (color_tag, color);
}

/* color getters */
SCM_DEFINE_NUMBER_GETTER ("sdl-color:r", color_r, color_tag, SDL_Color*, r)
SCM_DEFINE_NUMBER_GETTER ("sdl-color:g", color_g, color_tag, SDL_Color*, g)
SCM_DEFINE_NUMBER_GETTER ("sdl-color:b", color_b, color_tag, SDL_Color*, b)

/* color setters */
SCM_DEFINE_NUMBER_SETTER ("sdl-color:set-r!", color_set_r, color_tag, SDL_Color*, r)
SCM_DEFINE_NUMBER_SETTER ("sdl-color:set-g!", color_set_g, color_tag, SDL_Color*, g)
SCM_DEFINE_NUMBER_SETTER ("sdl-color:set-b!", color_set_b, color_tag, SDL_Color*, b)


void
sdl_init_color (void)
{
  /* smobs */
  color_tag = scm_make_smob_type_mfpe ("SDL-Color",
                                       sizeof(SDL_Color),
                                       NULL, 
                                       free_color, 
                                       print_color, 
                                       NULL);

  /* color functions */
  scm_c_define_gsubr ("sdl-make-color",         3, 0, 0, make_color);
  scm_c_define_gsubr ("sdl-color:r",            1, 0, 0, color_r);
  scm_c_define_gsubr ("sdl-color:g",            1, 0, 0, color_g);
  scm_c_define_gsubr ("sdl-color:b",            1, 0, 0, color_b);
  scm_c_define_gsubr ("sdl-color:set-r!",       2, 0, 0, color_set_r);
  scm_c_define_gsubr ("sdl-color:set-g!",       2, 0, 0, color_set_g);
  scm_c_define_gsubr ("sdl-color:set-b!",       2, 0, 0, color_set_b);

  /* exported symbols */
  scm_c_export (
    "sdl-make-color",   "sdl-color:r",  "sdl-color:g",  "sdl-color:b",
    "sdl-color:set-r!", "sdl-color:set-g!", "sdl-color:set-b!",
    NULL);
}

