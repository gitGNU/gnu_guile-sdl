/*******************************************************************
 *  ttf.h -- SDL_ttf for Guile                                     *
 *                                                                 *
 *  Created:    <2001-06-11 18:03:28 foof>                         *
 *  Time-stamp: <01/11/25 12:51:08 foof>                         *
 *  Author:     Alex Shinn <foof@debian.org>                       *
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

#ifndef _GUILE_SDL_TTF_H
#define _GUILE_SDL_TTF_H

/* guile headers */
#include <libguile.h>
/* sdl headers */
#include <SDL/SDL.h>
#include <SDL/SDL_ttf.h>
#include "sdlenums.h"
#include "sdlsmobs.h"

/* smob tags */
extern long ttf_font_tag;
extern SCM sdl_ttf_flags;

/* Initialize the TTF engine - returns 0 if successful, -1 on error */
SCM ttf_init (void);

/* Open a font file and create a font of the specified point size */
SCM ttf_load_font (SCM file, SCM ptsize);

/* Set and retrieve the font style
   This font style is implemented by modifying the font glyphs, and
   doesn't reflect any inherent properties of the truetype font file.
*/
SCM ttf_get_font_style (SCM font);
SCM ttf_set_font_style (SCM font, SCM style);

/* Get the total height of the font - usually equal to point size */
SCM ttf_font_height (SCM font);

/* Get the offset from the baseline to the top of the font
   This is a positive value, relative to the baseline.
 */
SCM ttf_font_ascent (SCM font);

/* Get the offset from the baseline to the bottom of the font
   This is a negative value, relative to the baseline.
 */
SCM ttf_font_descent (SCM font);

/* Get the recommended spacing between lines of text for this font */
SCM ttf_font_line_skip (SCM font);

/* Get the metrics (dimensions) of a glyph */
SCM ttf_glyph_metrics (SCM font, SCM ch);

/* Get the dimensions of a rendered string of text */
SCM ttf_size_text (SCM font, SCM text);
SCM ttf_size_utf8 (SCM font, SCM text);
/* SCM ttf_size_unicode (SCM font, SCM text); */

/* Create an 8-bit palettized surface and render the given text at
   fast quality with the given font and color.  The 0 pixel is the
   colorkey, giving a transparent background, and the 1 pixel is set
   to the text color.
   This function returns the new surface, or NULL if there was an error.
*/
SCM ttf_render_text (SCM font, SCM text, SCM fg, SCM bg);
SCM ttf_render_utf8 (SCM font, SCM text, SCM fg, SCM bg);
/* SCM ttf_render_unicode (SCM font, SCM text, SCM fg, SCM bg); */

/* Create an 8-bit palettized surface and render the given glyph at
   fast quality with the given font and color.  The 0 pixel is the
   colorkey, giving a transparent background, and the 1 pixel is set
   to the text color.  The glyph is rendered without any padding or
   centering in the X direction, and aligned normally in the Y direction.
   This function returns the new surface, or NULL if there was an error.
*/
SCM ttf_render_glyph (SCM font, SCM ch, SCM fg, SCM bg);

/* Close an opened font file */
size_t free_font (SCM font);

/* De-initialize the TTF engine */
SCM ttf_quit (void);


/* initialize the mixer subsystem */
void sdl_ttf_init (void);

#endif /* ! _GUILE_SDL_TTF_H */
