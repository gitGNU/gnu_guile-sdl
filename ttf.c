/*******************************************************************
 *  ttf.c -- SDL_ttf for Guile                                     *
 *                                                                 *
 *  Created:    <2001-06-11 18:03:28 foof>                         *
 *  Time-stamp: <2001-06-11 23:02:54 foof>                         *
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

#include "ttf.h"
#include "video.h"

/* smob tags */
long ttf_font_tag;

SCM
ttf_init (void)
{
   return (TTF_Init () ? SCM_BOOL_T : SCM_BOOL_F);
}

SCM
ttf_load_font (SCM file, SCM ptsize)
{
   TTF_Font *font;

   SCM_ASSERT ((SCM_NIMP (file) && SCM_STRINGP (file)),
               file, SCM_ARG1, "load-font");
   SCM_ASSERT (SCM_INUMP (ptsize), ptsize, SCM_ARG2, "load-font");

   font = TTF_OpenFont (SCM_CHARS (file), SCM_INUM (ptsize));
   SCM_RETURN_NEWSMOB (ttf_font_tag, font);
}

/* Set and retrieve the font style
   This font style is implemented by modifying the font glyphs, and
   doesn't reflect any inherent properties of the truetype font file.
*/
SCM
ttf_get_font_style (SCM s_font)
{
   TTF_Font *font;

   SCM_ASSERT_SMOB (s_font, ttf_font_tag, SCM_ARG1, "font:style");
   font = (TTF_Font*) SCM_SMOB_DATA (s_font);

   return SCM_MAKINUM (TTF_GetFontStyle (font));
}

SCM
ttf_set_font_style (SCM s_font, SCM s_style)
{
   TTF_Font *font;
   int style;

   SCM_ASSERT_SMOB (s_font, ttf_font_tag, SCM_ARG1, "font:set-style!");
   SCM_ASSERT (SCM_INUMP (s_style), s_style, SCM_ARG2, "font:set-style!");

   font = (TTF_Font*) SCM_SMOB_DATA (s_font);
   style = SCM_INUM (s_style);

   TTF_SetFontStyle (font, style);
   return SCM_UNSPECIFIED;
}

/* Get the total height of the font - usually equal to point size */
SCM
ttf_font_height (SCM s_font)
{
   TTF_Font *font;

   SCM_ASSERT_SMOB (s_font, ttf_font_tag, SCM_ARG1, "font:height");
   font = (TTF_Font*) SCM_SMOB_DATA (s_font);

   return SCM_MAKINUM (TTF_FontHeight (font));
}

/* Get the offset from the baseline to the top of the font
   This is a positive value, relative to the baseline.
 */
SCM
ttf_font_ascent (SCM s_font)
{
   TTF_Font *font;

   SCM_ASSERT_SMOB (s_font, ttf_font_tag, SCM_ARG1, "font:ascent");
   font = (TTF_Font*) SCM_SMOB_DATA (s_font);

   return SCM_MAKINUM (TTF_FontAscent (font));
}

/* Get the offset from the baseline to the bottom of the font
   This is a negative value, relative to the baseline.
 */
SCM
ttf_font_descent (SCM s_font)
{
   TTF_Font *font;

   SCM_ASSERT_SMOB (s_font, ttf_font_tag, SCM_ARG1, "font:descent");
   font = (TTF_Font*) SCM_SMOB_DATA (s_font);

   return SCM_MAKINUM (TTF_FontDescent (font));
}

/* Get the recommended spacing between lines of text for this font */
SCM
ttf_font_line_skip (SCM s_font)
{
   TTF_Font *font;

   SCM_ASSERT_SMOB (s_font, ttf_font_tag, SCM_ARG1, "font:line-skip");
   font = (TTF_Font*) SCM_SMOB_DATA (s_font);

   return SCM_MAKINUM (TTF_FontLineSkip (font));
}

/* Get the metrics (dimensions) of a glyph */
SCM
ttf_glyph_metrics (SCM s_font, SCM s_ch)
{
   TTF_Font *font;
   Uint16 ch;
   int minx, maxx, miny, maxy, advance;

   SCM_ASSERT_SMOB (s_font, ttf_font_tag, SCM_ARG1, "font:glyph-metrics");
   SCM_ASSERT (SCM_CHARP (s_ch), s_ch, SCM_ARG2, "font:glyph-metrics");

   font = (TTF_Font*) SCM_SMOB_DATA (s_font);
   ch = SCM_CHAR (s_ch);

   TTF_GlyphMetrics(font, ch, &minx, &maxx, &miny, &maxy, &advance);

   return SCM_LIST5 (SCM_MAKINUM (minx),   SCM_MAKINUM (maxx),
                     SCM_MAKINUM (miny),   SCM_MAKINUM (maxy),
                     SCM_MAKINUM (advance));
}

/* Get the dimensions of a rendered string of text */
SCM
ttf_size_text (SCM s_font, SCM s_text)
{
   TTF_Font *font;
   char *text;
   int w, h;

   SCM_ASSERT_SMOB (s_font, ttf_font_tag, SCM_ARG1, "font:size-text");
   SCM_ASSERT ((SCM_NIMP (s_text) && SCM_STRINGP (s_text)),
               s_text, SCM_ARG2, "font:size-text");

   font = (TTF_Font*) SCM_SMOB_DATA (s_font);
   text = SCM_CHARS (s_text);

   TTF_SizeText (font, text, &w, &h);
   return SCM_LIST2 (SCM_MAKINUM (w), SCM_MAKINUM (h));
}

SCM
ttf_size_utf8 (SCM s_font, SCM s_text)
{
   TTF_Font *font;
   char *text;
   int w, h;

   SCM_ASSERT_SMOB (s_font, ttf_font_tag, SCM_ARG1, "font:size-utf8");
   SCM_ASSERT ((SCM_NIMP (s_text) && SCM_STRINGP (s_text)),
               s_text, SCM_ARG2, "font:size-utf8");

   font = (TTF_Font*) SCM_SMOB_DATA (s_font);
   text = SCM_CHARS (s_text);

   TTF_SizeUTF8 (font, text, &w, &h);
   return SCM_LIST2 (SCM_MAKINUM (w), SCM_MAKINUM (h));
}

/* SCM */
/* ttf_size_unicode (SCM font, SCM text) */
/* { */
/* } */

SCM
ttf_render_text (SCM s_font, SCM s_text, SCM s_fg, SCM s_bg)
{
   TTF_Font *font;
   SDL_Color *fg, *bg;
   SDL_Surface *surface;
   char *text;

   SCM_ASSERT_SMOB (s_font, ttf_font_tag, SCM_ARG1, "render-text");
   SCM_ASSERT ((SCM_NIMP (s_text) && SCM_STRINGP (s_text)),
               s_text, SCM_ARG2, "render-text");
   SCM_ASSERT_SMOB (s_fg, color_tag, SCM_ARG3, "render-text");

   font = (TTF_Font*) SCM_SMOB_DATA (s_font);
   text = SCM_CHARS (s_text);
   fg = (SDL_Color*) SCM_SMOB_DATA (s_fg);

   if ((s_bg == SCM_UNDEFINED) || SCM_FALSEP (s_bg)) {
      /* printf ("TTF_RenderTextSolid(\"%s\");\n", text); */
      surface = TTF_RenderText_Solid (font, text, *fg);
   } else if (SCM_BOOLP (s_bg)) {
      /* printf ("TTF_RenderTextBlended(\"%s\");\n", text); */
      surface = TTF_RenderText_Blended (font, text, *fg);
   } else {
      /* printf ("TTF_RenderTextShaded(\"%s\");\n", text); */
      SCM_ASSERT_SMOB (s_bg, color_tag, SCM_ARG4, "render-text");
      bg = (SDL_Color*) SCM_SMOB_DATA (s_bg);
      surface = TTF_RenderText_Shaded (font, text, *fg, *bg);
   }

   SCM_RETURN_NEWSMOB (surface_tag, surface);
}

SCM
ttf_render_utf8 (SCM s_font, SCM s_text, SCM s_fg, SCM s_bg)
{
   TTF_Font *font;
   SDL_Color *fg, *bg;
   SDL_Surface *surface;
   char *text;

   SCM_ASSERT_SMOB (s_font, ttf_font_tag, SCM_ARG1, "render-utf8");
   SCM_ASSERT ((SCM_NIMP (s_text) && SCM_STRINGP (s_text)),
               s_text, SCM_ARG2, "render-utf8");
   SCM_ASSERT_SMOB (s_fg, color_tag, SCM_ARG3, "render-utf8");

   font = (TTF_Font*) SCM_SMOB_DATA (s_font);
   text = SCM_CHARS (s_text);
   fg = (SDL_Color*) SCM_SMOB_DATA (s_fg);

   if ((s_bg == SCM_UNDEFINED) || SCM_FALSEP (s_bg)) {
      surface = TTF_RenderUTF8_Solid (font, text, *fg);
   } else if (SCM_BOOLP (s_bg)) {
      surface = TTF_RenderUTF8_Blended (font, text, *fg);
   } else {
      SCM_ASSERT_SMOB (s_bg, color_tag, SCM_ARG4, "render-utf8");
      bg = (SDL_Color*) SCM_SMOB_DATA (s_bg);
      surface = TTF_RenderUTF8_Shaded (font, text, *fg, *bg);
   }

   SCM_RETURN_NEWSMOB (surface_tag, surface);
}

/* SCM */
/* ttf_render_unicode_solid (SCM font, SCM text, SCM fg) */
/* { */
/* } */

/* Create an 8-bit palettized surface and render the given glyph at
   fast quality with the given font and color.  The 0 pixel is the
   colorkey, giving a transparent background, and the 1 pixel is set
   to the text color.  The glyph is rendered without any padding or
   centering in the X direction, and aligned normally in the Y direction.
   This function returns the new surface, or NULL if there was an error.
*/
SCM
ttf_render_glyph (SCM s_font, SCM s_ch, SCM s_fg, SCM s_bg)
{
   TTF_Font *font;
   SDL_Color *fg, *bg;
   SDL_Surface *surface;
   char ch;

   SCM_ASSERT_SMOB (s_font, ttf_font_tag, SCM_ARG1, "render-glyph");
   SCM_ASSERT (SCM_CHARP (s_ch), s_ch, SCM_ARG2, "render-glyph");
   SCM_ASSERT_SMOB (s_fg, color_tag, SCM_ARG3, "render-glyph");

   font = (TTF_Font*) SCM_SMOB_DATA (s_font);
   ch = SCM_CHAR (s_ch);
   fg = (SDL_Color*) SCM_SMOB_DATA (s_fg);

   if ((s_bg == SCM_UNDEFINED) || SCM_FALSEP (s_bg)) {
      surface = TTF_RenderGlyph_Solid (font, ch, *fg);
   } else if (SCM_BOOLP (s_bg)) {
      surface = TTF_RenderGlyph_Blended (font, ch, *fg);
   } else {
      SCM_ASSERT_SMOB (s_bg, color_tag, SCM_ARG4, "render-glyph");
      bg = (SDL_Color*) SCM_SMOB_DATA (s_bg);
      surface = TTF_RenderGlyph_Shaded (font, ch, *fg, *bg);
   }

   SCM_RETURN_NEWSMOB (surface_tag, surface);
}


/* Close an opened font file */
scm_sizet
free_font (SCM s_font)
{
   TTF_Font *font = (TTF_Font*) SCM_SMOB_DATA (s_font);
   TTF_CloseFont (font);
   return sizeof (struct TTF_Font*);
}

/* De-initialize the TTF engine */
SCM
ttf_quit (void)
{
   TTF_Quit ();
   return SCM_UNSPECIFIED;
}


/* initialize the mixer subsystem */
void
sdl_ttf_init (void)
{
   /* smobs */
   ttf_font_tag = scm_make_smob_type ("font", sizeof (struct TTF_Font*));
   scm_set_smob_free (ttf_font_tag, free_font);

   /* functions */
   scm_c_define_gsubr ("ttf-init",            0, 0, 0, ttf_init);
   scm_c_define_gsubr ("ttf-quit",            0, 0, 0, ttf_quit);
   scm_c_define_gsubr ("load-font",           2, 0, 0, ttf_load_font);
   scm_c_define_gsubr ("font:style",          1, 0, 0, ttf_get_font_style);
   scm_c_define_gsubr ("font:set-style!",     2, 0, 0, ttf_set_font_style);
   scm_c_define_gsubr ("font:height",         1, 0, 0, ttf_font_height);
   scm_c_define_gsubr ("font:ascent",         1, 0, 0, ttf_font_ascent);
   scm_c_define_gsubr ("font:descent",        1, 0, 0, ttf_font_descent);
   scm_c_define_gsubr ("font:line-skip",      1, 0, 0, ttf_font_line_skip);
   scm_c_define_gsubr ("font:glyph-metrics",  2, 0, 0, ttf_glyph_metrics);
   scm_c_define_gsubr ("font:size-text",      2, 0, 0, ttf_size_text);
   scm_c_define_gsubr ("font:size-utf8",      2, 0, 0, ttf_size_utf8);
   scm_c_define_gsubr ("render-text",         3, 1, 0, ttf_render_text);
   scm_c_define_gsubr ("render-utf8",         3, 1, 0, ttf_render_utf8);
   scm_c_define_gsubr ("render-glyph",        3, 1, 0, ttf_render_glyph);

   /* exported symbols */
   scm_c_export (
      "ttf-init",             "ttf-quit",           "load-font",
      "font:style",           "font:set-style!",    "font:height",
      "font:ascent",          "font:descent",       "font:line-skip",
      "font:glyph-metrics",   "font:size-text",     "font:size-utf8",
      "render-text",          "render-utf8",        "render-glyph",
      NULL);
}

