/*******************************************************************
 *  sdlttf.c -- SDL_ttf for Guile                                  *
 *                                                                 *
 *  Created:    <2001-06-11 18:03:28 foof>                         *
 *  Time-stamp: <01/11/25 12:51:12 foof>                         *
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

#include "sdlttf.h"
#include "sdlvideo.h"

/* smob tags */
long ttf_font_tag;
SCM sdl_ttf_flags;

SCM_DEFINE( ttf_init, "sdl-ttf-init", 0, 0, 0,
            (),
"Initialize the SDL_ttf subsystem.")
#define FUNC_NAME s_ttf_init
{
  return (TTF_Init () ? SCM_BOOL_T : SCM_BOOL_F);
}
#undef FUNC_NAME


SCM_DEFINE( ttf_load_font, "sdl-load-font", 2, 0, 0,
            (SCM file,
             SCM ptsize),
"Load a font from a file given a point size.")
#define FUNC_NAME s_ttf_load_font
{
  TTF_Font *font;

  SCM_ASSERT ((SCM_NIMP (file) && SCM_STRINGP (file)),
              file, SCM_ARG1, "sdl-load-font");
  SCM_ASSERT (scm_exact_p (ptsize), ptsize, SCM_ARG2, "sdl-load-font");

  font = TTF_OpenFont (SCM_CHARS (file), scm_num2long (ptsize, SCM_ARG1, "scm_num2long"));
  SCM_RETURN_NEWSMOB (ttf_font_tag, font);
}
#undef FUNC_NAME


SCM_DEFINE( ttf_get_font_style, "sdl-font:style", 1, 0, 0,
            (SCM s_font),
"Retrieve the font's style.
This font style is implemented by modifying the font glyphs, and
doesn't reflect any inherent properties of the truetype font file.")
#define FUNC_NAME s_ttf_get_font_style
{ 
  TTF_Font *font; 

  SCM_ASSERT_SMOB (s_font, ttf_font_tag, SCM_ARG1, "sdl-font:style"); 
  font = (TTF_Font*) SCM_SMOB_DATA (s_font); 

  return scm_ulong2flags (TTF_GetFontStyle (font), sdl_ttf_flags); 
} 
#undef FUNC_NAME


SCM_DEFINE( ttf_set_font_style, "sdl-font:set-style!", 2, 0, 0,
            (SCM s_font,
             SCM s_style),
"Set the font's style.
This font style is implemented by modifying the font glyphs, and
doesn't reflect any inherent properties of the truetype font file.")
#define FUNC_NAME s_ttf_set_font_style
{
  TTF_Font *font;
  int style;

  SCM_ASSERT_SMOB (s_font, ttf_font_tag, SCM_ARG1, "sdl-font:set-style!");
  /* SCM_ASSERT (scm_exact_p (s_style), s_style, SCM_ARG2, "sdl-font:set-style!"); */

  font = (TTF_Font*) SCM_SMOB_DATA (s_font);
  style = scm_flags2ulong (s_style, sdl_ttf_flags, SCM_ARG1, "scm_num2long");

  TTF_SetFontStyle (font, style);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE( ttf_font_height, "sdl-font:height", 1, 0, 0,
            (SCM s_font),
"Get the total height of the font - usually equal to point size.")
#define FUNC_NAME s_ttf_font_height
{
  TTF_Font *font;

  SCM_ASSERT_SMOB (s_font, ttf_font_tag, SCM_ARG1, "sdl-font:height");
  font = (TTF_Font*) SCM_SMOB_DATA (s_font);

  return scm_long2num (TTF_FontHeight (font));
}
#undef FUNC_NAME


SCM_DEFINE( ttf_font_ascent, "sdl-font:ascent", 1, 0, 0,
            (SCM s_font),
"Get the offset from the baseline to the top of the font.
This is a positive value, relative to the baseline.")
#define FUNC_NAME s_ttf_font_ascent
{
  TTF_Font *font;

  SCM_ASSERT_SMOB (s_font, ttf_font_tag, SCM_ARG1, "sdl-font:ascent");
  font = (TTF_Font*) SCM_SMOB_DATA (s_font);

  return scm_long2num (TTF_FontAscent (font));
}
#undef FUNC_NAME


SCM_DEFINE( ttf_font_descent, "sdl-font:descent", 1, 0, 0,
            (SCM s_font),
"Get the offset from the baseline to the bottom of the font.
This is a negative value, relative to the baseline.")
#define FUNC_NAME s_ttf_font_descent
{
  TTF_Font *font;

  SCM_ASSERT_SMOB (s_font, ttf_font_tag, SCM_ARG1, "sdl-font:descent");
  font = (TTF_Font*) SCM_SMOB_DATA (s_font);

  return scm_long2num (TTF_FontDescent (font));
}
#undef FUNC_NAME


SCM_DEFINE( ttf_font_line_skip, "sdl-font:line-skip", 1, 0, 0,
            (SCM s_font),
"Get the recommended spacing between lines of text for this font.")
#define FUNC_NAME s_ttf_font_line_skip
{
  TTF_Font *font;

  SCM_ASSERT_SMOB (s_font, ttf_font_tag, SCM_ARG1, "sdl-font:line-skip");
  font = (TTF_Font*) SCM_SMOB_DATA (s_font);

  return scm_long2num (TTF_FontLineSkip (font));
}
#undef FUNC_NAME


SCM_DEFINE( ttf_glyph_metrics, "sdl-font:glyph-metrics", 2, 0, 0,
            (SCM s_font,
             SCM s_ch),
"Get the metrics (dimensions) of a glyph as an alist.")
#define FUNC_NAME s_ttf_glyph_metrics
{
  TTF_Font *font;
  Uint16 ch;
  int minx, maxx, miny, maxy, advance;

  SCM_ASSERT_SMOB (s_font, ttf_font_tag, SCM_ARG1, "sdl-font:glyph-metrics");
  SCM_ASSERT (SCM_CHARP (s_ch), s_ch, SCM_ARG2, "sdl-font:glyph-metrics");

  font = (TTF_Font*) SCM_SMOB_DATA (s_font);
  ch = SCM_CHAR (s_ch);

  TTF_GlyphMetrics(font, ch, &minx, &maxx, &miny, &maxy, &advance);

  return SCM_LIST5 (scm_cons (scm_str2symbol ("minx"), scm_long2num (minx)),
                    scm_cons (scm_str2symbol ("maxx"), scm_long2num (maxx)),
                    scm_cons (scm_str2symbol ("miny"), scm_long2num (miny)),
                    scm_cons (scm_str2symbol ("maxy"), scm_long2num (maxy)),
                    scm_cons (scm_str2symbol ("advance"),
                              scm_long2num (advance)));
}
#undef FUNC_NAME


SCM_DEFINE( ttf_size_text, "sdl-font:size-text", 2, 0, 0,
            (SCM s_font,
             SCM s_text),
"Get the dimensions of a rendered string of text as an alist.")
#define FUNC_NAME s_ttf_size_text
{
  TTF_Font *font;
  char *text;
  int w, h;

  SCM_ASSERT_SMOB (s_font, ttf_font_tag, SCM_ARG1, "sdl-font:size-text");
  SCM_ASSERT ((SCM_NIMP (s_text) && SCM_STRINGP (s_text)),
              s_text, SCM_ARG2, "sdl-font:size-text");

  font = (TTF_Font*) SCM_SMOB_DATA (s_font);
  text = SCM_CHARS (s_text);

  TTF_SizeText (font, text, &w, &h);
  return SCM_LIST2 (scm_cons (scm_str2symbol ("w"), scm_long2num (w)),
                    scm_cons (scm_str2symbol ("h"), scm_long2num (h)));
}
#undef FUNC_NAME


SCM_DEFINE( ttf_size_utf8, "sdl-font:size-utf8", 2, 0, 0,
            (SCM s_font,
             SCM s_text),
"Get the dimensions of a rendered utf8 string of text as an alist.")
#define FUNC_NAME s_ttf_size_utf8
{
  TTF_Font *font;
  char *text;
  int w, h;

  SCM_ASSERT_SMOB (s_font, ttf_font_tag, SCM_ARG1, "sdl-font:size-utf8");
  SCM_ASSERT ((SCM_NIMP (s_text) && SCM_STRINGP (s_text)),
              s_text, SCM_ARG2, "sdl-font:size-utf8");

  font = (TTF_Font*) SCM_SMOB_DATA (s_font);
  text = SCM_CHARS (s_text);

  TTF_SizeUTF8 (font, text, &w, &h);
  return SCM_LIST2 (scm_long2num (w), scm_long2num (h));
}
#undef FUNC_NAME

/* SCM */
/* ttf_size_unicode (SCM font, SCM text) */
/* { */
/* } */


SCM_DEFINE( ttf_render_text, "sdl-render-text", 3, 1, 0,
            (SCM s_font,
             SCM s_text,
             SCM s_fg,
             SCM s_bg),
"Return a new surface containing text rendered in a font.
Third argument is the foreground color; optional fourth argument
is the background color, or #t if the text is to be blended.")
#define FUNC_NAME s_ttf_render_text
{
  TTF_Font *font;
  SDL_Color *fg, *bg;
  SDL_Surface *surface;
  char *text;

  SCM_ASSERT_SMOB (s_font, ttf_font_tag, SCM_ARG1, "sdl-render-text");
  SCM_ASSERT ((SCM_NIMP (s_text) && SCM_STRINGP (s_text)),
              s_text, SCM_ARG2, "sdl-render-text");
  SCM_ASSERT_SMOB (s_fg, color_tag, SCM_ARG3, "sdl-render-text");

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
    SCM_ASSERT_SMOB (s_bg, color_tag, SCM_ARG4, "sdl-render-text");
    bg = (SDL_Color*) SCM_SMOB_DATA (s_bg);
    surface = TTF_RenderText_Shaded (font, text, *fg, *bg);
  }

  SCM_RETURN_NEWSMOB (surface_tag, surface);
}
#undef FUNC_NAME


SCM_DEFINE( ttf_render_utf8, "sdl-render-utf8", 3, 1, 0,
            (SCM s_font,
             SCM s_text,
             SCM s_fg,
             SCM s_bg),
"Return a new surface containing a utf8 string rendered in a font.
Third argument is the foreground color; optional fourth argument
is the background color, or #t if the text is to be blended.")
#define FUNC_NAME s_ttf_render_utf8
{
  TTF_Font *font;
  SDL_Color *fg, *bg;
  SDL_Surface *surface;
  char *text;

  SCM_ASSERT_SMOB (s_font, ttf_font_tag, SCM_ARG1, "sdl-render-utf8");
  SCM_ASSERT ((SCM_NIMP (s_text) && SCM_STRINGP (s_text)),
              s_text, SCM_ARG2, "sdl-render-utf8");
  SCM_ASSERT_SMOB (s_fg, color_tag, SCM_ARG3, "sdl-render-utf8");

  font = (TTF_Font*) SCM_SMOB_DATA (s_font);
  text = SCM_CHARS (s_text);
  fg = (SDL_Color*) SCM_SMOB_DATA (s_fg);

  if ((s_bg == SCM_UNDEFINED) || SCM_FALSEP (s_bg)) {
    surface = TTF_RenderUTF8_Solid (font, text, *fg);
  } else if (SCM_BOOLP (s_bg)) {
    surface = TTF_RenderUTF8_Blended (font, text, *fg);
  } else {
    SCM_ASSERT_SMOB (s_bg, color_tag, SCM_ARG4, "sdl-render-utf8");
    bg = (SDL_Color*) SCM_SMOB_DATA (s_bg);
    surface = TTF_RenderUTF8_Shaded (font, text, *fg, *bg);
  }

  SCM_RETURN_NEWSMOB (surface_tag, surface);
}
#undef FUNC_NAME

/* SCM */
/* ttf_render_unicode_solid (SCM font, SCM text, SCM fg) */
/* { */
/* } */


SCM_DEFINE( ttf_render_glyph, "sdl-render-glyph", 3, 1, 0,
            (SCM s_font,
             SCM s_ch,
             SCM s_fg,
             SCM s_bg),
"Return a new surface containing a char rendered in a font.
Third argument is the foreground color; optional fourth argument
is the background color, or #t if the text is to be blended.")
#define FUNC_NAME s_ttf_render_glyph
{
  TTF_Font *font;
  SDL_Color *fg, *bg;
  SDL_Surface *surface;
  char ch;

  SCM_ASSERT_SMOB (s_font, ttf_font_tag, SCM_ARG1, "sdl-render-glyph");
  SCM_ASSERT (SCM_CHARP (s_ch), s_ch, SCM_ARG2, "sdl-render-glyph");
  SCM_ASSERT_SMOB (s_fg, color_tag, SCM_ARG3, "sdl-render-glyph");

  font = (TTF_Font*) SCM_SMOB_DATA (s_font);
  ch = SCM_CHAR (s_ch);
  fg = (SDL_Color*) SCM_SMOB_DATA (s_fg);

  if ((s_bg == SCM_UNDEFINED) || SCM_FALSEP (s_bg)) {
    surface = TTF_RenderGlyph_Solid (font, ch, *fg);
  } else if (SCM_BOOLP (s_bg)) {
    surface = TTF_RenderGlyph_Blended (font, ch, *fg);
  } else {
    SCM_ASSERT_SMOB (s_bg, color_tag, SCM_ARG4, "sdl-render-glyph");
    bg = (SDL_Color*) SCM_SMOB_DATA (s_bg);
    surface = TTF_RenderGlyph_Shaded (font, ch, *fg, *bg);
  }

  SCM_RETURN_NEWSMOB (surface_tag, surface);
}
#undef FUNC_NAME


/* Close an opened font file */
size_t
free_font (SCM s_font)
{
  TTF_Font *font = (TTF_Font*) SCM_SMOB_DATA (s_font);
  TTF_CloseFont (font);
  return sizeof (struct TTF_Font*);
}


SCM_DEFINE( ttf_quit, "sdl-ttf-quit", 0, 0, 0,
            (),
"Quit the SDL_ttf subsystem.")
#define FUNC_NAME s_ttf_quit
{
  TTF_Quit ();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


/* initialize the ttf subsystem */
void
sdl_ttf_init (void)
{
  /* smobs */
  ttf_font_tag = scm_make_smob_type ("font", sizeof (struct TTF_Font*));
  scm_set_smob_free (ttf_font_tag, free_font);

  /* init flags */
  sdl_ttf_flags = scm_c_define_flag (
    "sdl-ttf-flags",
    "TTF_STYLE_NORMAL",      TTF_STYLE_NORMAL,
    "TTF_STYLE_BOLD",        TTF_STYLE_BOLD,
    "TTF_STYLE_ITALIC",      TTF_STYLE_ITALIC,
    "TTF_STYLE_UNDERLINE",   TTF_STYLE_UNDERLINE,
    NULL);

  /* exported symbols */
  scm_c_export (
    "sdl-ttf-flags",
    "sdl-ttf-init",             "sdl-ttf-quit",           "sdl-load-font",
    "sdl-font:style",           "sdl-font:set-style!",    "sdl-font:height",
    "sdl-font:ascent",          "sdl-font:descent",       "sdl-font:line-skip",
    "sdl-font:glyph-metrics",   "sdl-font:size-text",     "sdl-font:size-utf8",
    "sdl-render-text",          "sdl-render-utf8",        "sdl-render-glyph",
    NULL);

#ifndef SCM_MAGIC_SNARFER
#include "sdlttf.x"
#endif

}

