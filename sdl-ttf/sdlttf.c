/* sdlttf.c --- SDL_ttf for Guile
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
#include <SDL/SDL_ttf.h>

#include "config.h"
#include "argcheck.h"
#include "sdlsmobs.h"
#include "sdlenums.h"

GH_USE_MODULE (sdlsup, "(sdl sdl-sup)"); /* for various gsdl_* C funcs */


static SCM ttf_flags;

GH_DEFPROC (get_ttf_flags, "flagstash:ttf", 0, 0, 0, (void),
            "Return the flagstash object for ttf flags.\n"
            "You can pass this object to proc @code{flagstash-flags}\n"
            "to get a list of its flags.")
{
  return ttf_flags;
}


/* smob tags */
static long ttf_font_tag;

#define ASSERT_TTFONT(obj,which) \
  ASSERT_SMOB (obj, ttf_font_tag, which)

#define UNPACK_TTFONT(smob) \
  SMOBGET (smob, TTF_Font *)

#define RETURN_NEW_TTFONT(x) \
  SCM_RETURN_NEWSMOB (ttf_font_tag, x)

static
SCM
mark_font (SCM s_font)
{
  return s_font;
}

static
size_t
free_font (SCM s_font)
{
  TTF_CloseFont (UNPACK_TTFONT (s_font));
  return sizeof (TTF_Font *);
}


GH_DEFPROC (ttf_init, "ttf-init", 0, 0, 0,
            (void),
            "Initialize the SDL_ttf subsystem.")
#define FUNC_NAME s_ttf_init
{
  return gh_bool2scm
    (TTF_Init ());
}
#undef FUNC_NAME


GH_DEFPROC (ttf_load_font, "load-font", 2, 0, 0,
            (SCM file, SCM ptsize),
            "Load a font from @var{file} with point size @var{ptsize}.\n"
            "Return a handle.")
#define FUNC_NAME s_ttf_load_font
{
  ASSERT_STRING (file, ARGH1);
  ASSERT_EXACT (ptsize, ARGH2);

  RETURN_NEW_TTFONT
    (TTF_OpenFont (SCM_CHARS (file),
                   gh_scm2long (ptsize)));
}
#undef FUNC_NAME


GH_DEFPROC (ttf_get_font_style, "font:style", 1, 0, 0,
            (SCM s_font),
            "Return the style of @var{font} (see @code{flagstash:ttf}).\n"
            "This font style is implemented by modifying the font glyphs, and\n"
            "doesn't reflect any inherent properties of the truetype font file.")
#define FUNC_NAME s_ttf_get_font_style
{
  ASSERT_TTFONT (s_font, ARGH1);

  return gsdl_ulong2flags (TTF_GetFontStyle (UNPACK_TTFONT (s_font)),
                           ttf_flags);
}
#undef FUNC_NAME


GH_DEFPROC (ttf_set_font_style, "font:set-style!", 2, 0, 0,
            (SCM s_font, SCM s_style),
            "Set @var{font} style to @var{style} (see @code{flagstash:ttf}).\n"
            "This font style is implemented by modifying the font glyphs, and\n"
            "doesn't reflect any inherent properties of the truetype font file.")
#define FUNC_NAME s_ttf_set_font_style
{
  int style;

  ASSERT_TTFONT (s_font, ARGH1);

  style = GSDL_FLAGS2ULONG (s_style, ttf_flags, ARGH2);

  TTF_SetFontStyle (UNPACK_TTFONT (s_font), style);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


GH_DEFPROC (ttf_font_height, "font:height", 1, 0, 0,
            (SCM s_font),
            "Return the total height of @var{font},\n"
            "usually equal to point size.")
#define FUNC_NAME s_ttf_font_height
{
  ASSERT_TTFONT (s_font, ARGH1);

  return gh_long2scm (TTF_FontHeight (UNPACK_TTFONT (s_font)));
}
#undef FUNC_NAME


GH_DEFPROC (ttf_font_ascent, "font:ascent", 1, 0, 0,
            (SCM s_font),
            "Return the offset from the baseline to the top of\n"
            "@var{font}.  This is a positive number.")
#define FUNC_NAME s_ttf_font_ascent
{
  ASSERT_TTFONT (s_font, ARGH1);

  return gh_long2scm (TTF_FontAscent (UNPACK_TTFONT (s_font)));
}
#undef FUNC_NAME


GH_DEFPROC (ttf_font_descent, "font:descent", 1, 0, 0,
            (SCM s_font),
            "Return the offset from the baseline to the bottom of\n"
            "@var{font}.  This is a negative number.")
#define FUNC_NAME s_ttf_font_descent
{
  ASSERT_TTFONT (s_font, ARGH1);

  return gh_long2scm (TTF_FontDescent (UNPACK_TTFONT (s_font)));
}
#undef FUNC_NAME


GH_DEFPROC (ttf_font_line_skip, "font:line-skip", 1, 0, 0,
            (SCM s_font),
            "Return the recommended spacing between lines of\n"
            "text for @var{font}.")
#define FUNC_NAME s_ttf_font_line_skip
{
  ASSERT_TTFONT (s_font, ARGH1);

  return gh_long2scm (TTF_FontLineSkip (UNPACK_TTFONT (s_font)));
}
#undef FUNC_NAME


SCM_SYMBOL (gsdl_sym_minx, "minx");
SCM_SYMBOL (gsdl_sym_maxx, "maxx");
SCM_SYMBOL (gsdl_sym_miny, "miny");
SCM_SYMBOL (gsdl_sym_maxy, "maxy");
SCM_SYMBOL (gsdl_sym_advance, "advance");

GH_DEFPROC (ttf_glyph_metrics, "font:glyph-metrics", 2, 0, 0,
            (SCM s_font, SCM s_ch),
            "Return the metrics (dimensions) of a glyph as an alist.\n"
            "The glyph is a @var{font}-specific rendering of char @var{ch}.\n"
            "Alist keys are: @code{minx}, @code{maxx}, @code{miny},\n"
            "@code{maxy} and @code{advance}.  Values are numbers.")
#define FUNC_NAME s_ttf_glyph_metrics
{
  int minx, maxx, miny, maxy, advance;

  ASSERT_TTFONT (s_font, ARGH1);
  ASSERT_CHAR (s_ch, ARGH2);

  TTF_GlyphMetrics (UNPACK_TTFONT (s_font),
                    (Uint16) gh_scm2long (s_ch),
                    &minx, &maxx, &miny, &maxy, &advance);

  return SCM_LIST5 (gh_cons (gsdl_sym_minx, gh_long2scm (minx)),
                    gh_cons (gsdl_sym_maxx, gh_long2scm (maxx)),
                    gh_cons (gsdl_sym_miny, gh_long2scm (miny)),
                    gh_cons (gsdl_sym_maxy, gh_long2scm (maxy)),
                    gh_cons (gsdl_sym_advance, gh_long2scm (advance)));
}
#undef FUNC_NAME


SCM_SYMBOL (gsdl_sym_w, "w");
SCM_SYMBOL (gsdl_sym_h, "h");

GH_DEFPROC (ttf_size_text, "font:size-text", 2, 0, 0,
            (SCM s_font, SCM s_text),
            "Return an alist with keys @code{w} and @code{h} and\n"
            "corresponding values (numbers) representing the width\n"
            "and height of the @var{font}-specific rendering of\n"
            "the string @var{text}.")
#define FUNC_NAME s_ttf_size_text
{
  int w, h;

  ASSERT_TTFONT (s_font, ARGH1);
  ASSERT_STRING (s_text, ARGH2);

  TTF_SizeText (UNPACK_TTFONT (s_font), SCM_CHARS (s_text), &w, &h);
  return SCM_LIST2 (gh_cons (gsdl_sym_w, gh_long2scm (w)),
                    gh_cons (gsdl_sym_h, gh_long2scm (h)));
}
#undef FUNC_NAME


GH_DEFPROC (ttf_size_utf8, "font:size-utf8", 2, 0, 0,
            (SCM s_font, SCM s_text),
            "Return an alist with keys @code{w} and @code{h} and\n"
            "corresponding values (numbers) representing the width\n"
            "and height of the @var{font}-specific rendering of\n"
            "the utf8 string @var{text}.")
#define FUNC_NAME s_ttf_size_utf8
{
  int w, h;

  ASSERT_TTFONT (s_font, ARGH1);
  ASSERT_STRING (s_text, ARGH2);

  TTF_SizeUTF8 (UNPACK_TTFONT (s_font), SCM_CHARS (s_text), &w, &h);
  return SCM_LIST2 (gh_cons (gsdl_sym_w, gh_long2scm (w)),
                    gh_cons (gsdl_sym_h, gh_long2scm (h)));
}
#undef FUNC_NAME


GH_DEFPROC (ttf_render_text, "render-text", 3, 1, 0,
            (SCM s_font, SCM s_text, SCM s_fg, SCM s_bg),
            "Return a new surface containing the @var{font}-specific\n"
            "rendering of the @var{text} string.\n"
            "Third argument is the foreground color;\n"
            "optional fourth argument is the background color,\n"
            "or #t if the text is to be blended.")
#define FUNC_NAME s_ttf_render_text
{
  TTF_Font *font;
  SDL_Color *fg;
  SDL_Surface *surface;
  char *text;

  ASSERT_TTFONT (s_font, ARGH1);
  ASSERT_STRING (s_text, ARGH2);
  ASSERT_COLOR (s_fg, ARGH3);

  font = UNPACK_TTFONT (s_font);
  text = SCM_CHARS (s_text);
  fg = UNPACK_COLOR (s_fg);

  UNBOUND_MEANS_FALSE (s_bg);

  if (SCM_FALSEP (s_bg)) {
    surface = TTF_RenderText_Solid (font, text, *fg);
  } else if (gh_eq_p (s_bg, SCM_BOOL_T)) {
    surface = TTF_RenderText_Blended (font, text, *fg);
  } else {
    ASSERT_COLOR (s_bg, ARGH4);
    surface = TTF_RenderText_Shaded (font, text, *fg, *(UNPACK_COLOR (s_bg)));
  }

  RETURN_NEW_SURFACE (surface);
}
#undef FUNC_NAME


GH_DEFPROC (ttf_render_utf8, "render-utf8", 3, 1, 0,
            (SCM s_font, SCM s_text, SCM s_fg, SCM s_bg),
            "Return a new surface containing a @var{font}-specific\n"
            "rendering of the utf8 string @var{text}.\n"
            "Third argument is the foreground color;\n"
            "optional fourth argument is the background color,\n"
            "or #t if the text is to be blended.")
#define FUNC_NAME s_ttf_render_utf8
{
  TTF_Font *font;
  SDL_Color *fg;
  SDL_Surface *surface;
  char *text;

  ASSERT_TTFONT (s_font, ARGH1);
  ASSERT_STRING (s_text, ARGH2);
  ASSERT_COLOR (s_fg, ARGH3);

  font = UNPACK_TTFONT (s_font);
  text = SCM_CHARS (s_text);
  fg = UNPACK_COLOR (s_fg);

  UNBOUND_MEANS_FALSE (s_bg);

  if (SCM_FALSEP (s_bg)) {
    surface = TTF_RenderUTF8_Solid (font, text, *fg);
  } else if (gh_eq_p (s_bg, SCM_BOOL_T)) {
    surface = TTF_RenderUTF8_Blended (font, text, *fg);
  } else {
    ASSERT_COLOR (s_bg, ARGH4);
    surface = TTF_RenderUTF8_Shaded (font, text, *fg, *(UNPACK_COLOR (s_bg)));
  }

  RETURN_NEW_SURFACE (surface);
}
#undef FUNC_NAME


GH_DEFPROC (ttf_render_glyph, "render-glyph", 3, 1, 0,
            (SCM s_font, SCM s_ch, SCM s_fg, SCM s_bg),
            "Return a new surface containing a @var{font}-specific\n"
            "rendering of the character @var{ch}.\n"
            "Third argument is the foreground color;\n"
            "optional fourth argument is the background color,\n"
            "or #t if the text is to be blended.")
#define FUNC_NAME s_ttf_render_glyph
{
  TTF_Font *font;
  SDL_Color *fg;
  SDL_Surface *surface;
  char ch;

  ASSERT_TTFONT (s_font, ARGH1);
  ASSERT_CHAR (s_ch, ARGH2);
  ASSERT_COLOR (s_fg, ARGH3);

  font = UNPACK_TTFONT (s_font);
  ch = gh_scm2char (s_ch);
  fg = UNPACK_COLOR (s_fg);

  UNBOUND_MEANS_FALSE (s_bg);

  if (SCM_FALSEP (s_bg)) {
    surface = TTF_RenderGlyph_Solid (font, ch, *fg);
  } else if (gh_eq_p (s_bg, SCM_BOOL_T)) {
    surface = TTF_RenderGlyph_Blended (font, ch, *fg);
  } else {
    ASSERT_COLOR (s_bg, ARGH4);
    surface = TTF_RenderGlyph_Shaded (font, ch, *fg, *(UNPACK_COLOR (s_bg)));
  }

  RETURN_NEW_SURFACE (surface);
}
#undef FUNC_NAME


GH_DEFPROC (ttf_quit, "ttf-quit", 0, 0, 0,
            (void),
            "Quit the SDL_ttf subsystem.")
#define FUNC_NAME s_ttf_quit
{
  TTF_Quit ();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



extern flagstash_t gsdl_ttf_flagstash;

/* initialize the ttf subsystem */
static
void
init_module (void)
{
  /* smobs */
  ttf_font_tag = scm_make_smob_type ("font", sizeof (struct TTF_Font*));
  scm_set_smob_mark (ttf_font_tag, mark_font);
  scm_set_smob_free (ttf_font_tag, free_font);

  /* init flags */
  ttf_flags = gsdl_make_flagstash (&gsdl_ttf_flagstash);

#include "sdlttf.x"
}

GH_MODULE_LINK_FUNC ("sdl ttf", sdl_ttf, init_module)

/* sdlttf.c ends here */
