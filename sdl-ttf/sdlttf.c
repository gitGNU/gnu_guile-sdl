/* sdlttf.c --- SDL_ttf for Guile
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
#include <SDL/SDL_ttf.h>

#include "config.h"
#include "argcheck.h"
#include "sdlsmobs.h"
#include "sdlenums.h"
#include "retval.h"
#include "sym.h"
#include "bool.h"

GH_USE_MODULE (sdlsup, "(sdl sdl)"); /* for various gsdl_* C funcs */


static SCM ttf_flags;

GH_DEFPROC (get_ttf_flags, "flagstash:ttf", 0, 0, 0, (void),
            "Return the flagstash object for ttf flags.\n"
            "You can pass this object to proc @code{flagstash-flags}\n"
            "to get a list of its flags.")
{
  return ttf_flags;
}


/* Smob tags.  */
static long ttf_font_tag;

#define ASSERT_TTFONT(obj,which) \
  ASSERT_SMOB (obj, ttf_font_tag, which)

#define UNPACK_TTFONT(smob) \
  SMOBGET (smob, TTF_Font *)

#define RETURN_NEW_TTFONT(x) \
  NEWSMOB_OR_FALSE (ttf_font_tag, x)

static
size_t
free_font (SCM font)
{
  TTF_CloseFont (UNPACK_TTFONT (font));
  return sizeof (TTF_Font *);
}


GH_DEFPROC (ttf_init, "ttf-init", 0, 0, 0,
            (void),
            "Initialize the SDL_ttf subsystem.")
{
#define FUNC_NAME s_ttf_init
  RETURN_INT (TTF_Init ());
#undef FUNC_NAME
}


GH_DEFPROC (ttf_load_font, "load-font", 2, 0, 0,
            (SCM file, SCM ptsize),
            "Load a font from @var{file} with point size @var{ptsize}.\n"
            "Return a handle.")
{
#define FUNC_NAME s_ttf_load_font
  ASSERT_STRING (file, ARGH1);
  ASSERT_EXACT (ptsize, ARGH2);

  RETURN_NEW_TTFONT
    (TTF_OpenFont (SCM_CHARS (file),
                   gh_scm2long (ptsize)));
#undef FUNC_NAME
}


GH_DEFPROC (ttf_get_font_style, "font:style", 1, 0, 0,
            (SCM font),
            "Return the style of @var{font} (see @code{flagstash:ttf}).\n"
            "This font style is implemented by modifying the font glyphs, and\n"
            "doesn't reflect any inherent properties of the truetype font file.")
{
#define FUNC_NAME s_ttf_get_font_style
  ASSERT_TTFONT (font, ARGH1);

  return gsdl_ulong2flags (TTF_GetFontStyle (UNPACK_TTFONT (font)),
                           ttf_flags);
#undef FUNC_NAME
}


GH_DEFPROC (ttf_set_font_style, "font:set-style!", 2, 0, 0,
            (SCM font, SCM style),
            "Set @var{font} style to @var{style} (see @code{flagstash:ttf}).\n"
            "This font style is implemented by modifying the font glyphs, and\n"
            "doesn't reflect any inherent properties of the truetype font file.")
{
#define FUNC_NAME s_ttf_set_font_style
  int cstyle;

  ASSERT_TTFONT (font, ARGH1);

  cstyle = GSDL_FLAGS2ULONG (style, ttf_flags, ARGH2);

  TTF_SetFontStyle (UNPACK_TTFONT (font), cstyle);
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


GH_DEFPROC (ttf_font_height, "font:height", 1, 0, 0,
            (SCM font),
            "Return the total height of @var{font},\n"
            "usually equal to point size.")
{
#define FUNC_NAME s_ttf_font_height
  ASSERT_TTFONT (font, ARGH1);

  RETURN_INT (TTF_FontHeight (UNPACK_TTFONT (font)));
#undef FUNC_NAME
}


GH_DEFPROC (ttf_font_ascent, "font:ascent", 1, 0, 0,
            (SCM font),
            "Return the offset from the baseline to the top of\n"
            "@var{font}.  This is a positive number.")
{
#define FUNC_NAME s_ttf_font_ascent
  ASSERT_TTFONT (font, ARGH1);

  RETURN_INT (TTF_FontAscent (UNPACK_TTFONT (font)));
#undef FUNC_NAME
}


GH_DEFPROC (ttf_font_descent, "font:descent", 1, 0, 0,
            (SCM font),
            "Return the offset from the baseline to the bottom of\n"
            "@var{font}.  This is a negative number.")
{
#define FUNC_NAME s_ttf_font_descent
  ASSERT_TTFONT (font, ARGH1);

  RETURN_INT (TTF_FontDescent (UNPACK_TTFONT (font)));
#undef FUNC_NAME
}


GH_DEFPROC (ttf_font_line_skip, "font:line-skip", 1, 0, 0,
            (SCM font),
            "Return the recommended spacing between lines of\n"
            "text for @var{font}.")
{
#define FUNC_NAME s_ttf_font_line_skip
  ASSERT_TTFONT (font, ARGH1);

  RETURN_INT (TTF_FontLineSkip (UNPACK_TTFONT (font)));
#undef FUNC_NAME
}


DECLARE_SIMPLE_SYM (minx);
DECLARE_SIMPLE_SYM (maxx);
DECLARE_SIMPLE_SYM (miny);
DECLARE_SIMPLE_SYM (maxy);
DECLARE_SIMPLE_SYM (advance);

GH_DEFPROC (ttf_glyph_metrics, "font:glyph-metrics", 2, 0, 0,
            (SCM font, SCM ch),
            "Return the metrics (dimensions) of a glyph as an alist.\n"
            "The glyph is a @var{font}-specific rendering of char @var{ch}.\n"
            "Alist keys are: @code{minx}, @code{maxx}, @code{miny},\n"
            "@code{maxy} and @code{advance}.  Values are numbers.")
{
#define FUNC_NAME s_ttf_glyph_metrics
  int minx, maxx, miny, maxy, advance;

  ASSERT_TTFONT (font, ARGH1);
  ASSERT_CHAR (ch, ARGH2);

  TTF_GlyphMetrics (UNPACK_TTFONT (font),
                    gh_scm2ulong (ch),
                    &minx, &maxx, &miny, &maxy, &advance);

  RETURN_LIST5 (gh_cons (SYM (minx), gh_long2scm (minx)),
                gh_cons (SYM (maxx), gh_long2scm (maxx)),
                gh_cons (SYM (miny), gh_long2scm (miny)),
                gh_cons (SYM (maxy), gh_long2scm (maxy)),
                gh_cons (SYM (advance), gh_long2scm (advance)));
#undef FUNC_NAME
}


DECLARE_SIMPLE_SYM (w);
DECLARE_SIMPLE_SYM (h);

GH_DEFPROC (ttf_size_text, "font:size-text", 2, 0, 0,
            (SCM font, SCM text),
            "Return an alist with keys @code{w} and @code{h} and\n"
            "corresponding values (numbers) representing the width\n"
            "and height of the @var{font}-specific rendering of\n"
            "the string @var{text}.")
{
#define FUNC_NAME s_ttf_size_text
  int w, h;

  ASSERT_TTFONT (font, ARGH1);
  ASSERT_STRING (text, ARGH2);

  TTF_SizeText (UNPACK_TTFONT (font), SCM_CHARS (text), &w, &h);
  RETURN_LIST2 (gh_cons (SYM (w), gh_long2scm (w)),
                gh_cons (SYM (h), gh_long2scm (h)));
#undef FUNC_NAME
}


GH_DEFPROC (ttf_size_utf8, "font:size-utf8", 2, 0, 0,
            (SCM font, SCM text),
            "Return an alist with keys @code{w} and @code{h} and\n"
            "corresponding values (numbers) representing the width\n"
            "and height of the @var{font}-specific rendering of\n"
            "the utf8 string @var{text}.")
{
#define FUNC_NAME s_ttf_size_utf8
  int w, h;

  ASSERT_TTFONT (font, ARGH1);
  ASSERT_STRING (text, ARGH2);

  TTF_SizeUTF8 (UNPACK_TTFONT (font), SCM_CHARS (text), &w, &h);
  RETURN_LIST2 (gh_cons (gsdl_sym_w, gh_long2scm (w)),
                gh_cons (gsdl_sym_h, gh_long2scm (h)));
#undef FUNC_NAME
}


GH_DEFPROC (ttf_render_text, "render-text", 3, 1, 0,
            (SCM font, SCM text, SCM fg, SCM bg),
            "Return a new surface containing the @var{font}-specific\n"
            "rendering of the @var{text} string.\n"
            "Third argument is the foreground color;\n"
            "optional fourth argument is the background color,\n"
            "or #t if the text is to be blended.")
{
#define FUNC_NAME s_ttf_render_text
  TTF_Font *cfont;
  SDL_Color *cfg;
  SDL_Surface *surface;
  char *ctext;

  ASSERT_TTFONT (font, ARGH1);
  ASSERT_STRING (text, ARGH2);
  ASSERT_COLOR (fg, ARGH3);

  cfont = UNPACK_TTFONT (font);
  ctext = SCM_CHARS (text);
  cfg = UNPACK_COLOR (fg);

  UNBOUND_MEANS_FALSE (bg);

  if (EXACTLY_FALSEP (bg))
    surface = TTF_RenderText_Solid (cfont, ctext, *cfg);
  else if (EXACTLY_TRUEP (bg))
    surface = TTF_RenderText_Blended (cfont, ctext, *cfg);
  else
    {
      ASSERT_COLOR (bg, ARGH4);
      surface = TTF_RenderText_Shaded (cfont, ctext, *cfg, *(UNPACK_COLOR (bg)));
    }

  RETURN_NEW_SURFACE (surface);
#undef FUNC_NAME
}


GH_DEFPROC (ttf_render_utf8, "render-utf8", 3, 1, 0,
            (SCM font, SCM text, SCM fg, SCM bg),
            "Return a new surface containing a @var{font}-specific\n"
            "rendering of the utf8 string @var{text}.\n"
            "Third argument is the foreground color;\n"
            "optional fourth argument is the background color,\n"
            "or #t if the text is to be blended.")
{
#define FUNC_NAME s_ttf_render_utf8
  TTF_Font *cfont;
  SDL_Color *cfg;
  SDL_Surface *surface;
  char *ctext;

  ASSERT_TTFONT (font, ARGH1);
  ASSERT_STRING (text, ARGH2);
  ASSERT_COLOR (fg, ARGH3);

  cfont = UNPACK_TTFONT (font);
  ctext = SCM_CHARS (text);
  cfg = UNPACK_COLOR (fg);

  UNBOUND_MEANS_FALSE (bg);

  if (EXACTLY_FALSEP (bg))
    surface = TTF_RenderUTF8_Solid (cfont, ctext, *cfg);
  else if (EXACTLY_TRUEP (bg))
    surface = TTF_RenderUTF8_Blended (cfont, ctext, *cfg);
  else
    {
      ASSERT_COLOR (bg, ARGH4);
      surface = TTF_RenderUTF8_Shaded (cfont, ctext, *cfg, *(UNPACK_COLOR (bg)));
    }

  RETURN_NEW_SURFACE (surface);
#undef FUNC_NAME
}


GH_DEFPROC (ttf_render_glyph, "render-glyph", 3, 1, 0,
            (SCM font, SCM ch, SCM fg, SCM bg),
            "Return a new surface containing a @var{font}-specific\n"
            "rendering of the character @var{ch}.\n"
            "Third argument is the foreground color;\n"
            "optional fourth argument is the background color,\n"
            "or #t if the text is to be blended.")
{
#define FUNC_NAME s_ttf_render_glyph
  TTF_Font *cfont;
  SDL_Color *cfg;
  SDL_Surface *surface;
  char cch;

  ASSERT_TTFONT (font, ARGH1);
  ASSERT_CHAR (ch, ARGH2);
  ASSERT_COLOR (fg, ARGH3);

  cfont = UNPACK_TTFONT (font);
  cch = gh_scm2char (ch);
  cfg = UNPACK_COLOR (fg);

  UNBOUND_MEANS_FALSE (bg);

  if (EXACTLY_FALSEP (bg))
    surface = TTF_RenderGlyph_Solid (cfont, cch, *cfg);
  else if (EXACTLY_TRUEP (bg))
    surface = TTF_RenderGlyph_Blended (cfont, cch, *cfg);
  else
    {
      ASSERT_COLOR (bg, ARGH4);
      surface = TTF_RenderGlyph_Shaded (cfont, cch, *cfg, *(UNPACK_COLOR (bg)));
    }

  RETURN_NEW_SURFACE (surface);
#undef FUNC_NAME
}


GH_DEFPROC (ttf_quit, "ttf-quit", 0, 0, 0,
            (void),
            "Quit the SDL_ttf subsystem.")
{
#define FUNC_NAME s_ttf_quit
  TTF_Quit ();
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}



/* Initialize the ttf subsystem.  */

extern flagstash_t gsdl_ttf_flagstash;

static
void
init_module (void)
{
  ttf_font_tag = scm_make_smob_type ("font", sizeof (struct TTF_Font*));
  scm_set_smob_free (ttf_font_tag, free_font);

  ttf_flags = gsdl_make_flagstash (&gsdl_ttf_flagstash);

#include "sdlttf.x"
}

GH_MODULE_LINK_FUNC ("sdl ttf", sdl_ttf, init_module)

/* sdlttf.c ends here */
