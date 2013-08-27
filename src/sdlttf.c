/* sdlttf.c --- SDL_ttf for Guile
 *
 * Copyright (C) 2003, 2004, 2005, 2007, 2009,
 *               2011, 2012, 2013 Thien-Thi Nguyen
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
#include "SDL_ttf.h"
#include "snuggle/finangle.h"
#include "b-values.h"

IMPORT_MODULE (sdlsup, "(sdl sdl)");
SELECT_MODULE_VAR (obtw, sdlsup, "%%Guile-SDL-obtw");


static SCM ttf_flags;

/* Smob tags.  */
static smob_tag_t ttf_font_tag;
#define ttf_font_nick "SDL-TTF"

#define ASSERT_TTFONT(obj,which) \
  ASSERT_SMOB (obj, ttf_font, which)

#define UNPACK_TTFONT(smob) \
  SMOBGET (smob, TTF_Font *)

#define RETURN_NEW_TTFONT(x) \
  NEWSMOB_OR_FALSE (ttf_font_tag, x)

static
size_t
free_font (SCM font)
{
  TTF_CloseFont (UNPACK_TTFONT (font));
  return 0;
}


PRIMPROC
(ttf_init, "ttf-init", 0, 0, 0,
 (void),
 doc: /***********
Initialize the SDL_ttf subsystem.  */)
{
#define FUNC_NAME s_ttf_init
  RETURN_INT (TTF_Init ());
#undef FUNC_NAME
}


PRIMPROC
(ttf_load_font, "load-font", 2, 0, 0,
 (SCM file, SCM ptsize),
 doc: /***********
Load a font from @var{file} with point size @var{ptsize}.
Return a handle.  */)
{
#define FUNC_NAME s_ttf_load_font
  range_t cfile;
  TTF_Font *rv;

  ASSERT_STRING (file, 1);
  ASSERT_INTEGER (ptsize, 2);

  FINANGLE (file);
  rv = TTF_OpenFont (RS (file), C_LONG (ptsize));
  UNFINANGLE (file);
  RETURN_NEW_TTFONT (rv);
#undef FUNC_NAME
}


PRIMPROC
(ttf_get_font_style, "font:style", 1, 0, 0,
 (SCM font),
 doc: /***********
Return the style of @var{font} (@pxref{font-style flags}).
This font style is implemented by modifying the font glyphs, and
doesn't reflect any inherent properties of the truetype font file.  */)
{
#define FUNC_NAME s_ttf_get_font_style
  ASSERT_TTFONT (font, 1);

  return btw->ulong2flags (TTF_GetFontStyle (UNPACK_TTFONT (font)),
                           ttf_flags);
#undef FUNC_NAME
}


PRIMPROC
(ttf_set_font_style, "font:set-style!", 2, 0, 0,
 (SCM font, SCM style),
 doc: /***********
Set @var{font} style to @var{style} (@pxref{font-style flags}).
This font style is implemented by modifying the font glyphs, and
doesn't reflect any inherent properties of the truetype font file.  */)
{
#define FUNC_NAME s_ttf_set_font_style
  DECLINIT_SYM2NUM_CC (2, ttf_flags);
  int cstyle;

  ASSERT_TTFONT (font, 1);

  cstyle = FLAGS2ULONG (2, style);

  TTF_SetFontStyle (UNPACK_TTFONT (font), cstyle);
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


PRIMPROC
(ttf_font_height, "font:height", 1, 0, 0,
 (SCM font),
 doc: /***********
Return the total height of @var{font},
usually equal to point size.  */)
{
#define FUNC_NAME s_ttf_font_height
  ASSERT_TTFONT (font, 1);

  RETURN_INT (TTF_FontHeight (UNPACK_TTFONT (font)));
#undef FUNC_NAME
}


PRIMPROC
(ttf_font_ascent, "font:ascent", 1, 0, 0,
 (SCM font),
 doc: /***********
Return the offset from the baseline to the top of
@var{font}.  This is a positive number.  */)
{
#define FUNC_NAME s_ttf_font_ascent
  ASSERT_TTFONT (font, 1);

  RETURN_INT (TTF_FontAscent (UNPACK_TTFONT (font)));
#undef FUNC_NAME
}


PRIMPROC
(ttf_font_descent, "font:descent", 1, 0, 0,
 (SCM font),
 doc: /***********
Return the offset from the baseline to the bottom of
@var{font}.  This is a negative number.  */)
{
#define FUNC_NAME s_ttf_font_descent
  ASSERT_TTFONT (font, 1);

  RETURN_INT (TTF_FontDescent (UNPACK_TTFONT (font)));
#undef FUNC_NAME
}


PRIMPROC
(ttf_font_line_skip, "font:line-skip", 1, 0, 0,
 (SCM font),
 doc: /***********
Return the recommended spacing between lines of
text for @var{font}.  */)
{
#define FUNC_NAME s_ttf_font_line_skip
  ASSERT_TTFONT (font, 1);

  RETURN_INT (TTF_FontLineSkip (UNPACK_TTFONT (font)));
#undef FUNC_NAME
}


PRIMPROC
(font_glyph_xXyYa, "font:glyph-xXyYa", 2, 0, 0,
 (SCM font, SCM ch),
 doc: /***********
Return the metrics (dimensions) of a glyph as five values.
The glyph is a @var{font}-specific rendering of char @var{ch}.
Values are: @code{minx}, @code{maxx}, @code{miny},
@code{maxy} and @code{advance} (all integers).  */)
{
#define FUNC_NAME s_font_glyph_xXyYa
  int minx, maxx, miny, maxy, advance;

  ASSERT_TTFONT (font, 1);
  ASSERT_CHAR (ch, 2);

  TTF_GlyphMetrics (UNPACK_TTFONT (font), C_CHAR (ch),
                    &minx, &maxx, &miny, &maxy, &advance);

  RETURN_VALUES5
    (NUM_LONG (minx),
     NUM_LONG (maxx),
     NUM_LONG (miny),
     NUM_LONG (maxy),
     NUM_LONG (advance));
#undef FUNC_NAME
}


DECLARE_SIMPLE_SYM (minx);
DECLARE_SIMPLE_SYM (maxx);
DECLARE_SIMPLE_SYM (miny);
DECLARE_SIMPLE_SYM (maxy);
DECLARE_SIMPLE_SYM (advance);

PRIMPROC
(ttf_glyph_metrics, "font:glyph-metrics", 2, 0, 0,
 (SCM font, SCM ch),
 doc: /***********
NB: This procedure is obsoleted by @code{font:glyph-xXyYa}
and @strong{will be removed} after 2013-12-31.

Return the metrics (dimensions) of a glyph as an alist.
The glyph is a @var{font}-specific rendering of char @var{ch}.
Alist keys are: @code{minx}, @code{maxx}, @code{miny},
@code{maxy} and @code{advance}.  Values are numbers.  */)
{
#define FUNC_NAME s_ttf_glyph_metrics
  int minx, maxx, miny, maxy, advance;

  ASSERT_TTFONT (font, 1);
  ASSERT_CHAR (ch, 2);

  TTF_GlyphMetrics (UNPACK_TTFONT (font), C_CHAR (ch),
                    &minx, &maxx, &miny, &maxy, &advance);

  return LIST5 (CONS (SYM (minx), NUM_LONG (minx)),
                CONS (SYM (maxx), NUM_LONG (maxx)),
                CONS (SYM (miny), NUM_LONG (miny)),
                CONS (SYM (maxy), NUM_LONG (maxy)),
                CONS (SYM (advance), NUM_LONG (advance)));
#undef FUNC_NAME
}


PRIMPROC
(text_wh, "text-wh", 2, 0, 0,
 (SCM font, SCM text),
 doc: /***********
Return two values: @code{width} and @code{height} (both integers)
representing the dimensions of the @var{font}-specific rendering
of the string @var{text}.  */)
{
#define FUNC_NAME s_text_wh
  range_t ctext;
  int w, h;

  ASSERT_TTFONT (font, 1);
  ASSERT_STRING (text, 2);

  FINANGLE (text);
  TTF_SizeText (UNPACK_TTFONT (font), RS (text), &w, &h);
  UNFINANGLE (text);
  RETURN_VALUES2
    (NUM_LONG (w),
     NUM_LONG (h));
#undef FUNC_NAME
}


DECLARE_SIMPLE_SYM (w);
DECLARE_SIMPLE_SYM (h);

PRIMPROC
(ttf_size_text, "font:size-text", 2, 0, 0,
 (SCM font, SCM text),
 doc: /***********
NB: This procedure is obsoleted by @code{text-wh}
and @strong{will be removed} after 2013-12-31.

Return an alist with keys @code{w} and @code{h} and
corresponding values (numbers) representing the width
and height of the @var{font}-specific rendering of
the string @var{text}.  */)
{
#define FUNC_NAME s_ttf_size_text
  range_t ctext;
  int w, h;

  ASSERT_TTFONT (font, 1);
  ASSERT_STRING (text, 2);

  FINANGLE (text);
  TTF_SizeText (UNPACK_TTFONT (font), RS (text), &w, &h);
  UNFINANGLE (text);
  return LIST2 (CONS (SYM (w), NUM_LONG (w)),
                CONS (SYM (h), NUM_LONG (h)));
#undef FUNC_NAME
}


PRIMPROC
(utf8_wh, "utf8-wh", 2, 0, 0,
 (SCM font, SCM text),
 doc: /***********
Return two values: @code{width} and @code{height} (both integers)
representing the dimensions of the @var{font}-specific rendering
of the UTF-8 string @var{text}.  */)
{
#define FUNC_NAME s_utf8_wh
  range_t ctext;
  int w, h;

  ASSERT_TTFONT (font, 1);
  ASSERT_STRING (text, 2);

  FINANGLE (text);
  TTF_SizeUTF8 (UNPACK_TTFONT (font), RS (text), &w, &h);
  UNFINANGLE (text);
  RETURN_VALUES2
    (NUM_LONG (w),
     NUM_LONG (h));
#undef FUNC_NAME
}


PRIMPROC
(ttf_size_utf8, "font:size-utf8", 2, 0, 0,
 (SCM font, SCM text),
 doc: /***********
NB: This procedure is obsoleted by @code{utf8-wh}
and @strong{will be removed} after 2013-12-31.

Return an alist with keys @code{w} and @code{h} and
corresponding values (numbers) representing the width
and height of the @var{font}-specific rendering of
the utf8 string @var{text}.  */)
{
#define FUNC_NAME s_ttf_size_utf8
  range_t ctext;
  int w, h;

  ASSERT_TTFONT (font, 1);
  ASSERT_STRING (text, 2);

  FINANGLE (text);
  TTF_SizeUTF8 (UNPACK_TTFONT (font), RS (text), &w, &h);
  UNFINANGLE (text);
  return LIST2 (CONS (gsdl_sym_w, NUM_LONG (w)),
                CONS (gsdl_sym_h, NUM_LONG (h)));
#undef FUNC_NAME
}


PRIMPROC
(ttf_render_text, "render-text", 3, 1, 0,
 (SCM font, SCM text, SCM fg, SCM bg),
 doc: /***********
Return a new surface containing the @var{font}-specific
rendering of the @var{text} string.
Third argument is the foreground color;
optional fourth argument is the background color,
or @code{#t} if the text is to be blended.  */)
{
#define FUNC_NAME s_ttf_render_text
  range_t ctext;
  TTF_Font *cfont;
  SDL_Color *cfg;
  SDL_Surface *surface;

  ASSERT_TTFONT (font, 1);
  ASSERT_STRING (text, 2);
  ASSERT_COLOR (fg, 3);

  cfont = UNPACK_TTFONT (font);
  cfg = UNPACK_COLOR (fg);

  UNBOUND_MEANS_FALSE (bg);

  FINANGLE (text);
  if (EXACTLY_FALSEP (bg))
    surface = TTF_RenderText_Solid (cfont, RS (text), *cfg);
  else if (EXACTLY_TRUEP (bg))
    surface = TTF_RenderText_Blended (cfont, RS (text), *cfg);
  else
    {
      ASSERT_COLOR (bg, 4);
      surface = TTF_RenderText_Shaded (cfont, RS (text), *cfg,
                                       *(UNPACK_COLOR (bg)));
    }
  UNFINANGLE (text);

  RETURN_NEW_SURFACE (surface);
#undef FUNC_NAME
}


PRIMPROC
(ttf_render_utf8, "render-utf8", 3, 1, 0,
 (SCM font, SCM text, SCM fg, SCM bg),
 doc: /***********
Return a new surface containing a @var{font}-specific
rendering of the utf8 string @var{text}.
Third argument is the foreground color;
optional fourth argument is the background color,
or @code{#t} if the text is to be blended.  */)
{
#define FUNC_NAME s_ttf_render_utf8
  range_t ctext;
  TTF_Font *cfont;
  SDL_Color *cfg;
  SDL_Surface *surface;

  ASSERT_TTFONT (font, 1);
  ASSERT_STRING (text, 2);
  ASSERT_COLOR (fg, 3);

  cfont = UNPACK_TTFONT (font);
  cfg = UNPACK_COLOR (fg);

  UNBOUND_MEANS_FALSE (bg);

  FINANGLE (text);
  if (EXACTLY_FALSEP (bg))
    surface = TTF_RenderUTF8_Solid (cfont, RS (text), *cfg);
  else if (EXACTLY_TRUEP (bg))
    surface = TTF_RenderUTF8_Blended (cfont, RS (text), *cfg);
  else
    {
      ASSERT_COLOR (bg, 4);
      surface = TTF_RenderUTF8_Shaded (cfont, RS (text), *cfg,
                                       *(UNPACK_COLOR (bg)));
    }
  UNFINANGLE (text);

  RETURN_NEW_SURFACE (surface);
#undef FUNC_NAME
}


PRIMPROC
(ttf_render_glyph, "render-glyph", 3, 1, 0,
 (SCM font, SCM ch, SCM fg, SCM bg),
 doc: /***********
Return a new surface containing a @var{font}-specific
rendering of the character @var{ch}.
Third argument is the foreground color;
optional fourth argument is the background color,
or @code{#t} if the text is to be blended.  */)
{
#define FUNC_NAME s_ttf_render_glyph
  TTF_Font *cfont;
  SDL_Color *cfg;
  SDL_Surface *surface;
  char cch;

  ASSERT_TTFONT (font, 1);
  ASSERT_CHAR (ch, 2);
  ASSERT_COLOR (fg, 3);

  cfont = UNPACK_TTFONT (font);
  cch = C_CHAR (ch);
  cfg = UNPACK_COLOR (fg);

  UNBOUND_MEANS_FALSE (bg);

  if (EXACTLY_FALSEP (bg))
    surface = TTF_RenderGlyph_Solid (cfont, cch, *cfg);
  else if (EXACTLY_TRUEP (bg))
    surface = TTF_RenderGlyph_Blended (cfont, cch, *cfg);
  else
    {
      ASSERT_COLOR (bg, 4);
      surface = TTF_RenderGlyph_Shaded (cfont, cch, *cfg, *(UNPACK_COLOR (bg)));
    }

  RETURN_NEW_SURFACE (surface);
#undef FUNC_NAME
}


PRIMPROC
(ttf_quit, "ttf-quit", 0, 0, 0,
 (void),
 doc: /***********
Quit the SDL_ttf subsystem.  */)
{
#define FUNC_NAME s_ttf_quit
  TTF_Quit ();
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}



/* Initialize the ttf subsystem.  */

#include "k/ttf.c"

static
void
init_module (void)
{
  DEFSMOB (ttf_font_tag, ttf_font_nick,
           NULL,
           free_font,
           /* TODO: print_font */ NULL);

#include "sdlttf.x"

  btw = UNPACK_POINTER (CALL0 (obtw));

  {
    kf_init_t allf[] = {
      { &ttf_flags, &ttf_flagstash }
    };

    REGISTER_KF_V (allf);
  }
}

MOD_INIT_LINK_THUNK ("sdl ttf", sdl_ttf, init_module)

/* sdlttf.c ends here */
