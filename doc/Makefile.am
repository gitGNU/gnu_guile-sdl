## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = gnu

info_TEXINFOS = guile-sdl.texi

# doc snarfing
GUILE_SNARF_DOCS = guile-snarf-docs
GUILE_SNARF_DOCS_TEXI = guile-snarf-docs-texi

SUFFIXES = .texi .doc

%.doc: %.c
	$(GUILE_SNARF_DOCS) $(DEFS) $(INCLUDES) -I../include \
	-I../src/SDL_rotozoom/ -I../sdl-gfx/SDL_gfxPrimitives/ \
	$(CPPFLAGS) $(CFLAGS) $< > $@ \
	|| { rm $@; false; }

.doc.texi:
	$(GUILE_SNARF_DOCS_TEXI) $< > $@ \
	|| { rm $@; false; }

