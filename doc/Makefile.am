## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = gnu

BUILT_SOURCES = ref.texi

info_TEXINFOS = guile-sdl.texi

SUFFIXES = .texi .twerp

guile-sdl.info: ref.texi

.twerp.texi:
	guile-tools twerp2texi \
	  -I ../src/.doc-index \
	  -I ../sdl-ttf/.doc-index \
	  -I ../sdl-gfx/.doc-index \
	  -I ../sdl-mixer/.doc-index \
	  -o $@ $<

## Makefile.am ends here
