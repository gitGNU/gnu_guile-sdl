## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = gnu

BUILT_SOURCES = ref.texi ef-ref.texi TODO-excerpt

info_TEXINFOS = guile-sdl.texi

SUFFIXES = .texi .twerp

TODO-excerpt: $(top_srcdir)/TODO
	sed '1,/^_/!d;s/__*//' $< > $@

guile-sdl.info: $(BUILT_SOURCES)

.twerp.texi:
	guile-tools twerp2texi \
	  -I ../src/.doc-index \
	  -I ../sdl-ttf/.doc-index \
	  -I ../sdl-gfx/.doc-index \
	  -I ../sdl-mixer/.doc-index \
	  -o $@ $<

EXTRA_DIST = ref.twerp ef-ref.twerp cov

TESTS = cov

## Makefile.am ends here
