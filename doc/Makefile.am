## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = gnu

BUILT_SOURCES = ref.texi ef-ref.texi TODO-excerpt

info_TEXINFOS = guile-sdl.texi

SUFFIXES = .texi .twerp

TODO-excerpt: $(top_srcdir)/TODO
	sed '1,/^_/!d;s/__*//' $< > $@

guile-sdl.info: $(BUILT_SOURCES)

if HAVE_TTF
minusIttfdocindex = -I ../sdl-ttf/.doc-index
endif

if HAVE_MIXER
minusImixerdocindex = -I ../sdl-mixer/.doc-index
endif

.twerp.texi:
	guile-tools twerp2texi \
	  -I ../src/.doc-index \
	  $(minusIttfdocindex) \
	  -I ../sdl-gfx/.doc-index \
	  $(minusImixerdocindex) \
	  -o $@ $<

EXTRA_DIST = ref.twerp ef-ref.twerp cov $(BUILT_SOURCES)

if MAINTAINER_MODE
TESTS_ENVIRONMENT = srcdir=$(srcdir)
TESTS = cov
endif ## MAINTAINER_MODE

## Makefile.am ends here
