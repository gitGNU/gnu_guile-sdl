## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = gnu

BUILT_SOURCES = ref.texi ef-ref.texi excuse.texi TODO-excerpt

info_TEXINFOS = guile-sdl.texi
guile_sdl_TEXINFOS = $(BUILT_SOURCES)

SUFFIXES = .texi .twerp

TODO-excerpt: $(top_srcdir)/TODO
	sed '1,/^_/!d;s/__*//' $< > $@

# This causes (one hopes) Automake 1.8 and later to generate rules to
# place the info file in the build tree instead of the source tree.
DISTCLEANFILES = guile-sdl.info

ref.texi: $(top_srcdir)/*/*.c $(top_srcdir)/sdl/*.scm
ef-ref.texi: $(top_srcdir)/*/*.c

.twerp.texi:
	guile-tools twerp2texi \
	  -I ../sdl/.doc-index \
	  -I ../src/.doc-index \
	  -o $@ $(TWERP2TEXIFLAGS) $<

excuse.texi: $(top_srcdir)/cov/notes.tdt
	$(srcdir)/mkexcuse $< > $@

EXTRA_DIST = ref.twerp ef-ref.twerp cov mkexcuse $(BUILT_SOURCES)

if MAINTAINER_MODE
TESTS_ENVIRONMENT = srcdir=$(srcdir)
TESTS = cov
endif ## MAINTAINER_MODE

## Makefile.am ends here
