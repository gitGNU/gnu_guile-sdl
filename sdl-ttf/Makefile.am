## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = gnu

include $(top_srcdir)/am/common

if HAVE_TTF

WRAPPER_SOURCES = sdlttf.c
FLAGSTASH       = ttff.c
BUILT_SOURCES   = $(WRAPPER_SOURCES:.c=.x) $(FLAGSTASH)

cmod_LTLIBRARIES = ttf.la
ttf_la_SOURCES = $(FLAGSTASH) $(WRAPPER_SOURCES)
ttf_la_LDFLAGS = -version-info 0:0:0 -export-dynamic -module -lSDL_ttf

## empty directive for $(top_srcdir)/am/fspec support
EXTRA_DIST =
include $(top_srcdir)/am/fspec
include $(top_srcdir)/am/snarfing

endif # HAVE_TTF

install-data-hook:
if HAVE_TTF
	cd $(cmoddir) ; for f in $(cmod_LTLIBRARIES) ; do \
	   test -f lib$$f && rm -f lib$$f ; $(LN_S) $$f lib$$f ; done
else
	@echo '(nothing to do for "$@")'
endif # not HAVE_TTF

## Makefile.am ends here
