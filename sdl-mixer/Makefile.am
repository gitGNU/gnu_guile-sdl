## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = gnu

include $(top_srcdir)/am/common

if HAVE_MIXER

WRAPPER_SOURCES  = sdlmixer.c
BUILT_SOURCES    = $(WRAPPER_SOURCES:.c=.x)

cmod_LTLIBRARIES = mixer.la
mixer_la_SOURCES = $(WRAPPER_SOURCES)
mixer_la_LDFLAGS = -version-info 0:0:0 -export-dynamic -module -lSDL_mixer

include $(top_srcdir)/am/snarfing

endif # HAVE_MIXER

install-data-hook:
if HAVE_MIXER
	cd $(cmoddir) ; for f in $(cmod_LTLIBRARIES) ; do \
	   test -f lib$$f && rm -f lib$$f ; $(LN_S) $$f lib$$f ; done
else
	@echo '(nothing to do for "$@")'
endif # not HAVE_MIXER


## Makefile.am ends here
