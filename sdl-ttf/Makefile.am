## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = gnu

include $(top_srcdir)/am/common

EXTRA_DIST = sdlttf.h

WRAPPER_SOURCES = sdlttf.c
FLAGSTASH       = ttff.c
BUILT_SOURCES   = $(WRAPPER_SOURCES:.c=.x) $(FLAGSTASH)

if HAVE_TTF
cmod_LTLIBRARIES = ttf.la
ttf_la_SOURCES = $(BUILT_SOURCES) $(WRAPPER_SOURCES)
ttf_la_LDFLAGS = -version-info 0:0:0 -export-dynamic -module -lSDL_ttf
else
cmod_LTLIBRARIES =
ttf_la_SOURCES =
ttf_la_LDFLAGS =
endif

include $(top_srcdir)/am/fspec
include $(top_srcdir)/am/snarfing

## Makefile.am ends here
