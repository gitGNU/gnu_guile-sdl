## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = gnu

include $(top_srcdir)/am/common

WRAPPER_SOURCES = sdlmixer.c
FLAGSTASH	=
BUILT_SOURCES   = $(WRAPPER_SOURCES:.c=.x) $(FLAGSTASH)

if HAVE_MIXER
cmod_LTLIBRARIES = mixer.la
mixer_la_SOURCES = $(BUILT_SOURCES) $(WRAPPER_SOURCES)
mixer_la_LDFLAGS = -version-info 0:0:0 -export-dynamic -module -lSDL_mixer
else
cmod_LTLIBRARIES =
mixer_la_SOURCES =
mixer_la_LDFLAGS =
endif

include $(top_srcdir)/am/snarfing

## Makefile.am ends here
