## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = gnu

include $(top_srcdir)/am/common

cmod_LTLIBRARIES = sdl.la

WRAPPER_SOURCES = sdl.c sdlvideo.c sdlsurface.c sdlcdrom.c       \
                  sdlrect.c sdlcolor.c sdljoystick.c sdlevent.c  \
                  sdlenums.c misc.c
FLAGSTASH       = initf.c kmodf.c videof.c palettef.c overlayf.c evmaskf.c
BUILT_SOURCES   = $(WRAPPER_SOURCES:.c=.x) $(FLAGSTASH)

sdl_la_SOURCES = $(FLAGSTASH) $(WRAPPER_SOURCES)
sdl_la_LDFLAGS = -version-info 0:0:0 -export-dynamic -module

install-data-hook: do-sofix
	rm -f $(cmoddir)/sdl-sup.*

## empty directive for $(top_srcdir)/am/fspec support
EXTRA_DIST =
include $(top_srcdir)/am/fspec
include $(top_srcdir)/am/snarfing

## Makefile.am ends here
