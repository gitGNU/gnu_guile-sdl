## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = gnu

SUBDIRS = src sdl sdl-mixer sdl-ttf sdl-gfx doc

# test suite
TESTS = test/rect.scm test/image.scm test/event.scm test/mixer.scm \
        test/ttf.scm  test/cursor.scm test/roto.scm test/gfx.scm
TESTS_ENVIRONMENT = GUILE_LOAD_PATH=$(srcdir) \
	LTDL_LIBRARY_PATH=$(srcdir)/src/.libs:$(srcdir)/sdl-mixer/.libs:$(srcdir)/sdl-ttf/.libs:$(srcdir)/sdl-gfx/.libs

# distribution
EXAMPLE_DIST =  examples/README \
		examples/sicp-painter.scm \
		examples/imagebrowser.scm \
		examples/bustagnu.scm \
		examples/circle.png \
		examples/gnu-head.png \
		examples/launcher.png \
		examples/william-rogers.png
INCLUDE_DIST =  include/sdl.h       include/sdlcolor.h \
		include/sdlevent.h  include/sdlrect.h \
		include/sdlsmobs.h  include/sdlvideo.h \
		include/sdlcdrom.h  include/sdlenums.h \
		include/sdljoystick.h  include/sdlroto.h \
		include/sdlsurface.h  

EXTRA_DIST = $(EXAMPLE_DIST) test/README $(TESTS) $(INCLUDE_DIST)

