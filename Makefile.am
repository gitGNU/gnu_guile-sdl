## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = gnu

SUBDIRS = src sdl sdl-mixer sdl-ttf sdl-gfx doc

# test suite
TESTS = alive.test \
	test/rect.scm test/image.scm test/event.scm test/mixer.scm \
        test/ttf.scm  test/cursor.scm test/roto.scm test/gfx.scm

TESTS_ENVIRONMENT = ./wily-guile -l $(srcdir)/test/common -s

# distribution
EXAMPLE_DIST =  examples/README \
		examples/sicp-painter.scm \
		examples/imagebrowser.scm \
		examples/bustagnu.scm \
		examples/circle.png \
		examples/gnu-head.png \
		examples/launcher.png \
		examples/william-rogers.png
TEST_DIST    =  test/README            test/crystal.ttf \
		test/gnu-goatee.jpg    test/noise.wav \
		test/test.wav
INCLUDE_DIST =  include/sdl.h          include/sdlcolor.h \
		include/sdlevent.h     include/sdlrect.h \
		include/sdlsmobs.h     include/sdlvideo.h \
		include/sdlcdrom.h     include/sdlenums.h \
		include/sdljoystick.h  include/sdlroto.h \
		include/sdlsurface.h

EXTRA_DIST = $(EXAMPLE_DIST) $(TEST_DIST) $(TESTS) $(INCLUDE_DIST)


noinst_DATA = pre-inst.merged-module-catalog

pre-inst.merged-module-catalog:
	guile-tools make-module-catalog				\
	   -X "[.]o$$"						\
	   -X "[.]lo$$"						\
	   -X "~$$"						\
	   -X "doc/"						\
	   -X "CVS/"						\
	   -X "/test/"						\
	   `pwd` `test "$(srcdir)" = "." || { cd $(srcdir) ; pwd ; }`
	if test "$(srcdir)" = "." ; then			\
	  mv .module-catalog $@ ;				\
	else							\
	  guile-tools merge-module-catalogs -o $@		\
	              .module-catalog				\
	    $(srcdir)/.module-catalog ;				\
	fi
	rm -f .module-catalog $(srcdir)/.module-catalog
	test -d .deps || mkdir .deps
	( echo $@ : \\ ; \
	  sed '\,((.*).*"/.*"),!d;s/"[^"]*$$/ \\/;s/^.*"/ /' $@ ; \
	  echo ' # elephant' ) > .deps/pre-inst.Pmerged-module-catalog

# Makefile.am ends here
