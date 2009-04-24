## Process this file with Automake to create Makefile.in

ACLOCAL_AMFLAGS = -I am

SUBDIRS = src doc

noinst_DATA = .module-catalog

.module-catalog:
	if guile-tools | grep -q mkpimmc ; then			\
	  guile-tools mkpimmc -d $(DEPDIR) $@ "$(srcdir)"	\
	    -- -d in,lo,o,git					\
	       -X '~$$' -X 'doc/' -X '/test/' ;			\
	else echo '()' > $@ ; fi

CLEANFILES = .module-catalog
clean-local:
	./config.status module-catalog-clean-local

install-data-hook: update-libsite-module-catalog
uninstall-hook:    update-libsite-module-catalog

update-libsite-module-catalog:
	if guile-tools | grep -q make-module-catalog ; then	\
	   guile-tools make-module-catalog -t "$(DESTDIR)"	\
	     "$(DESTDIR)$(GUILE_LIBSITE)" ;			\
	fi

# test suite
TESTS = test/alive.test				\
	test/leak.scm				\
	test/rect.scm				\
	test/image.scm				\
	test/event.scm				\
	test/mixer.scm				\
	test/ttf.scm				\
	test/cursor.scm				\
	test/roto.scm				\
	test/gfx.scm				\
	test/fading.scm

TESTS_ENVIRONMENT = \
	HAVE_TTF=$(HAVE_TTF) \
	HAVE_MIXER=$(HAVE_MIXER) \
	$(GUILE) -L "@abs_top_builddir@" -l $(srcdir)/test/common -s

# distribution
as_is_dirs = test

EXTRA_DIST = HISTORY \
	am/guile-sdl.m4 \
	am/sofix \
	$(as_is_dirs)

## these are to support upgrading from 0:0:0 ltlib: see README

check-000-libdir:
	@echo 'files in $(libdir) matching "libguileSDL*":'
	@ls $(libdir)/libguileSDL* | sed 's/^/ /g'

clean-000-libdir:
	@echo 'deleting $(libdir)/libguileSDL* files'
	rm -f $(libdir)/libguileSDL*

## Makefile.am ends here
