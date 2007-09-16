## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = gnu

ACLOCAL_AMFLAGS = -I am

SUBDIRS = src sdl sdl-mixer sdl-ttf sdl-gfx doc

noinst_DATA = .module-catalog

.module-catalog:
	if guile-tools | grep -q mkpimmc ; then			\
	  guile-tools mkpimmc -d $(DEPDIR) $@ "$(srcdir)"	\
	    -- -d in,lo,o					\
	       -X '~$$' -X 'doc/' -X 'CVS/' -X '/test/' ;	\
	else echo '()' > $@ ; fi

CLEANFILES = .module-catalog
clean-local:
	./config.status module-catalog-clean-local

install-data-hook:
	if guile-tools | grep -q make-module-catalog ; then	\
	   guile-tools make-module-catalog -t "$(DESTDIR)"	\
	     $(DESTDIR)$(GUILE_LIBSITE) ;			\
	fi

# test suite
TESTS = alive.test				\
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
as_is_dirs = examples test include cov

EXTRA_DIST = \
	am/aclocal-suffix \
	am/guile-sdl.m4 \
	am/sofix \
	alive.test $(as_is_dirs)

dist-hook:
	for d in $(as_is_dirs) ; do \
	  rm -rf `find $(distdir)/$$d -name CVS` ; done

## these are to support upgrading from 0:0:0 ltlib: see README

check-000-libdir:
	@echo 'files in $(libdir) matching "libguileSDL*":'
	@ls $(libdir)/libguileSDL* | sed 's/^/ /g'

clean-000-libdir:
	@echo 'deleting $(libdir)/libguileSDL* files'
	rm -f $(libdir)/libguileSDL*

## Makefile.am ends here
