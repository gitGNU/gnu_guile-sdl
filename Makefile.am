## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = gnu

SUBDIRS = src sdl sdl-mixer sdl-ttf sdl-gfx doc

noinst_DATA = pre-inst.merged-module-catalog

pre-inst.merged-module-catalog:
	guile-tools make-module-catalog				\
	   -d o,lo						\
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

install-data-hook:
	if guile-tools | grep -q make-module-catalog ; then	\
	  cd $(DESTDIR)$(GUILE_LIBSITE) ;			\
	  guile-tools make-module-catalog `pwd` ;		\
	  cd $(DESTDIR)$(GUILE_SITE) ;				\
	  guile-tools make-module-catalog `pwd` ;		\
	fi

# test suite
TESTS = alive.test \
	test/rect.scm test/image.scm test/event.scm test/mixer.scm \
        test/ttf.scm  test/cursor.scm test/roto.scm test/gfx.scm

TESTS_ENVIRONMENT = ./wily-guile -l $(srcdir)/test/common -s

# distribution
as_is_dirs = examples test include

EXTRA_DIST = aclocal-suffix alive.test $(as_is_dirs)

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

## maintenance

if MAINTAINER_MODE

cvs-state-summary:
	cd $(top_srcdir) ;			\
	ttn-do display-cvs-state-summary	\
	  `find . -name CVS -printf ' %h'`

endif # MAINTAINER_MODE

## Makefile.am ends here
