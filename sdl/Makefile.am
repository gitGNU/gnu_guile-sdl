## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = gnu

include $(top_srcdir)/am/common

schememoddir = $(GUILE_SITE)/sdl
schememod_DATA = misc-utils.scm

EXTRA_DIST = $(schememod_DATA)

# begin doc snarfing for scheme code
#  (if this block is ever needed outside of cwd, move it to ../am/scm-snarfing)
scm2doc		= guile-tools doc-snarf -D
mkdocindex	= guile-tools make-twerp2texi-index
dotdocfiles	= $(schememod_DATA:.scm=.doc)

.scm.doc:
	$(scm2doc) -o $@ $<

.doc-index: $(dotdocfiles)
	$(mkdocindex) -o $@ $(dotdocfiles)

SUFFIXES	+= .x .doc
DISTCLEANFILES	+= *.x
if MAINTAINER_MODE
noinst_DATA	+= .doc-index
endif ## MAINTAINER_MODE
# end doc snarfing for scheme code
