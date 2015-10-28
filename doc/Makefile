#  This file is part of the herschel package 
#
#  Copyright (c) 2010-2011 Gregor Klinke
#  All rights reserved.
#

top_srcdir=..

include $(top_srcdir)/config.mk

TEXINFO_FILES = herschel.texinfo

DISTFILES = Makefile       \
	herschel-mode.el          \
	minimum-requirements.txt \
	patterns.txt             \
	texinfo.tex              \
	CHANGES

include $(top_srcdir)/build/pre.mk

all-local: pdf info

PDFS =  $(TEXINFO_FILES:%.texinfo=$(BUILDDIR)/%.pdf)

pdf: $(PDFS)

$(BUILDDIR)/%.pdf : $(BUILDDIR)/pdf herschel.texinfo version.texinfo
	texi2pdf --build-dir=$(BUILDDIR)/pdf \
			     -I $(top_srcdir)/doc -I $(curdir)doc \
					 -o $@ herschel.texinfo

$(BUILDDIR)/pdf: $(BUILDDIR)
	@if [ ! -d "$@" ]; then mkdir $@; fi

INFOS =  $(TEXINFO_FILES:%.texinfo=$(BUILDDIR)/%.info)

info: $(INFOS)

$(BUILDDIR)/%.info : %.texinfo version.texinfo
	makeinfo -o $@ herschel.texinfo


clean-local:

distclean-local: clean-local
	-rm -rf $(BUILDDIR)/pdf $(BUILDDIR)/*.pdf $(BUILDDIR)/*.info
	-rm -rf version.texinfo

include $(top_srcdir)/build/generic.mk


