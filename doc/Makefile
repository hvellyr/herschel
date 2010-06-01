#  This file is part of the heather package 
#
#  Copyright (c) 2010 Gregor Klinke
#  All rights reserved.
#

top_srcdir=..

include $(top_srcdir)/config.mk

TEXINFO_FILES = heather.texinfo

DISTFILES = Makefile       \
	heather-mode.el          \
	heather-logo.eps         \
	copyright.head           \
	copyright.cpp            \
	minimum-requirements.txt \
	patterns.txt             \
	texinfo.tex              \
	artwork/heather-logo.eps \
	artwork/heather-logo.png \
	artwork/heather.jpg      \
	fonts/Makefile           \
	fonts/OcrB.afm           \
	fonts/OcrB.map           \
	fonts/fobr.tfm           \
	fonts/fobr.vf            \
	fonts/fobr.vpl           \
	fonts/fobro.tfm          \
	fonts/fobro.vf           \
	fonts/fobro.vpl          \
	fonts/ocrb.sfd           \
	fonts/ocrb.ttf           \
	fonts/rfobr.tfm          \
	fonts/rfobro.tfm         \
	CHANGES

include $(top_srcdir)/build/pre.mk

all-local: pdf info

PDFS =  $(TEXINFO_FILES:%.texinfo=$(BUILDDIR)/%.pdf)

pdf: $(PDFS)

$(BUILDDIR)/%.pdf : %.texinfo
	texi2pdf -o $@ $<


INFOS =  $(TEXINFO_FILES:%.texinfo=$(BUILDDIR)/%.info)

info: $(INFOS)

$(BUILDDIR)/%.info : %.texinfo
	makeinfo -o $@ $<


clean-local:

distclean-local: clean-local

include $(top_srcdir)/build/generic.mk


