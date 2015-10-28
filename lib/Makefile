#  This file is part of the herschel package
#
#  Copyright (c) 2010-2011 Gregor Klinke
#  All rights reserved.
#

top_srcdir = ..

include $(top_srcdir)/config.mk

SUBDIRS = lang

DISTFILES = \
	Makefile

include $(top_srcdir)/build/pre.mk

nop:

all-local: nop

dist-local:
	for d in clang coll core io; do \
		mkdir $(top_distdir)/$(subdir)/$$d ; \
		for f in $$d/*.hr; do \
			cp -p $$f $(top_distdir)/$(subdir)/$$f >/dev/null 2>&1; \
		done; \
	done

include $(top_srcdir)/build/generic.mk
