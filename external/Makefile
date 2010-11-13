#  This file is part of the heather package
#
#  Copyright (c) 2010 Gregor Klinke
#  All rights reserved.
#

top_srcdir = ..

include $(top_srcdir)/config.mk

SUBDIRS = UnitTest++

DISTFILES = \
	Makefile

include $(top_srcdir)/build/pre.mk

nop:

all-local: nop

dist-local:
	for d in UnitTest++; do \
		if [ ! -d "$(top_distdir)/build/$$d" ]; then \
			echo "mkdir $(top_distdir)/build/$$d"; \
			mkdir "$(top_distdir)/build/$$d" >/dev/null 2>&1; \
		fi; \
	done

include $(top_srcdir)/build/generic.mk

