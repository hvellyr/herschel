#  This file is part of the herschel package
#
#  Copyright (c) 2010-2011 Gregor Klinke
#  All rights reserved.
#

top_srcdir = ..

include $(top_srcdir)/config.mk

DISTFILES = \
	Makefile

include $(top_srcdir)/build/pre.mk

nop:

all-local: nop

dist-local:
	for d in catch cxxopts; do \
		if [ ! -d "$(top_distdir)/$(subdir)/$$d" ]; then \
			echo "mkdir $(top_distdir)/$(subdir)/$$d"; \
			mkdir "$(top_distdir)/$(subdir)/$$d" >/dev/null 2>&1; \
		fi; \
	done

include $(top_srcdir)/build/generic.mk

