#  This file is part of the herschel package
#
#  Copyright (c) 2010-2011 Gregor Klinke
#  All rights reserved.
#
#  This source code is released under the BSD License.

top_srcdir = ..

include $(top_srcdir)/config.mk

SUBDIRS = dist

DISTFILES = \
	Makefile generic.mk pre.mk platform.sh gccver.sh sysconf.h config.mk

include $(top_srcdir)/build/pre.mk

nop:

all-local: nop

dist-local:
	for d in mac; do \
		if [ ! -d "$(top_distdir)/build/$$d" ]; then \
			echo "mkdir $(top_distdir)/build/$$d"; \
			mkdir $(top_distdir)/build/$$d >/dev/null 2>&1; \
		fi; \
		for f in $$d/*.plist $$d/*.icns; do \
			if [ -f "$$f" ]; then \
				echo "cp $$f $(top_distdir)/build/$$f"; \
				cp -p $$f `dirname $(top_distdir)/build/$$f` >/dev/null 2>&1; \
			fi; \
		done; \
	done

include $(top_srcdir)/build/generic.mk

