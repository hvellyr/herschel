#  This file is part of the heather package
#
#  Copyright (c) 2010 Gregor Klinke
#  All rights reserved.
#

top_srcdir=..

include $(top_srcdir)/config.mk

include $(top_srcdir)/build/pre.mk

all-local: syntax-tests import-tests transform-tests annotate-tests

testtemp_dir = $(BUILDDIR)/temp

$(testtemp_dir):
	@if [ ! -d "$@" ]; then mkdir $@; fi

syntax-tests:
	@$(PYTHON) runtests.py -e $(BUILDDIR)/$(BUILDSTYLE)/heather \
		-I $(top_srcdir)/tests -D syntax syntax/

import-tests:
	@$(PYTHON) runtests.py -e $(BUILDDIR)/$(BUILDSTYLE)/heather \
		-I $(top_srcdir)/tests -D import import/

transform-tests:
	@$(PYTHON) runtests.py -e $(BUILDDIR)/$(BUILDSTYLE)/heather \
		-I $(top_srcdir)/tests -D transform transform/

annotate-tests:
	@$(PYTHON) runtests.py -e $(BUILDDIR)/$(BUILDSTYLE)/heather \
		-I $(top_srcdir)/tests -D annotate annotate/

include $(top_srcdir)/build/generic.mk



#----------------------------------------------------------------------

prepare-syntax-tests:
	for a in syntax/*.hea; do \
	  $(BUILDDIR)/$(BUILDSTYLE)/heather -P $$a > `dirname $$a`/`basename $$a .hea`.xml; \
  done
