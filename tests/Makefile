#  This file is part of the herschel package
#
#  Copyright (c) 2010-2011 Gregor Klinke
#  All rights reserved.
#

top_srcdir=..

include $(top_srcdir)/config.mk

include $(top_srcdir)/build/pre.mk

all-local: all-tests

testtemp_dir = $(BUILDDIR)/temp

$(testtemp_dir):
	@if [ ! -d "$@" ]; then mkdir $@; fi

all-tests:
	@$(PYTHON) runtests.py $(RUNTESTOPT) -e $(BUILDDIR)/$(BUILDSTYLE) \
		-I $(top_srcdir)/tests -t $(testtemp_dir) syntax import transform annotate compile

syntax-tests:
	@$(PYTHON) runtests.py $(RUNTESTOPT) -e $(BUILDDIR)/$(BUILDSTYLE) \
		-I $(top_srcdir)/tests syntax

import-tests:
	@$(PYTHON) runtests.py $(RUNTESTOPT) -e $(BUILDDIR)/$(BUILDSTYLE) \
		-I $(top_srcdir)/tests import

transform-tests:
	@$(PYTHON) runtests.py $(RUNTESTOPT) -e $(BUILDDIR)/$(BUILDSTYLE) \
		-I $(top_srcdir)/tests transform

annotate-tests:
	@$(PYTHON) runtests.py $(RUNTESTOPT) -e $(BUILDDIR)/$(BUILDSTYLE) \
		-I $(top_srcdir)/tests annotate

compile-tests: $(testtemp_dir)
	@$(PYTHON) runtests.py $(RUNTESTOPT) -e $(BUILDDIR)/$(BUILDSTYLE) \
		-I $(top_srcdir)/tests -t $(testtemp_dir) compile

include $(top_srcdir)/build/generic.mk



#----------------------------------------------------------------------

prepare-syntax-tests:
	for a in syntax/*.h7; do \
	  $(BUILDDIR)/$(BUILDSTYLE)/hrc -P $$a > `dirname $$a`/`basename $$a .h7`.xml; \
  done
