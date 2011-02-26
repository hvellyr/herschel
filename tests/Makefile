#  This file is part of the herschel package
#
#  Copyright (c) 2010-2011 Gregor Klinke
#  All rights reserved.
#

top_srcdir=..

include $(top_srcdir)/config.mk

include $(top_srcdir)/build/pre.mk

all-local: syntax-tests import-tests transform-tests annotate-tests compile-tests

testtemp_dir = $(BUILDDIR)/temp

$(testtemp_dir):
	@if [ ! -d "$@" ]; then mkdir $@; fi

syntax-tests:
	@$(PYTHON) runtests.py $(RUNTESTOPT) -e $(BUILDDIR)/$(BUILDSTYLE)/hrc \
		-I $(top_srcdir)/tests -D syntax syntax/

import-tests:
	@$(PYTHON) runtests.py $(RUNTESTOPT) -e $(BUILDDIR)/$(BUILDSTYLE)/hrc \
		-I $(top_srcdir)/tests -D import import/

transform-tests:
	@$(PYTHON) runtests.py $(RUNTESTOPT) -e $(BUILDDIR)/$(BUILDSTYLE)/hrc \
		-I $(top_srcdir)/tests -D transform transform/

annotate-tests:
	@$(PYTHON) runtests.py $(RUNTESTOPT) -e $(BUILDDIR)/$(BUILDSTYLE)/hrc \
		-I $(top_srcdir)/tests -D annotate annotate/

compile-tests: $(testtemp_dir)
	@$(PYTHON) runtests.py $(RUNTESTOPT) -e $(BUILDDIR)/$(BUILDSTYLE)/herschel \
		-I $(top_srcdir)/tests -D compile -t $(testtemp_dir) compile/

include $(top_srcdir)/build/generic.mk



#----------------------------------------------------------------------

prepare-syntax-tests:
	for a in syntax/*.hr; do \
	  $(BUILDDIR)/$(BUILDSTYLE)/hrc -P $$a > `dirname $$a`/`basename $$a .hr`.xml; \
  done
