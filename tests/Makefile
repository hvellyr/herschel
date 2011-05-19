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

ifeq ($(srcdir),)
TESTS_DIR = .
else
TESTS_DIR = $(srcdir)/tests
endif

ifeq ($(curdir),)
BASE_DIR = .
else
BASE_DIR = $(curdir)
endif

$(testtemp_dir):
	@if [ ! -d "$@" ]; then mkdir $@; fi

all-tests: $(testtemp_dir)
	@(cd $(BASE_DIR); \
	  $(PYTHON) $(TESTS_DIR)/runtests.py $(RUNTESTOPT) -e $(BUILDDIR)/$(BUILDSTYLE) \
		-I $(TESTS_DIR) -t $(testtemp_dir) \
	  $(TESTS_DIR)/syntax \
	  $(TESTS_DIR)/import \
	  $(TESTS_DIR)/transform \
	  $(TESTS_DIR)/annotate \
		$(TESTS_DIR)/compile )

syntax-tests:
	@(cd $(BASE_DIR); \
	  $(PYTHON) $(TESTS_DIR)/runtests.py $(RUNTESTOPT) -e $(BUILDDIR)/$(BUILDSTYLE) \
		  -I $(TESTS_DIR) $(TESTS_DIR)/syntax )

import-tests:
	@(cd $(BASE_DIR); \
	  $(PYTHON) $(TESTS_DIR)/runtests.py $(RUNTESTOPT) -e $(BUILDDIR)/$(BUILDSTYLE) \
		  -I $(TESTS_DIR) $(TESTS_DIR)/import )

transform-tests:
	@(cd $(BASE_DIR); \
	  $(PYTHON) $(TESTS_DIR)/runtests.py $(RUNTESTOPT) -e $(BUILDDIR)/$(BUILDSTYLE) \
		  -I $(TESTS_DIR) $(TESTS_DIR)/transform )

annotate-tests:
	@(cd $(BASE_DIR); \
	  $(PYTHON) $(TESTS_DIR)/runtests.py $(RUNTESTOPT) -e $(BUILDDIR)/$(BUILDSTYLE) \
		  -I $(TESTS_DIR) $(TESTS_DIR)/annotate )

compile-tests: $(testtemp_dir)
	@(cd $(BASE_DIR); \
	  $(PYTHON) $(TESTS_DIR)/runtests.py $(RUNTESTOPT) -e $(BUILDDIR)/$(BUILDSTYLE) \
		 -I $(TESTS_DIR) -t $(testtemp_dir) \
	   $(TESTS_DIR)/compile )

include $(top_srcdir)/build/generic.mk



#----------------------------------------------------------------------

prepare-syntax-tests:
	for a in syntax/*.h7; do \
	  $(BUILDDIR)/$(BUILDSTYLE)/hrc -P $$a > `dirname $$a`/`basename $$a .h7`.xml; \
  done
