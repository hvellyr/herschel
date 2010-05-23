#  This file is part of the heather package 
#
#  Copyright (c) 2010 Gregor Klinke
#  All rights reserved.
#

top_srcdir=..

include $(top_srcdir)/config.mk

include $(top_srcdir)/build/pre.mk

all-local: syntax-tests

testtemp_dir = $(BUILDDIR)/temp

$(testtemp_dir): 
	@if [ ! -d "$@" ]; then mkdir $@; fi

syntax-tests:
	python2.5 runtests.py -e $(BUILDDIR)/$(BUILDSTYLE)/heather syntax/ 



include $(top_srcdir)/build/generic.mk



#----------------------------------------------------------------------

prepare-syntax-tests:
	for a in syntax/*.hea; do \
	  $(BUILDDIR)/$(BUILDSTYLE)/heather -P $$a > `dirname $$a`/`basename $$a .hea`.xml; \
  done
