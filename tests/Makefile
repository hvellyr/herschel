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

syntax-tests: $(testtemp_dir)
	for testf in syntax/*.hea; do \
		xmlf=`dirname $$testf`/`basename $$testf .hea`.xml; \
		destf=$(testtemp_dir)/`basename $$testf .hea`.xml; \
	  $(BUILDDIR)/$(BUILDSTYLE)/heather -P $$testf > $$destf; \
		python comparexml.py $$xmlf $$destf; \
  done



include $(top_srcdir)/build/generic.mk



#----------------------------------------------------------------------

prepare-syntax-tests:
	for a in syntax/*.hea; do \
	  $(BUILDDIR)/$(BUILDSTYLE)/heather -P $$a > `dirname $$a`/`basename $$a .hea`.xml; \
  done
