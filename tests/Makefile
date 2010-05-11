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

syntax-tests: pass1-tests

# pass2-tests

pass1-tests: $(testtemp_dir)
	for testf in syntax/*.hea; do \
		xmlf=`dirname $$testf`/`basename $$testf .hea`_1.xml; \
		destf=$(testtemp_dir)/`basename $$testf .hea`_1.xml; \
	  $(BUILDDIR)/$(BUILDSTYLE)/heather -T pass1 -P $$testf > $$destf; \
		python comparexml.py $$xmlf $$destf; \
  done

pass2-tests: $(testtemp_dir)
	for testf in syntax/*.hea; do \
		xmlf=`dirname $$testf`/`basename $$testf .hea`_2.xml; \
		destf=$(testtemp_dir)/`basename $$testf .hea`_2.xml; \
	  $(BUILDDIR)/$(BUILDSTYLE)/heather -T pass2 -P $$testf > $$destf; \
		python comparexml.py $$xmlf $$destf; \
  done



include $(top_srcdir)/build/generic.mk



#----------------------------------------------------------------------

prepare-syntax-tests:
	for a in syntax/*.hea; do \
	  $(BUILDDIR)/$(BUILDSTYLE)/heather -P $$a > `dirname $$a`/`basename $$a .hea`.xml; \
  done
