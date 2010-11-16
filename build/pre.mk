#  This file is part of the heather package 
#
#  Copyright (c) 2010 Gregor Klinke
#  All rights reserved.
#

ifdef SUBDIRS

#### MAKEFILE WITH SUBDIRS ####
all: all-local all-rec all-post

clean: clean-local clean-rec clean-post

#dist: dist-local dist-rec dist-post

distclean: distclean-local distclean-rec distclean-post

else
#### MAKEFILE WITHOUT SUBDIRS ####
all: all-local all-post

clean: clean-local clean-post

distclean: distclean-local distclean-post

#dist: dist-local

endif


REC_TARGETS = all-rec clean-rec distclean-rec dist-rec
$(REC_TARGETS): 
	target=`echo $@ | sed s/-rec//`; \
	for subdir in $(SUBDIRS); do \
	  echo "Making $$target in $$subdir"; \
		$(MAKE) -C $$subdir  $$target; \
	done


$(BUILDDIR): 
	@if [ ! -d $@ ]; then mkdir $@; fi

$(BUILDDIR)/$(BUILDSTYLE): $(BUILDDIR)
	@if [ ! -d $@ ]; then mkdir $@; fi

$(PKGDIR): 
	@if [ ! -d $@ ]; then mkdir $@; fi

DEPS_DIR = $(BUILDDIR)/$(BUILDSTYLE)/.deps
