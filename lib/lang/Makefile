#  This file is part of the herschel package
#
#  Copyright (c) 2010-2011 Gregor Klinke
#  All rights reserved.
#
#  This source code is released under the BSD License.

top_srcdir = ../..

include $(top_srcdir)/config.mk

#------------------------------------------------------------------------------
# langkit.hlib - the base runtime library
#------------------------------------------------------------------------------

LIBLANGKIT_SOURCES =  \
	numbers.h7          \
	types.h7            \
	runtime.h7          \
	sliceable.h7	\
	impl/numbers.h7

LIBLANGKIT_OBJS = $(LIBLANGKIT_SOURCES:%.h7=$(BUILDDIR)/$(BUILDSTYLE)/%.bc)

LANGKIT_LIB = $(BUILDDIR)/$(BUILDSTYLE)/langkit.hlib


#------------------------------------------------------------------------------
# targets
#------------------------------------------------------------------------------

SOURCES = $(LIBLANGKIT_SOURCES)

include $(top_srcdir)/build/pre.mk

all-local: $(BUILDDIR) $(BUILDDIR)/$(BUILDSTYLE) LANGKIT_LIBX


#--------
LANGKIT_LIBX: $(LANGKIT_LIB)

$(LANGKIT_LIB): $(LIBLANGKIT_OBJS)
	$(LLVM_EXE)/llvm-link -o $(BUILDDIR)/$(BUILDSTYLE)/langkit.bc $^
	$(LLVM_EXE)/llvm-ar crus $@ $(BUILDDIR)/$(BUILDSTYLE)/langkit.bc




#------------------------------------------------------------------------------
DISTFILES = Makefile $(SOURCES)

clean-local:
	rm -f $(BUILDDIR)/$(BUILDSTYLE)/*.bc $(BUILDDIR)/$(BUILDSTYLE)/impl/*.bc $(LANGKIT_LIB)

distclean-local: clean-local
	-rm -rf $(DEPS_DIR) TAGS

rebuild: clean all



# DEPS=

.PRECIOUS: $(BUILDDIR)/$(BUILDSTYLE)/*.bc

# -include $(DEPS)

include $(top_srcdir)/build/generic.mk
