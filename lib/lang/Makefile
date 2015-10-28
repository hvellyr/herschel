#  This file is part of the herschel package
#
#  Copyright (c) 2010-2011 Gregor Klinke
#  All rights reserved.
#
#  This source code is released under the BSD License.

top_srcdir = ../..

include $(top_srcdir)/config.mk

#------------------------------------------------------------------------------
# langkit.a - the base runtime library
#------------------------------------------------------------------------------

LIBLANGKIT_SOURCES =  \
	comparable.hr       \
	copyable.hr         \
	numbers.hr          \
	runtime.hr          \
	sliceable.hr        \
	string.hr           \
	types.hr            \
	impl/comparable.hr  \
	impl/numbers.hr     \
	impl/string.hr

LIBLANGKIT_OBJS = $(LIBLANGKIT_SOURCES:%.hr=$(BUILDDIR)/$(BUILDSTYLE)/%.bc)

LANGKIT_OBJ = $(BUILDDIR)/$(BUILDSTYLE)/langkit.o
LANGKIT_LIB = $(BUILDDIR)/$(BUILDSTYLE)/langkit.a


#------------------------------------------------------------------------------
# targets
#------------------------------------------------------------------------------

SOURCES = $(LIBLANGKIT_SOURCES)

include $(top_srcdir)/build/pre.mk

all-local: $(BUILDDIR) $(BUILDDIR)/$(BUILDSTYLE) LANGKIT_LIBX


#--------
LANGKIT_LIBX: $(LANGKIT_LIB)

$(LANGKIT_LIB): $(LANGKIT_OBJ)
	$(AR) crus $@ $^

$(LANGKIT_OBJ): $(LIBLANGKIT_OBJS)
	$(LLVM_EXE)/llvm-link $^ | $(LLVM_EXE)/llc -filetype=obj -o $@



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
