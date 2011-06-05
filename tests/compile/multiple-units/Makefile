top_srcdir=../../..

include $(top_srcdir)/config.mk

include $(top_srcdir)/build/pre.mk

ifeq ($(curdir),)
RUNTIME_DIR = $(top_srcdir)/lib
else
RUNTIME_DIR = $(abspath ../../../lib)
endif


TESTDIR ?= $(BUILDTESTSDIR)

all-local: $(TESTDIR)/$(APPNAME)$(APPEXT)

$(TESTDIR)/$(APPNAME)$(APPEXT): $(TESTDIR)/employee.bc $(TESTDIR)/student.bc $(TESTDIR)/person.bc
	@$(LLVM_EXE)/llvm-ld -native -o $@ $^ $(BUILDDIR)/$(BUILDSTYLE)/libhr.a


$(TESTDIR)/%.bc: %.h7
	@$(HERSCHEL_APPBINARY) -d $(TESTDIR) --isys=$(RUNTIME_DIR) -c $<

clean-local:
	rm -f $(TESTDIR)/*

include $(top_srcdir)/build/generic.mk

.SUFFIXES: .bc .h7
.PRECIOUS: %.bc
