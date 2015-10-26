top_srcdir=../../..

include $(top_srcdir)/config.mk

include $(top_srcdir)/build/pre.mk

ifeq ($(curdir),)
RUNTIME_DIR = $(top_srcdir)/lib
else
RUNTIME_DIR = $(abspath ../../../lib)
endif


TESTDIR ?= $(BUILDTESTSDIR)
APPNAME ?= keyw

TEST_SOURCES = one.hr two.hr three.hr
TEST_BCS = $(TEST_SOURCES:%.hr=$(TESTDIR)/%.bc)

all-local: $(TESTDIR)/$(APPNAME)$(APPEXT)

$(TESTDIR)/$(APPNAME)$(APPEXT): $(TEST_BCS)
	@$(LLVM_EXE)/llvm-link $^ | $(LLVM_EXE)/llc -filetype=obj -o $(TESTDIR)/tmp-$(APPNAME)$(OBJEXT)
	@$(CC) -o $@ $(TESTDIR)/tmp-$(APPNAME)$(OBJEXT) $(BUILDDIR)/$(BUILDSTYLE)/libhr.a


$(TESTDIR)/%.bc: %.hr
	@$(HERSCHEL_APPBINARY) -d $(TESTDIR) --isys=$(RUNTIME_DIR) -c $<

clean-local:
	rm -f $(TESTDIR)/*

include $(top_srcdir)/build/generic.mk

.SUFFIXES: .bc .hr
.PRECIOUS: %.bc
