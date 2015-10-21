top_srcdir=../../..

include $(top_srcdir)/config.mk

include $(top_srcdir)/build/pre.mk

ifeq ($(curdir),)
RUNTIME_DIR = $(top_srcdir)/lib
else
RUNTIME_DIR = $(abspath ../../../lib)
endif


TESTDIR ?= $(BUILDTESTSDIR)
APPNAME ?= mu

TEST_SOURCES = employee.hr student.hr person.hr
TEST_BCS = $(TEST_SOURCES:%.hr=$(TESTDIR)/%.bc)

all-local: $(TESTDIR)/$(APPNAME)$(APPEXT)

$(TESTDIR)/$(APPNAME)$(APPEXT): $(TESTDIR)/employee.bc $(TESTDIR)/student.bc $(TESTDIR)/person.bc
	@$(LLVM_EXE)/llvm-link $^ | $(LLVM_EXE)/llc -filetype=obj -o $(TESTDIR)/tmp-$(APPNAME)$(OBJEXT)
	@$(CC) -o $@ $(TESTDIR)/tmp-$(APPNAME)$(OBJEXT) $(BUILDDIR)/$(BUILDSTYLE)/libhr.a


$(TESTDIR)/%.bc: %.hr
	@$(HERSCHEL_APPBINARY) -d $(TESTDIR) --isys=$(RUNTIME_DIR) -c $<

clean-local:
	rm -f $(TESTDIR)/*

include $(top_srcdir)/build/generic.mk

.SUFFIXES: .bc .hr
.PRECIOUS: %.bc
