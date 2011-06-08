top_srcdir=../../..

include $(top_srcdir)/config.mk

include $(top_srcdir)/build/pre.mk

ifeq ($(curdir),)
RUNTIME_DIR = $(top_srcdir)/lib
else
RUNTIME_DIR = $(abspath ../../../lib)
endif


TESTDIR ?= $(BUILDTESTSDIR)
APPNAME ?= gf

.SUFFIXES: .bc .h7

TEST_SOURCES = main.h7 dog.h7 cat.h7

TEST_OBJS = $(TEST_SOURCES:%.h7=$(TESTDIR)/%.bc)


all-local: $(TESTDIR)/$(APPNAME)$(APPEXT)

$(TESTDIR)/$(APPNAME)$(APPEXT): $(TEST_OBJS)
	@$(LLVM_EXE)/llvm-ld -native -o $@ $^ $(BUILDDIR)/$(BUILDSTYLE)/libhr.a


$(TESTDIR)/%.bc: %.h7
	@$(HERSCHEL_APPBINARY) --isys=$(RUNTIME_DIR) -d $(TESTDIR) -c $<

clean-local:
	rm -f $(TESTDIR)/*

include $(top_srcdir)/build/generic.mk

.PRECIOUS: %.bc
