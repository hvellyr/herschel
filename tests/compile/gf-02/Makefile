top_srcdir=../../..

include $(top_srcdir)/config.mk

include $(top_srcdir)/build/pre.mk

TESTDIR ?= $(BUILDTESTSDIR)
APPNAME ?= gf


all-local: $(TESTDIR)/$(APPNAME)$(APPEXT)

$(TESTDIR)/$(APPNAME)$(APPEXT): $(TESTDIR)/main.bc $(TESTDIR)/dog.bc $(TESTDIR)/cat.bc
	$(LLVM_EXE)/llvm-ld -native -o $@ $^ $(BUILDDIR)/$(BUILDSTYLE)/libhr.a


$(TESTDIR)/%.bc : %.h7
	$(HERSCHEL_APPBINARY) -d $(TESTDIR) -c $<

clean-local:
	rm -f $(TESTDIR)/*

include $(top_srcdir)/build/generic.mk

#.PRECIOUS: %.bc
