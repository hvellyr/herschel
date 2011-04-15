top_srcdir=../../..

include $(top_srcdir)/config.mk

include $(top_srcdir)/build/pre.mk

TESTDIR ?= $(BUILDTESTSDIR)


all-local: $(TESTDIR)/$(APPNAME)$(APPEXT)

$(TESTDIR)/$(APPNAME)$(APPEXT): $(TESTDIR)/employee.bc $(TESTDIR)/student.bc $(TESTDIR)/person.bc
	$(LLVM_EXE)/llvm-ld -native -o $@ $^ $(BUILDDIR)/$(BUILDSTYLE)/libhr.a


$(TESTDIR)/%.bc : %.h7
	$(HERSCHEL_APPBINARY) -d $(TESTDIR) -c $<

clean-local:
	rm -f $(TESTDIR)/*

include $(top_srcdir)/build/generic.mk

#.PRECIOUS: %.bc
