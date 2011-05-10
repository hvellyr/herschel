#  This file is part of the herschel package
#
#  Copyright (c) 2010-2011 Gregor Klinke
#  All rights reserved.
#
#  This source code is released under the BSD License.

all-local:

clean-local:

distclean-local:

dist-local:

all-post:

clean-post:

distclean-post:

dist-post:

distdir: $(DISTFILES)
	-chmod -R a+w $(distdir) >/dev/null 2>&1; rm -rf $(distdir)
	mkdir $(distdir)
	for file in $(DISTFILES); do \
	  d=$(srcdir); \
	  if test -d $$d/$$file; then \
	    cp -pr $$d/$$file $(distdir)/$$file; \
	  else \
	    test -f $(distdir)/$$file \
	    || ln $$d/$$file $(distdir)/$$file 2> /dev/null \
	    || cp -p $$d/$$file $(distdir)/$$file || :; \
	  fi; \
	done; \
	for subdir in $(SUBDIRS); do \
	  if test "$$subdir" = .; then :; else \
	    test -d $(distdir)/$$subdir \
	    || mkdir $(distdir)/$$subdir \
	    || exit 1; \
	    chmod 777 $(distdir)/$$subdir; \
	    (cd $$subdir && $(MAKE) top_distdir=../$(distdir) \
	 		 distdir=../$(distdir)/$$subdir distdir) \
	        || exit 1; \
	    (cd $$subdir && $(MAKE) top_distdir=../$(distdir) \
	 		 distdir=../$(distdir)/$$subdir dist-local) \
	        || exit 1; \
	  fi; \
	done


## ------- rules

$(BUILDDIR)/$(BUILDSTYLE)/%$(OBJEXT) : %.c
	$(CC) $(CFLAGS) -c -o $@ $<

$(BUILDDIR)/$(BUILDSTYLE)/%$(OBJEXT) : %.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $<

$(BUILDDIR)/$(BUILDSTYLE)/%$(DYOEXT) : %.c
	$(CC) $(PIC) $(CFLAGS) -c -o $@ $<

$(BUILDDIR)/$(BUILDSTYLE)/%$(DYOEXT) : %.cpp
	$(CXX) $(PIC) $(CXXFLAGS) -c -o $@ $<

$(DEPS_DIR)/%.d: %.c $(PRE_DEPS)
	$(top_srcdir)/build/mkinstalldirs $(DEPS_DIR); \
	$(CC) -MM $(CFLAGS) $< > $@.$$$$; \
	sed 's,\($*\)\$(OBJEXT)[ :]*,$(BUILDDIR)/$(BUILDSTYLE)/\1$(OBJEXT) $@ : ,g' < $@.$$$$ > $@; \
	rm -f $@.$$$$

$(DEPS_DIR)/%.d: %.cpp $(PRE_DEPS)
	$(top_srcdir)/build/mkinstalldirs $(DEPS_DIR); \
	$(CXX) -MM $(CXXFLAGS) $< > $@.$$$$; \
	sed 's,\($*\)\$(OBJEXT)[ :]*,$(BUILDDIR)/$(BUILDSTYLE)/\1$(OBJEXT) $@ : ,g' < $@.$$$$ > $@; \
	rm -f $@.$$$$

$(DEPS_DIR)/%.ld: %.c $(PRE_DEPS)
	$(top_srcdir)/build/mkinstalldirs $(DEPS_DIR); \
	$(CC) -MM $(CFLAGS) $< > $@.$$$$; \
	sed 's,\($*\)\$(DYOEXT)[ :]*,$(BUILDDIR)/$(BUILDSTYLE)/\1$(DYOEXT) $@ : ,g' < $@.$$$$ > $@; \
	rm -f $@.$$$$

$(DEPS_DIR)/%.ld: %.cpp $(PRE_DEPS)
	$(top_srcdir)/build/mkinstalldirs $(DEPS_DIR); \
	$(CXX) -MM $(CFLAGS) $< > $@.$$$$; \
	sed 's,\($*\)\$(DYOEXT)[ :]*,$(BUILDDIR)/$(BUILDSTYLE)/\1$(DYOEXT) $@ : ,g' < $@.$$$$ > $@; \
	rm -f $@.$$$$


%$(LIBEXT)(%$(OBJEXT)): %$(OBJEXT)
	$(AR) cru $@ $%


# -include $(ALL_DEPS)

.PRECIOUS: %.o %.lo $(DEPS_DIR)/%.d $(DEPS_DIR)/%.ld

.PHONY: $(DEPS_DIR)

.SUFFIXES:
.SUFFIXES: .c .cpp .o .lo .m .d .ld .y .h