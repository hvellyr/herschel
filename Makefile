#  This file is part of the heather package 
#
#  Copyright (c) 2010 Gregor Klinke
#  All rights reserved.
#

top_srcdir = .

include $(top_srcdir)/config.mk

#----------------------------------------------------------------------

SUBDIRS = bootstrap build doc tests

DISTFILES = \
	AUTHORS   \
	BUGS      \
	LICENSE   \
	INSTALL   \
	NEWS      \
	Makefile  \
	README    \
	TODO      \
	config.mk 

include $(top_srcdir)/build/pre.mk

.PHONY: build tests docs 

build: version.h config-local.h $(BUILDDIR) $(BUILDDIR)/$(BUILDSTYLE)
	(cd bootstrap && $(MAKE) all)

tests: build
	(cd tests/ && $(MAKE) all)

docs: doc/version.texinfo $(BUILDDIR) 
	(cd doc/ && $(MAKE) all)


all-local: version.h config-local.h doc/version.texinfo $(BUILDDIR) $(BUILDDIR)/$(BUILDSTYLE)

TAGS:
	find -E . -regex ".*\.mm$$|.*\.cpp$$|.*\.h$$" -print | etags -o TAGS -

clean-local:
	rm -rf $(distdir) $(info-distdir) $(pdf-distdir)

distclean-post: clean-post
	rm -rf TAGS temp/ version.h config-local.h $(PKGDIR) $(BUILDDIR)


#--- doc distribution ------------------------------

dist-doc-info-prepare: $(BUILDDIR)
	-chmod -R a+w $(BUILDDIR)/$(info-distdir) >/dev/null 2>&1; rm -rf $(BUILDDIR)/$(info-distdir)
	mkdir $(BUILDDIR)/$(info-distdir)
	(cd doc/; \
	 $(MAKE) -k info; \
	 cp -f README.dist-info ../$(BUILDDIR)/$(info-distdir)/README >/dev/null 2>&1; \
	 cp -f -r *.info-[0-9] ../$(BUILDDIR)/$(info-distdir) >/dev/null 2>&1; \
	 cp -f -r *.info ../$(BUILDDIR)/$(info-distdir) >/dev/null 2>&1)

dist-doc-pdf-prepare: $(BUILDDIR)
	-chmod -R a+w $(BUILDDIR)/$(pdf-distdir) >/dev/null 2>&1; rm -rf $(BUILDDIR)/$(pdf-distdir)
	mkdir $(BUILDDIR)/$(pdf-distdir)
	(cd doc/; \
	 $(MAKE) -k pdf; \
	 cp -f README.dist-pdf ../$(BUILDDIR)/$(pdf-distdir)/README >/dev/null 2>&1; \
	 cp -f -r *.pdf ../$(BUILDDIR)/$(pdf-distdir) >/dev/null 2>&1)

dist-doc-info-zip: $(PKGDIR) dist-doc-info-prepare
	(cd $(BUILDDIR); \
	 $(ZIP) -r ../$(PKGDIR)/$(info-doc-pkg-name).zip $(info-distdir))

dist-doc-pdf-zip: $(PKGDIR) dist-doc-pdf-prepare
	(cd $(BUILDDIR); \
	 $(ZIP) -r ../$(PKGDIR)/$(pdf-doc-pkg-name).zip $(pdf-distdir))

dist-doc-info-tgz: $(PKGDIR) dist-doc-info-prepare
	(cd $(BUILDDIR); \
	 $(TAR) czvf ../$(PKGDIR)/$(info-doc-pkg-name).tar.gz $(info-distdir))

dist-doc-pdf-tgz: $(PKGDIR) dist-doc-pdf-prepare
	(cd $(BUILDDIR); \
	 $(TAR) czf ../$(PKGDIR)/$(pdf-doc-pkg-name).tar.gz $(pdf-distdir))

dist-doc-info: dist-doc-info-zip dist-doc-info-tgz

dist-doc-pdf: dist-doc-pdf-zip dist-doc-pdf-tgz

dist-doc: dist-doc-info dist-doc-pdf


#--- src distribution ------------------------------

dist-src-prepare: $(BUILDDIR) distdir

dist-src-zip: $(PKGDIR) dist-src-prepare
	(cd $(BUILDDIR); \
	 $(ZIP) -r ../$(PKGDIR)/$(src-pkg-name).zip $(PACKAGE)-$(VERSION))

dist-src-tgz: $(PKGDIR) dist-src-prepare
	(cd $(BUILDDIR); \
	 $(TAR) czf ../$(PKGDIR)/$(src-pkg-name).tar.gz $(PACKAGE)-$(VERSION))

dist-src: dist-src-zip dist-src-tgz

dist: dist-src


#--- binary distribution ------------------------------

dist-bin-pc: $(BUILDDIR) $(PKGDIR) all-local
	if [ "$(TARGET_OS)" = "win" ]; then \
	  echo "Prepare windows distribution"; \
	  (cd src && $(MAKE) all); \
	  pkg_name=$(PACKAGE)-pc-i386-$(VERSION); \
	  bin_pkgdir=$(BUILDDIR)/$$pkg_name; \
	  mkdir $$bin_pkgdir; \
	  cp doc/README.dist-pc-bin $$bin_pkgdir/README; \
	  cp $(BUILDDIR)/heather.exe $$bin_pkgdir/heather.exe; \
	  (cd $(BUILDDIR); \
	   $(ZIP) -r ../$(PKGDIR)/$$pkg_name.zip $$pkg_name); \
	fi

dist-bin-pkg: all-local
	(cd src && BUILDSTYLE=dist-$(BUILDSTYLE) $(MAKE) all)


#----------------------------------------------------------------------

# windows binary distribution

dist-windows: $(PKGDIR) 
	(BUILDSTYLE=release $(MAKE) dist-bin-pkg); \
	(cd build/dist/win && $(MAKE) all); \
	cp build/dist/win/heather-*.exe $(PKGDIR)


#----------------------------------------------------------------------

# mac os-x binary distribution

dist-macosx: $(PKGDIR) 
	(BUILDSTYLE=release $(MAKE) dist-bin-pkg); \
	(cd build/dist/mac && $(MAKE) all)


#----------------------------------------------------------------------

dist-all: dist-src dist-doc


version.h: Makefile config.mk 
	@echo "/* Don't edit this file.  It has been created automatically. */" > $@
	@echo "#ifndef heather_version_h" >> $@
	@echo "#define heather_version_h" >> $@
	@echo "#define PACKAGE \"$(PACKAGE)\"" >> $@
	@echo "#define VERSION \"$(VERSION)\"" >> $@
	@echo "#define COPYRIGHTYEAR \"$(COPYRIGHTYEAR)\"" >> $@
	@echo "#define COPYRIGHTOWNER \"$(COPYRIGHTOWNER)\"" >> $@
	@echo "#define HEA_HOSTTYPE \"$(HEA_TARGET_DESC)\"" >> $@
	@echo "#define HEA_BASE_REVISION \"$(BASE_REVISION)\"" >> $@
	@echo "#endif" >> $@

doc/version.texinfo: Makefile config.mk
	@echo "@c Don't edit this file.  It has been created automatically." > $@
	@echo "@set VERSION $(LANG_VERSION) " > $@
	@echo "@set COPYRIGHTYEAR $(COPYRIGHTYEAR) " >> $@
	@echo "@set COPYRIGHTOWNER $(COPYRIGHTOWNER) " >> $@
	@echo "@set BASEREVISION $(BASE_REVISION) " >> $@

config-local.h: Makefile config.mk 
	@echo "/* Don't edit this file.  It has been created automatically. */" > $@
	@echo "#ifndef heather_config_local_h" > $@
	@echo "#define heather_config_local_h" >> $@
	@echo "#define HEA_INSTDIR_prefix \"$(prefix)/\"" >> $@
	@echo "#define HEA_INSTDIR_bindir \"$(bindir)/\"" >> $@
	@echo "#define HEA_INSTDIR_libdir \"$(libdir)/\"" >> $@
	@echo "#define HEA_INSTDIR_pkglibdir \"$(pkglibdir)/\"" >> $@
	@echo "#define HEA_INSTDIR_datadir \"$(datadir)/\"" >> $@
	@echo "#define HEA_INSTDIR_pkgdatadir \"$(pkgdatadir)/\"" >> $@
	@echo "#endif" >> $@


include $(top_srcdir)/build/generic.mk
