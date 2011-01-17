#  This file is part of the herschel package 
#
#  Copyright (c) 2010 Gregor Klinke
#  All rights reserved.
#

top_srcdir=../../..

include $(top_srcdir)/config.mk

MAC_DMG = $(top_srcdir)/packages/$(HR_PACKAGE)-$(VERSION).dmg

DISTFILES = \
	Makefile make-pkg.sh Info.plist.in Desc.plist background.jpg

include $(top_srcdir)/build/pre.mk

all-local: $(MAC_DMG)

clean-local:
	-rm -rf $(top_srcdir)/build/mac/$(HR_PACKAGE)-$(VERSION)-image 
	-rm -rf $(top_srcdir)/build/mac/$(HR_PACKAGE)-$(VERSION)

distclean-local: clean-local
	-rm -f Info.plist

Info.plist: Info.plist.in
	sed -e"s/\(@VERSION@\)/${VERSION}/g" $< > tmp.$$$$; \
	mv tmp.$$$$ $@

$(MAC_DMG): Info.plist Desc.plist $(top_srcdir)/temp/dist-release/herschel
	 $(SHELL) make-pkg.sh $(top_srcdir) $(PACKAGE) $(HR_PACKAGE) $(VERSION) $@


include $(top_srcdir)/build/generic.mk
