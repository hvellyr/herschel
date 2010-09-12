#  This file is part of the heather package 
#
#  Copyright (c) 2010 Gregor Klinke
#  All rights reserved.
#

top_srcdir=../../..
pc_top_srcdir="..\\\\..\\\\.."

include $(top_srcdir)/config.mk

PC_INSTALLER_SCRIPT=heather.nsi
PC_INSTALLER=heather-$(VERSION)-installer.exe

DISTFILES = \
	Makefile README heather.nsi.in

include $(top_srcdir)/build/pre.mk

all-local: $(PC_INSTALLER)

clean-local:

distclean-local: clean-local

$(PC_INSTALLER_SCRIPT): heather.nsi.in Makefile
	sed -e"s/\(@VERSION@\)/${VERSION}/g" $< > tmp.$$$$; \
	sed -e"s,\(@top_srcdir@\),${pc_top_srcdir},g" tmp.$$$$ > tmp.2.$$$$; \
	mv tmp.2.$$$$ $@; \
	rm tmp.$$$$


$(PC_INSTALLER): $(PC_INSTALLER_SCRIPT)
	 $(MAKENSIS) $<

include $(top_srcdir)/build/generic.mk
