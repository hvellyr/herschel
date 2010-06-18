#  This file is part of the heather package 
#
#  Copyright (c) 2010 Gregor Klinke
#  All rights reserved.
#

top_srcdir = ../..

include $(top_srcdir)/config.mk

include $(top_srcdir)/build/pre.mk

SUBDIRS = win mac

DISTFILES = Makefile

clean: clean-local clean-rec clean-post

distclean: distclean-local distclean-rec distclean-post

nop:

all: nop all-local

include $(top_srcdir)/build/generic.mk

