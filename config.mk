#  This file is part of the herschel package
#
#  Copyright (c) 2010-2011 Gregor Klinke
#  All rights reserved.
#

srcdir = .

# ----------------------------------------------------------------------

PACKAGE = herschel
VERSION = 0.1
COPYRIGHTYEAR = 2002, 2003, 2009-2011
COPYRIGHTOWNER = Gregor C. Klinke
# human readable package name, used for Mac packages, etc.
HR_PACKAGE = Herschel

LANG_VERSION = 0.2.1

# ----------------------------------------------------------------------
# plattfrom settings
# OS = mac | win | linux | haiku
# ARCH = i386 | ppc
# FLAVOUR = plain | mingw32 | cygwin | darwin

TARGET_OS    ?= $(shell $(SHELL) $(top_srcdir)/build/platform.sh -m)
TARGET_ARCH  ?= $(shell $(SHELL) $(top_srcdir)/build/platform.sh -a)
TARGET_OSVER ?= $(shell $(SHELL) $(top_srcdir)/build/platform.sh -r)
TARGET_OSFLV ?= $(shell $(SHELL) $(top_srcdir)/build/platform.sh -f)
HR_TARGET_DESC ?= $(shell $(SHELL) $(top_srcdir)/build/platform.sh)

# OS_FLAGS = -DOS_$(TARGET_OS) -DARCH_$(TARGET_ARCH) -DOSFLAV_$(TARGET_OSFLV)
OS_FLAGS = -DOS_$(TARGET_OS) -DARCH_$(TARGET_ARCH) # -DOSFLAV_$(TARGET_OSFLV)

#CC = gcc
#CXX = g++
#MAKE = make
#AR = ar
#RANLIB = ranlib
#SHELL = /bin/sh
#DYLD =

#INSTALL     = /usr/bin/install -c
#INSTALLDATA = /usr/bin/install -c -m 644

# ZIP = zip
# TAR = tar

# Give the CC version. Known values:
# gcc3.0
# gcc4.0, gcc4.1, gcc4.2, gcc4.3, gcc4.4, gcc4.5

CC_VERSION ?= $(shell CC=$(CC) $(SHELL) $(top_srcdir)/build/gccver.sh)

BASE_REVISION = $(shell $(SHELL) $(top_srcdir)/build/reprevision.sh)


# ----------------------------------------------------------------------
# Features:

# activate for performance measures, if you want to see the cost of various
# functions
# COPT ?= -DNO_INLINE

#NLS        += -DHAVE_LOCALE_H
#NLS        += -DHAVE_SETLOCALE
#NLS        += -DENABLE_NLS
#NLSLIBS     = -lgettext
#NLS_LDFLAGS =


# ----------------------------------------------------------------------

#CDEBUG += -g
#COPT += -O0
#CWARNING += -Winline
#CXXADDS =
#LDADDS

#DYLIBEXT = .so
#LYOEXT = .lo
#LIBEXT = .a
#OBJEXT = .o
#APPEXT =

#PIC =


-include $(top_srcdir)/config-local.mk
-include $(top_srcdir)/build/config.mk

BUILDSTYLE ?= debug

ifeq ($(BUILDSTYLE),debug)
CBUILDFLAG = $(CDEBUG) -DIS_DEBUG -DUNITTESTS
endif
ifeq ($(BUILDSTYLE),release)
# CBUILDFLAG = -DNDEBUG $(COPT) -DIS_RELEASE
CBUILDFLAG = $(COPT) -DIS_RELEASE
endif

# don't use the Buildstyle dist-release explicitely, it is automatically
# used when calling make dist-bin-pkg on toplevel.
ifeq ($(BUILDSTYLE),dist-release)
# CBUILDFLAG = -DNDEBUG $(COPT) -DIS_RELEASE -DIS_DIST
CBUILDFLAG = $(COPT) -DIS_RELEASE -DIS_DIST
endif


# ----------------------------------------------------------------------
DEFS   += $(WX_CXXFLAGS)
LIBS   += -lUnitTest++
LDFLAGS += -L$(top_srcdir)/external/UnitTest++

CFLAGS += $(CBUILDFLAG) $(CWARNING) $(OS_FLAGS)
CFLAGS += -I. -I$(top_srcdir)/build
CFLAGS += $(DEFS) $(CADDS)

CXXFLAGS += $(CBUILDFLAG) $(CWARNING) $(OS_FLAGS)
CXXFLAGS += -I. -I$(top_srcdir)/build
CXXFLAGS += $(DEFS) $(CXXADDS)


#----------------------------------------------------------------------
# Path settings
prefix        ?= /usr/local
bindir        ?= $(prefix)/bin
libdir  	    ?= $(prefix)/lib
pkglibdir	    ?= $(libdir)/herschel
includedir    ?= $(prefix)/include
pkgincludedir ?= $(prefix)/include/herschel
datadir       ?= $(prefix)/share
pkgdatadir    ?= $(prefix)/share/herschel
mandir        ?= $(prefix)/man

BUILDDIR = $(top_srcdir)/temp
PKGDIR = $(top_srcdir)/packages


distdir = $(BUILDDIR)/$(PACKAGE)-$(VERSION)
info-distdir = $(PACKAGE)-docs-info-$(VERSION)
pdf-distdir = $(PACKAGE)-docs-pdf-$(VERSION)

pkg-name = $(PACKAGE)-$(VERSION)
src-pkg-name = $(PACKAGE)-src-$(VERSION)
pdf-doc-pkg-name = $(PACKAGE)-doc-pdf-$(VERSION)
info-doc-pkg-name = $(PACKAGE)-doc-info-$(VERSION)


