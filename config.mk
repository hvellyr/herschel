#  This file is part of the herschel package
#
#  Copyright (c) 2010-2012 Gregor Klinke
#  All rights reserved.
#
#  This source code is released under the BSD License.

# ----------------------------------------------------------------------

PACKAGE = herschel
VERSION = 0.1
COPYRIGHTYEAR = 2002, 2003, 2009-2012
COPYRIGHTOWNER = Gregor C. Klinke
# human readable package name, used for Mac packages, etc.
HR_PACKAGE = Herschel

LANG_VERSION = 0.3.0

# ----------------------------------------------------------------------
# plattfrom settings
# OS = mac | win | linux |
# ARCH = i386 | x86_64 | ppc

TARGET_OS    ?= $(shell $(SHELL) $(top_srcdir)/build/platform.sh -m)
TARGET_ARCH  ?= $(shell $(SHELL) $(top_srcdir)/build/platform.sh -a)
TARGET_OSVER ?= $(shell $(SHELL) $(top_srcdir)/build/platform.sh -r)

OS_FLAGS ?= -DOS_$(TARGET_OS) -DARCH_$(TARGET_ARCH)

BASE_REVISION ?= $(shell $(SHELL) $(top_srcdir)/build/reprevision.sh)


# ----------------------------------------------------------------------
# Features:

#CDEBUG += -g
#COPT += -O0
#CWARNING += -Winline
#CXXADDS =
#LDADDS


# include automatic settings (if exist)
ifeq ($(curdir),)
-include $(top_srcdir)/auto-config-local.mk
else
-include $(curdir)/auto-config-local.mk
endif
# include manual user settings (if exist)
-include $(top_srcdir)/config-local.mk
# include system fallback settings
include $(top_srcdir)/build/config.mk

# define the build style.  Possible values
# debug = debug build (incl. debug symbols, assertions, ext. traces, etc.)
# release = release build with optimizations, incl. traces, unit tests
# dist-release = release build with optimizations, no traces, etc.
BUILDSTYLE ?= debug

UNITTESTS ?= -DUNITTESTS
TRACES ?= -DTRACES

ifeq ($(BUILDSTYLE),debug)
CBUILDFLAG = $(CDEBUG) -DIS_DEBUG $(UNITTESTS) $(TRACES)
endif
ifeq ($(BUILDSTYLE),release)
CBUILDFLAG = $(COPT) -DIS_RELEASE $(UNITTESTS) $(TRACES)
endif

# use dist-release to make a build for a distribution.  This version won't
# include unit test code and a number of options.
ifeq ($(BUILDSTYLE),dist-release)
CBUILDFLAG = $(COPT) -DIS_RELEASE
endif


# ----------------------------------------------------------------------
# prepare the settings for the compiler

LIBS += -lUnitTest++
LDFLAGS += -L$(BUILDDIR)/$(BUILDSTYLE)/UnitTest++

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

ifeq ($(curdir),)
BUILDDIR = $(top_srcdir)/temp
PKGDIR = $(top_srcdir)/packages
BUILDTESTSDIR = $(top_srcdir)/temp/tests
else
BUILDDIR = $(curdir)/temp
PKGDIR = $(curdir)/packages
BUILDTESTSDIR = $(curdir)/temp/tests
CFLAGS += -I$(curdir)
CXXFLAGS += -I$(curdir)
endif

distdir = $(BUILDDIR)/$(PACKAGE)-$(VERSION)
info-distdir = $(PACKAGE)-docs-info-$(VERSION)
pdf-distdir = $(PACKAGE)-docs-pdf-$(VERSION)

pkg-name = $(PACKAGE)-$(VERSION)
src-pkg-name = $(PACKAGE)-src-$(VERSION)
pdf-doc-pkg-name = $(PACKAGE)-doc-pdf-$(VERSION)
info-doc-pkg-name = $(PACKAGE)-doc-info-$(VERSION)


