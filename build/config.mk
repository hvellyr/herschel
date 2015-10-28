#  This file is part of the herschel package
#
#  Copyright (c) 2010-2011 Gregor Klinke
#  All rights reserved.
#
#  This source code is released under the BSD License.

CDEBUG ?= -g
COPT ?= -O3
CWARNING += -Wall

CC ?= cc
CXX ?= c++
MAKE ?= make
AR ?= ar
RANLIB ?= ranlib
SHELL ?= /bin/sh
DYLD ?= gcc

# where is llvm-config installed?
LLVM_EXE ?= $(top_srcdir)/external/llvm/bin
LLVM_CPPFLAGS ?= $(shell $(LLVM_EXE)/llvm-config --cppflags backend bitwriter)
LLVM_LDFLAGS ?= $(shell $(LLVM_EXE)/llvm-config --ldflags backend bitwriter)
LLVM_LIBS ?= $(shell $(LLVM_EXE)/llvm-config --libs backend bitwriter)

# python to use.  at least 2.5
PYTHON ?= python

ZIP ?= zip
TAR ?= tar

# mercurial version to use
HG ?= hg

INSTALL     = /usr/bin/install -c
INSTALLDATA = /usr/bin/install -c -m 644


LIBEXT = .a
OBJEXT = .o
APPEXT =

PIC =

ifeq ($(TARGET_OS),linux)
PIC = -fpic -dynamic
endif

ifeq ($(TARGET_OS),mac)
DYLD = $(CC) -dynamiclib
PIC = -fPIC -dynamic
endif

# =================== Windows paths and settings
ifeq ($(TARGET_OS),win)
APPEXT = .exe

PIC = -fPIC -dynamic

MAKENSIS ?= c:/programme/nsis/makensis.exe

ifeq ($(TARGET_OSFLV),mingw32)
CADDS = -mno-cygwin
LDADDS = -mno-cygwin
endif

endif
