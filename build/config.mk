#  This file is part of the herschel package
#
#  Copyright (c) 2010-2011 Gregor Klinke
#  All rights reserved.
#
#  This source code is released under the BSD License.

CDEBUG ?= -g
COPT ?= -O3
CWARNING += -Wall

CC ?= gcc
CXX ?= g++
MAKE ?= make
AR ?= ar
RANLIB ?= ranlib
SHELL ?= /bin/sh
DYLD ?= gcc

PYTHON ?= python2.5

ZIP ?= zip
TAR ?= tar

HG ?= hg

INSTALL     = /usr/bin/install -c
INSTALLDATA = /usr/bin/install -c -m 644


DYLIBEXT = .so
LYOEXT = .lo
LIBEXT = .a
OBJEXT = .o
APPEXT =

PIC =

ifeq ($(TARGET_OS),linux)
PIC = -fpic -dynamic
endif

ifeq ($(TARGET_OS),openbsd)
MAKE = gmake
PIC = -fpic -dynamic
endif

ifeq ($(TARGET_OS),mac)
DYLD = $(CC) -dynamiclib
DYLIBEXT = .dylib
PIC = -fPIC -dynamic
endif

LLVM_EXE = $(top_srcdir)/external/llvm/Release/bin

# do we have a readline installed?


# Path settings

prefix      ?= /usr/local


# =================== Windows paths and settings
ifeq ($(TARGET_OS),win)
APPEXT = .exe

PIC = -fPIC -dynamic

MAKENSIS ?= c:/programme/nsis/makensis.exe

ifeq ($(TARGET_OSFLV),mingw32)
CADDS = -mno-cygwin
LDADDS = -mno-cygwin

# setting for UnitTest++
MSYSTEM = MINGW32
endif

endif
