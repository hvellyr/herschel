#  This file is part of the heather package
#
#  Copyright (c) 2010 Gregor Klinke
#  All rights reserved.
#

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
