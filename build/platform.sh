#!/bin/sh
#
#  This file is part of the herschel package
#
#  Copyright (c) 2010-2011 Gregor Klinke
#  All rights reserved.
#
#  This source code is released under the BSD License.

UNAME_ARCH=`(uname -m) 2>/dev/null` || UNAME_ARCH=unknown
UNAME_OS=`(uname -s) 2>/dev/null` || UNAME_OS=unknown
UNAME_RELEASE=`(uname -r) 2>/dev/null` || UNAME_RELEASE=unknown

case "${UNAME_ARCH}" in
  BePC)
    ARCH=i386
    ;;
  "Power Macintosh")
    ARCH=ppc
    ;;
  i386|i486|i586|i686)
    ARCH=i386
    ;;
  x86_64)
    ARCH=x86_64
    ;;
  *)
    ARCH=unknown
esac

FLAVOUR=plain
case "${UNAME_OS}" in
  Darwin)
    OS=mac
    ;;
  Haiku)
    OS=haiku
    ;;
  OpenBSD)
    OS=openbsd
    ;;
  Linux|linux)
    OS=linux
    ;;
  MINGW*)
    OS=win
    FLAVOUR=mingw32
    ;;
  CYGWIN)
    OS=win
    FLAVOUR=cygwin
    ;;
  *)
    OS=unknown
esac

RELEASE=${UNAME_RELEASE}


if [ "$1" = "-a" ]; then
  echo ${ARCH}
fi
if [ "$1" = "-m" ]; then
  echo ${OS}
fi
if [ "$1" = "-r" ]; then
  echo ${RELEASE}
fi
if [ "$1" = "-f" ]; then
  echo ${FLAVOUR}
fi

if [ "$1" = "-e" ]; then
  echo "ARCH=${ARCH} OS=${OS} FLAVOUR=${FLAVOUR} OSVERSION=${RELEASE}"
fi


if [ $# = 0 ]; then
  echo ${ARCH}-${OS}-${FLAVOUR}-${RELEASE}
fi
