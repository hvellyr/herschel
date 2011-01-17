#!/bin/sh
#
#  This file is part of the herschel package 
#
#  Copyright (c) 2010 Gregor Klinke
#  All rights reserved.
#

UNAME_ARCH=`(uname -m) 2>/dev/null` || UNAME_ARCH=unknown
UNAME_OS=`(uname -s) 2>/dev/null` || UNAME_OS=unknown
UNAME_RELEASE=`(uname -r) 2>/dev/null` || UNAME_RELEASE=unknown


case "${UNAME_ARCH}:${UNAME_OS}:${UNAME_RELEASE}" in
  BePC:Haiku:*)
    ARCH=i386
    OS=haiku
    FLAVOUR=plain
    RELEASE=${UNAME_RELEASE}
    ;;

  "Power Macintosh:Darwin:*")
    ARCH=ppc
    OS=mac
    FLAVOUR=plain
    RELEASE=${UNAME_RELEASE}
    ;;

  i386:Darwin:*)
    ARCH=i386
    OS=mac
    FLAVOUR=plain
    RELEASE=${UNAME_RELEASE}
    ;;

  i*86:OpenBSD:*)
    ARCH=i386
    OS=openbsd
    FLAVOUR=plain
    RELEASE=${UNAME_RELEASE}
    ;;

  *:Darwin:*)
    # guess this is ppc
    ARCH=ppc
    OS=mac
    FLAVOUR=plain
    RELEASE=${UNAME_RELEASE}
    ;;

  i*86:OpenBSD:*)
    ARCH=i386
    OS=openbsd
    FLAVOUR=plain
    RELEASE=${UNAME_RELEASE}
    ;;

  i*86:Linux:*)
    ARCH=i386
    OS=linux
    FLAVOUR=unknown
    RELEASE=${UNAME_RELEASE}
    ;;

  i*:MINGW*:*)
    ARCH=i386
    OS=win
    FLAVOUR=mingw32
    RELEASE=${UNAME_RELEASE}
    ;;

  i*:CYGWIN*:*)
    ARCH=i386
    OS=win
    FLAVOUR=cygwin
    RELEASE=${UNAME_RELEASE}
    ;;

  *)
    ARCH=unknown
    OS=unknown
    FLAVOUR=unknown
    RELEASE=unknown
esac

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
