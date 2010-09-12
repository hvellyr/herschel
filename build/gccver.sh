#!/bin/sh
#
#  This file is part of the heather package 
#
#  Copyright (c) 2010 Gregor Klinke
#  All rights reserved.
#

CC_VERSION=`($CC --version) 2>/dev/null` || CC_VERSION=unknown

CC_MAJOR=`echo $CC_VERSION | sed -n -e 's/.*(GCC)[ \t]*\([234]\)\.\([0-9]*\).*/\1/p'`
CC_MINOR=`echo $CC_VERSION | sed -n -e 's/.*(GCC)[ \t]*\([234]\)\.\([0-9]*\).*/\2/p'`

echo "gcc$CC_MAJOR.$CC_MINOR"