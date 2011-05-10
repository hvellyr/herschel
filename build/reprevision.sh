#!/bin/sh
#
#  This file is part of the herschel package
#
#  Copyright (c) 2010-2011 Gregor Klinke
#  All rights reserved.
#
#  This source code is released under the BSD License.

if [ -z "$HG" ]; then
  HG=hg
fi

# $HG id --debug | sed -e 's/\([a-zA-Z0-9]*\).*/\1/'
expr "$($HG id --debug)" : '\([0-9a-zA-Z]*\)'