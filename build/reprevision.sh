#!/bin/sh
#
#  This file is part of the heather package
#
#  Copyright (c) 2010 Gregor Klinke
#  All rights reserved.
#

if [ -z "$HG" ]; then
  HG=hg
fi

# $HG id --debug | sed -e 's/\([a-zA-Z0-9]*\).*/\1/'
expr "$($HG id --debug)" : '\([0-9a-zA-Z]*\)'