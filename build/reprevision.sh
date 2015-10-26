#!/bin/sh
#
#  This file is part of the herschel package
#
#  Copyright (c) 2010-2011 Gregor Klinke
#  All rights reserved.
#
#  This source code is released under the BSD License.

if [ -z "$GIT" ]; then
  GIT=git
fi

expr "$($GIT log -1 --format="%H")"
