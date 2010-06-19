#!/bin/sh
#
#  This file is part of the heather package 
#
#  Copyright (c) 2010 Gregor Klinke
#  All rights reserved.
#

REP_ID=`hg id | awk '{print $1}'` || REP_ID=unknown

echo "$REP_ID"