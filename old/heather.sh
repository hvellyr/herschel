#!/bin/sh
export ARC_HOME=`pwd`/scheme
export ARC_INIT_DIR=`pwd`/scheme
cd scheme; ./arc -s ../heather.scm --dir `pwd`/../ $*