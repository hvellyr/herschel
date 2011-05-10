#!/bin/sh
#
#  This file is part of the herschel package 
#
#  Copyright (c) 2010-2011 Gregor Klinke
#  All rights reserved.
#
#  This source code is released under the BSD License.

[ -z "$CC" ] && CC=gcc >/dev/null

CC_VERSION=`($CC --version) 2>/dev/null` || CC_VERSION=unknown

CC_MAJOR=$(printf "%s" "$CC_VERSION" | sed -n -e 's/.*(GCC)[ \t]*\([234]\)\.\([0-9]*\).*/\1/p')
CC_MINOR=$(printf "%s" "$CC_VERSION" | sed -n -e 's/.*(GCC)[ \t]*\([234]\)\.\([0-9]*\).*/\2/p')

[ ! -z "$CC_MAJOR" ] && { echo "gcc$CC_MAJOR.$CC_MINOR"; exit 0; }

# ok we didn't got the version from the version info.  Let's try to run gcc.

tmpfile=tmp_0000.c
cat >$tmpfile <<EOF
#include <stdio.h>
int main()
{
#ifdef __GNUC__
  char *p=__VERSION__;
  if (p[0] >= '2' && p[0] <= '4' &&
      p[1] != '\0' && p[2] != '\0' &&
      p[2] >= '0' && p[2] <= '9')
    printf("gcc%c.%c\n", p[0], p[2]);
  else
    printf("gcc_unknown\n");
#else
  printf("unknown\n");
#endif
  return 0;
}
EOF
$CC $tmpfile >/dev/null 2>&1
if test -f ./a.out; then
  ./a.out
  /bin/rm -f a.out $tmpfile
elif test -f a.exe; then
  ./a.exe
  /bin/rm -f $tmpfile a.exe
else
  /bin/rm -f $tmpfile
  echo "run_unknown"
fi
