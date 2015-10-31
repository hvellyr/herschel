/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include <stdio.h>

#include "common.h"
#include "str.h"

#if defined(IS_DEBUG)

extern "C"
void gdb_ps(herschel::String str);


void gdb_ps(herschel::String str)
{
  fprintf(stderr, "%s\n", (zstring)herschel::StrHelper(str));
}
#endif

