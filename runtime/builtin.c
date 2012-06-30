/* -*-c-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include <stdarg.h>
#include <string.h>
#include <stdio.h>

#include "runtime/herschel.h"


int __h7_lang_equal(int one, int two)
{
  return one == two;
}


int __h7_lang_add(int one, int two)
{
  return one + two;
}
