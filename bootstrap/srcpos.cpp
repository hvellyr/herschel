/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "common.h"
#include "srcpos.h"
#include "str.h"
#include "strbuf.h"


using namespace herschel;

String
SrcPos::toString() const
{
  return (StringBuffer(fFile) << ':' << fromInt(fLineNo)).toString();
}
