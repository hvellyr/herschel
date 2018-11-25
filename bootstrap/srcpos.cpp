/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "srcpos.hpp"

#include "str.hpp"
#include "strbuf.hpp"


using namespace herschel;

String SrcPos::toString() const
{
  return (StringBuffer(fFile) << ':' << fromInt(fLineNo)).toString();
}
