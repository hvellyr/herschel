/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include "common.h"
#include "str.h"
#include "symbol.h"


//----------------------------------------------------------------------------

using namespace heather;


String
heather::qualifyId(const String& ns, const String& name)
{
  if (isQualified(name) || ns.isEmpty())
    return name;

  return ns + "|" + name;
}


bool
heather::isQualified(const String& sym)
{
  return (sym.lastIndexOf('|') >= 0);
}


String
heather::baseName(const String& sym)
{
  int idx = sym.lastIndexOf('|');
  if (idx >= 0)
    return sym.part(idx + 1, sym.length());
  return sym;
}


String
heather::nsName(const String& sym)
{
  int idx = sym.lastIndexOf('|');
  if (idx >= 0)
    return sym.part(0, idx);
  return String();
}


