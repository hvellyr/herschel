/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.
*/

#include "common.h"
#include "str.h"
#include "symbol.h"
#include "strbuf.h"

#include "stdio.h"


//----------------------------------------------------------------------------

using namespace herschel;


String
herschel::qualifyId(const String& ns, const String& name)
{
  if (isQualified(name) || ns.isEmpty())
    return name;

  return ns + "|" + name;
}


bool
herschel::isQualified(const String& sym)
{
  return (sym.lastIndexOf('|') >= 0);
}


String
herschel::baseName(const String& sym)
{
  int idx = sym.lastIndexOf('|');
  if (idx >= 0)
    return sym.part(idx + 1, sym.length());
  return sym;
}


String
herschel::nsName(const String& sym)
{
  int idx = sym.lastIndexOf('|');
  if (idx >= 0)
    return sym.part(0, idx);
  return String();
}


namespace herschel {
static void
fastMangleSymPart(StringBuffer& result, const String& sym)
{
  bool hasToEncode = false;

  int startPos = result.length();

  for (int i = 0; i < sym.length(); i++) {
    Char c = sym[i];
    if ( (c >= 'a' && c <= 'z') ||
         (c >= 'A' && c <= 'Z') ||
         (c >= '0' && c <= '9') ) {
      if (hasToEncode)
        result << c;
    }
    else {
      if (!hasToEncode) {
        hasToEncode = true;
        for (int j = 0; j < i; j++)
          result << sym[j];
      }

      hr_assert(c > 0 && c < 256);
      char tmp[16];
      sprintf(tmp, "/%02x", c);
      result << tmp;
    }
  }

  if (!hasToEncode) 
    result << sym;

  int symLength = result.length() - startPos;
  hr_assert(symLength >= sym.length());

  char tmp[32];
  sprintf(tmp, "%d", symLength);

  result.insertAt(startPos, tmp);
}
}

//! pattern:
//!
//!   __QN..sym..sym..sym
//!
//! where .. is the length of the following sym in decimal digits.  Special
//! characters (other than a-zA-Z0-9 and _) are translated as /2two-hexdigit.
String
herschel::mangleToC(const String& qualId)
{
  StringBuffer result;
  result << "__QN";

  String tmp = qualId;
  String ns;
  while (tmp.split('|', ns, tmp) >= 0) {
    fastMangleSymPart(result, ns);
  }
  if (!tmp.isEmpty()) {
    fastMangleSymPart(result, tmp);
  }

  return result.toString();
}


#if defined(UNITTESTS)
//----------------------------------------------------------------------------

#include <UnitTest++.h>
#include <iostream>

TEST(SymbolMangling)
{
  CHECK_EQUAL(String("__QN13hello/2dworld"), mangleToC(String("hello-world")));
  CHECK_EQUAL(String("__QN3app4main"), mangleToC(String("app|main")));
  CHECK_EQUAL(String("__QN4core2io12/2astdout/2a"), mangleToC(String("core|io|*stdout*")));
  CHECK_EQUAL(String("__QN9call/2fcc"), mangleToC(String("call/cc")));
}

#endif  // #if defined(UNITTESTS)

