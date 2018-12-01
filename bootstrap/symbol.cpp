/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "symbol.hpp"
#include "common.hpp"
#include "predefined.hpp"
#include "str.hpp"
#include "strbuf.hpp"
#include "token.hpp"

#include "stdio.h"


namespace herschel {


String qualifyId(const String& ns, const String& name)
{
  if (!name.isEmpty() && name.indexOf('.', 0) == 0) {
    return name;
  }
  else if (ns.isEmpty()) {
    return String(".") + name;
  }

  return ns + "." + name;
}


bool isQualified(const String& sym)
{
  return sym.lastIndexOf('.') >= 0;
}


String baseName(const String& sym)
{
  int idx = sym.lastIndexOf('.');
  if (idx >= 0)
    return sym.part(idx + 1, sym.length());

  return sym;
}


String nsName(const String& sym)
{
  int idx = sym.lastIndexOf('.');
  if (idx >= 0)
    return sym.part(0, idx);

  return String();
}


static void fastMangleSymPart(StringBuffer& result, const String& sym)
{
  bool hasToEncode = false;

  int startPos = result.length();

  for (int i = 0; i < sym.length(); i++) {
    Char c = sym[i];
    if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')) {
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

//! pattern:
//!
//!   __QN..sym..sym..sym
//!
//! where .. is the length of the following sym in decimal digits.  Special
//! characters (other than a-zA-Z0-9 and _) are translated as /xy with xy
//! being hexdigits.
String mangleToC(const String& qualId)
{
  StringBuffer result;
  result << "__QN";

  String tmp = qualId;
  String ns;
  while (tmp.split('.', ns, tmp) >= 0) {
    fastMangleSymPart(result, ns);
  }
  if (!tmp.isEmpty()) {
    fastMangleSymPart(result, tmp);
  }

  return result.toString();
}


String qualifyId(const std::vector<Token>& tokens)
{
  StringBuffer result;

  for (const auto& t : tokens) {
    if (t == kDot)
      result << ".";
    else if (t == kSymbol)
      result << t.idValue();
  }

  return result.toString();
}


String deroot(const String& str)
{
  if (str.length() > 1 && str.indexOf('.', 0) == 0) {
    return str.part(1, str.length());
  }

  return str;
}

}  // namespace herschel
