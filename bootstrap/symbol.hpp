/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include <vector>

namespace herschel {
class String;
class Token;

/*! Produces a qualified, root name from the namespace part @p ns and
 * a @p name.  If @p name is a rooted name (i.e. starts with a ".", @p
 * ns is ignored and the result is @p name.  If @p ns is not rooted
 * itself, a leading "." is added. */
String qualifyId(const String& ns, const String& name);
bool isQualified(const String& sym);
String qualifyId(const std::vector<Token>& tokens);

/*! Removes a leading "." from @p str if existing. */
String deroot(const String& str);

String baseName(const String& sym);
String nsName(const String& sym);

String mangleToC(const String& qualId);


}  // namespace herschel
