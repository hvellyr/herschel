/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include "common.hpp"

#include "port.hpp"

#include <list>
#include <map>


namespace herschel {

using StringList = std::list<String>;
using StringStringMap = std::map<String, String>;


namespace xml {
  void displayOpenTag(Port<Octet>& port, zstring tagName, bool newline = true);
  void displayOpenTagAttrs(Port<Octet>& port, zstring tagName, zstring attrs,
                           bool newline = true);
  void displayCloseTag(Port<Octet>& port, zstring tagName);
  void displayEmptyTag(Port<Octet>& port, zstring tagName);
  void displayEmptyTagAttrs(Port<Octet>& port, zstring tagName, zstring attrs);
  void displayTag(Port<Octet>& port, zstring tagName, const String& value);
  void displayTagAttr(Port<Octet>& port, zstring tagName, zstring attrs,
                      const String& value);
  void displayStringList(Port<Octet>& port, zstring outerTagName, zstring tagName,
                         const StringList& strlist);
  void displayStringStringMap(Port<Octet>& port, zstring outerTagName, zstring tagName,
                              zstring firstPairTagName, zstring secPairTagName,
                              const StringStringMap& strMap);
}  // namespace xml

}  // namespace herschel
