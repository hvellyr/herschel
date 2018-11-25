/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "xmlout.hpp"

#include "port.hpp"
#include "strbuf.hpp"

#include <cstring>
#include <typeinfo>


namespace herschel {


void xml::displayOpenTag(Port<Octet>& port, zstring tagName, bool newline)
{
  if (tagName) {
    display(port, String() + "<" + tagName + ">");
    if (newline)
      display(port, String() + "\n");
  }
}


void xml::displayOpenTagAttrs(Port<Octet>& port, zstring tagName, zstring attrs,
                              bool newline)
{
  if (tagName) {
    display(port, String() + "<" + tagName);
    if (attrs && ::strlen(attrs) > 0)
      display(port, String() + " " + attrs + ">");
    else
      display(port, ">");
    if (newline)
      display(port, String() + "\n");
  }
}


void xml::displayCloseTag(Port<Octet>& port, zstring tagName)
{
  if (tagName)
    display(port, String() + "</" + tagName + ">\n");
}


void xml::displayEmptyTag(Port<Octet>& port, zstring tagName)
{
  if (tagName && ::strlen(tagName) > 0)
    display(port, String() + "<" + tagName + "/>\n");
}


void xml::displayEmptyTagAttrs(Port<Octet>& port, zstring tagName, zstring attrs)
{
  if (tagName && ::strlen(tagName) > 0)
    display(port, String() + "<" + tagName + " " + attrs + "/>\n");
}


void xml::displayTag(Port<Octet>& port, zstring tagName, const String& value)
{
  displayOpenTag(port, tagName, !K(newline));
  display(port, xmlEncode(value));
  displayCloseTag(port, tagName);
}


void xml::displayTagAttr(Port<Octet>& port, zstring tagName, zstring attrs,
                         const String& value)
{
  String encodedValue = xmlEncode(value);
  if (!encodedValue.isEmpty()) {
    displayOpenTagAttrs(port, tagName, attrs, !K(newline));
    display(port, encodedValue);
    displayCloseTag(port, tagName);
  }
  else
    displayEmptyTagAttrs(port, tagName, attrs);
}


void xml::displayStringList(Port<Octet>& port, zstring outerTagName, zstring tagName,
                            const StringList& strlist)
{
  if (!strlist.empty())
    displayOpenTag(port, outerTagName);

  for (StringList::const_iterator it = strlist.begin(); it != strlist.end(); it++) {
    String str = (*it);
    displayOpenTag(port, tagName, !K(newline));
    display(port, str);
    displayCloseTag(port, tagName);
  }

  if (!strlist.empty())
    displayCloseTag(port, outerTagName);
}


void xml::displayStringStringMap(Port<Octet>& port, zstring outerTagName, zstring tagName,
                                 zstring firstPairTagName, zstring secPairTagName,
                                 const StringStringMap& strMap)
{
  if (!strMap.empty())
    displayOpenTag(port, outerTagName);

  for (StringStringMap::const_iterator it = strMap.begin(); it != strMap.end(); it++) {
    displayOpenTag(port, tagName);
    displayTag(port, firstPairTagName, it->first);
    displayTag(port, secPairTagName, it->second);
    displayCloseTag(port, tagName);
  }

  if (!strMap.empty())
    displayCloseTag(port, outerTagName);
}

}  // namespace herschel
