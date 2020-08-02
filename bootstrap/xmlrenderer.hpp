/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include "common.hpp"

#include "ast.hpp"
#include "port.hpp"

#include <list>
#include <map>
#include <memory>


namespace herschel {

class AstNode;

using StringList = std::list<String>;
using StringStringMap = std::map<String, String>;


//! Visitor for xml output
class XmlRenderer {
public:
  XmlRenderer(std::shared_ptr<Port<Octet>> port, bool showNodeType = false);

  void render(std::shared_ptr<AstNode> node);

  void displayOpenTag(zstring tagName, bool newline = true);
  void displayOpenTagAttrs(zstring tagName, zstring attrs, bool newline = true);
  void displayCloseTag(zstring tagName);
  void displayEmptyTag(zstring tagName);
  void displayEmptyTagAttrs(zstring tagName, zstring attrs);
  void displayTag(zstring tagName, const String& value);
  void displayTagAttr(zstring tagName, zstring attrs, const String& value);
  void displayStringList(zstring outerTagName, zstring tagName,
                         const StringList& strlist);
  void displayStringStringMap(zstring outerTagName, zstring tagName,
                              zstring firstPairTagName, zstring secPairTagName,
                              const StringStringMap& strMap);
  void displayNode(zstring tagName, std::shared_ptr<AstNode> node);
  void displayNodeList(zstring tagName, const NodeList& nodelist);
  void displayType(zstring tagName, const Type& type, bool shortForm = true);
  void displayTypeVector(zstring tagName, const TypeVector& types, bool shortForm = true);

  std::shared_ptr<Port<Octet>> fPort;
  bool fShowNodeType;
  std::map<String, Type> fReferencedTypes;
};


namespace xml {
  void displayType(Port<Octet>& port, zstring tagName, const Type& type, bool shortForm = true);
  void displayTypeVector(Port<Octet>& port, zstring tagName, const TypeVector& types, bool shortForm = true);

  String displayTypeConv(const AstNode& node);

  void dump(std::shared_ptr<AstNode> node);
}  // namespace xml

}  // namespace herschel
