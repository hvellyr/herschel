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


namespace herschel {

class ApplyNode;
class AstNode;
class ArrayNode;
class ArrayTypeNode;
class AssignNode;
class BinaryNode;
class BlockNode;
class BoolNode;
class CastNode;
class CharNode;
class CompileUnitNode;
class DefNode;
class DictNode;
class FuncDefNode;
class FunctionNode;
class IfNode;
class IntNode;
class KeyargNode;
class KeywordNode;
class LetNode;
class MatchNode;
class UnaryNode;
class OnNode;
class ParamNode;
class RangeNode;
class RationalNode;
class RealNode;
class SelectNode;
class SlotdefNode;
class SlotRefNode;
class StringNode;
class SymbolNode;
class TypeDefNode;
class UndefNode;
class UnitConstNode;
class VardefNode;
class VectorNode;
class WhileNode;


using StringList = std::list<String>;
using StringStringMap = std::map<String, String>;


//! Visitor for xml output
class XmlRenderer {
public:
  XmlRenderer(std::shared_ptr<Port<Octet>> port, bool showNodeType = false);

  void render(const AstNode& node);

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
  void displayNode(zstring tagName, AstNode* node);
  void displayNodeList(zstring tagName, const NodeList& nodelist);
  void displayType(zstring tagName, const Type& type);
  void displayTypeVector(zstring tagName, const TypeVector& types);

  std::shared_ptr<Port<Octet>> fPort;
  bool fShowNodeType;
  std::map<String, Type> fReferencedTypes;
};


namespace xml {
  void displayType(Port<Octet>& port, zstring tagName, const Type& type);
  void displayTypeVector(Port<Octet>& port, zstring tagName, const TypeVector& types);

  String displayTypeConv(const AstNode& node);

  void dump(const AstNode& node);
}  // namespace xml

}  // namespace herschel
