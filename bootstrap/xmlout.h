/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include "port.h"
#include "apt.h"

#include <list>
#include <map>


namespace herschel
{
  class ApplyNode;
  class AptNode;
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

  //--------------------------------------------------------------------------

  using StringList = std::list<String>;
  using StringStringMap = std::map<String, String>;


  //--------------------------------------------------------------------------

  //! Visitor for xml output
  class XmlRenderer
  {
  public:
    XmlRenderer(Port<Octet>* port, bool showNodeType = false);

    void render(const AptNode& node);

    void renderNode(const ApplyNode& node);
    void renderNode(const ArrayNode& node);
    void renderNode(const ArrayTypeNode& node);
    void renderNode(const AssignNode& node);
    void renderNode(const BinaryNode& node);
    void renderNode(const BlockNode& node);
    void renderNode(const BoolNode& node);
    void renderNode(const CharNode& node);
    void renderNode(const CompileUnitNode& node);
    void renderNode(const DefNode& node);
    void renderNode(const DictNode& node);
    void renderNode(const FuncDefNode& node);
    void renderNode(const FunctionNode& node);
    void renderNode(const IfNode& node);
    void renderNode(const IntNode& node);
    void renderNode(const KeyargNode& node);
    void renderNode(const KeywordNode& node);
    void renderNode(const LetNode& node);
    void renderNode(const MatchNode& node);
    void renderNode(const UnaryNode& node);
    void renderNode(const OnNode& node);
    void renderNode(const ParamNode& node);
    void renderNode(const RangeNode& node);
    void renderNode(const RationalNode& node);
    void renderNode(const RealNode& node);
    void renderNode(const SelectNode& node);
    void renderNode(const SlotdefNode& node);
    void renderNode(const SlotRefNode& node);
    void renderNode(const StringNode& node);
    void renderNode(const SymbolNode& node);
    void renderNode(const TypeDefNode& node);
    void renderNode(const TypeNode& node);
    void renderNode(const UnitConstNode& node);
    void renderNode(const VardefNode& node);
    void renderNode(const VectorNode& node);
    void renderNode(const WhileNode& node);
    void renderNode(const CastNode& node);
    void renderNode(const UndefNode& node);

  private:
    void displayOpenTag(zstring tagName, bool newline = true);
    void displayOpenTagAttrs(zstring tagName, zstring attrs,
                             bool newline = true);
    void displayCloseTag(zstring tagName);
    void displayEmptyTag(zstring tagName);
    void displayEmptyTagAttrs(zstring tagName, zstring attrs);
    void displayTag(zstring tagName, const String& value);
    void displayTagAttr(zstring tagName,
                        zstring attrs,
                        const String& value);
    void displayStringList(zstring outerTagName, zstring tagName,
                           const StringList& strlist);
    void displayStringStringMap(zstring outerTagName,
                                zstring tagName,
                                zstring firstPairTagName,
                                zstring secPairTagName,
                                const StringStringMap& strMap);
    void displayNode(zstring tagName, AptNode* node);
    void displayNodeList(zstring tagName, const NodeList& nodelist);
    void displayType(zstring tagName, const Type& type);
    void displayTypeVector(zstring tagName, const TypeVector& types);

    //-------- data members

    Ptr<Port<Octet> > fPort;
    bool fShowNodeType;
    std::map<String, Type> fReferencedTypes;
  };


  namespace xml
  {
    void displayOpenTag(Port<Octet>* port, zstring tagName,
                        bool newline = true);
    void displayOpenTagAttrs(Port<Octet>* port,
                             zstring tagName, zstring attrs,
                             bool newline = true);
    void displayCloseTag(Port<Octet>* port, zstring tagName);
    void displayEmptyTag(Port<Octet>* port, zstring tagName);
    void displayEmptyTagAttrs(Port<Octet>* port, zstring tagName,
                              zstring attrs);
    void displayTag(Port<Octet>* port, zstring tagName, const String& value);
    void displayTagAttr(Port<Octet>* port, zstring tagName,
                        zstring attrs,
                        const String& value);
    void displayStringList(Port<Octet>* port,
                           zstring outerTagName, zstring tagName,
                           const StringList& strlist);
    void displayStringStringMap(Port<Octet>* port,
                                zstring outerTagName,
                                zstring tagName,
                                zstring firstPairTagName,
                                zstring secPairTagName,
                                const StringStringMap& strMap);
    void displayType(Port<Octet>* port, zstring tagName, const Type& type);
    void displayTypeVector(Port<Octet>* port,
                           zstring tagName, const TypeVector& types);

    String displayTypeConv(const AptNode& node);

    void dump(const AptNode& node);

  };

} // namespace
