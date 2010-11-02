/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_xmlout_h
#define bootstrap_xmlout_h

#include "refcountable.h"
#include "port.h"
#include "apt.h"

#include <list>
#include <map>


namespace heather
{
  class ApplyNode;
  class AptNode;
  class ArrayNode;
  class ArraySymbolNode;
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
  class NegateNode;
  class OnNode;
  class ParamNode;
  class RangeNode;
  class RationalNode;
  class RealNode;
  class SelectNode;
  class SlotdefNode;
  class StringNode;
  class SymbolNode;
  class ThenWhileNode;
  class TypeDefNode;
  class UnitConstNode;
  class VardefNode;
  class VectorNode;
  class WhileNode;

  //--------------------------------------------------------------------------

  typedef std::list<String> StringList;
  typedef std::map<String, String> StringStringMap;


  //--------------------------------------------------------------------------

  //! Visitor for xml output
  class XmlRenderer : public RefCountable
  {
  public:
    XmlRenderer(Port<Octet>* port, bool showNodeType = false);

    void render(AptNode* node);

    void renderNode(const ApplyNode* node);
    void renderNode(const ArrayNode* node);
    void renderNode(const ArraySymbolNode* node);
    void renderNode(const AssignNode* node);
    void renderNode(const BinaryNode* node);
    void renderNode(const BlockNode* node);
    void renderNode(const BoolNode* node);
    void renderNode(const CharNode* node);
    void renderNode(const CompileUnitNode* node);
    void renderNode(const DefNode* node);
    void renderNode(const DictNode* node);
    void renderNode(const FuncDefNode* node);
    void renderNode(const FunctionNode* node);
    void renderNode(const IfNode* node);
    void renderNode(const IntNode* node);
    void renderNode(const KeyargNode* node);
    void renderNode(const KeywordNode* node);
    void renderNode(const LetNode* node);
    void renderNode(const MatchNode* node);
    void renderNode(const NegateNode* node);
    void renderNode(const OnNode* node);
    void renderNode(const ParamNode* node);
    void renderNode(const RangeNode* node);
    void renderNode(const RationalNode* node);
    void renderNode(const RealNode* node);
    void renderNode(const SelectNode* node);
    void renderNode(const SlotdefNode* node);
    void renderNode(const StringNode* node);
    void renderNode(const SymbolNode* node);
    void renderNode(const ThenWhileNode* node);
    void renderNode(const TypeDefNode* node);
    void renderNode(const UnitConstNode* node);
    void renderNode(const VardefNode* node);
    void renderNode(const VectorNode* node);
    void renderNode(const WhileNode* node);
    void renderNode(const CastNode* node);

  private:
    void displayOpenTag(const char* tagName);
    void displayOpenTagAttrs(const char* tagName, const char* attrs);
    void displayCloseTag(const char* tagName);
    void displayEmptyTag(const char* tagName);
    void displayEmptyTagAttrs(const char* tagName, const char* attrs);
    void displayTag(const char* tagName, const String& value);
    void displayTagAttr(const char* tagName,
                        const char* attrs,
                        const String& value);
    void displayStringList(const char* outerTagName, const char* tagName,
                           const StringList& strlist);
    void displayStringStringMap(const char* outerTagName,
                                const char* tagName,
                                const char* firstPairTagName,
                                const char* secPairTagName,
                                const StringStringMap& strMap);
    void displayNode(const char* tagName, AptNode* node);
    void displayNodeList(const char* tagName, const NodeList& nodelist);
    void displayType(const char* tagName, const Type& type);
    void displayTypeVector(const char* tagName, const TypeVector& types);

    //-------- data members

    Ptr<Port<Octet> > fPort;
    bool fShowNodeType;
    std::map<String, Type> fReferencedTypes;
  };
};                              // namespace

#endif                          // bootstrap_xmlout_h
