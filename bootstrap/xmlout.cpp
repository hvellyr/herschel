/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include "common.h"

#include "xmlout.h"
#include "port.h"
#include "apt.h"
#include "strbuf.h"


//----------------------------------------------------------------------------

using namespace heather;


//----------------------------------------------------------------------------

XmlRenderer::XmlRenderer(Port<Octet>* port)
  : fPort(port)
{
  display(fPort, "<?xml version='1.0' encoding='utf-8'?>\n");
}


void
XmlRenderer::render(AptNode* node)
{
  node->render(this);
}


//----------------------------------------------------------------------------

void
XmlRenderer::displayOpenTag(const char* tagName)
{
  if (tagName != NULL)
    heather::display(fPort, String() + "<" + tagName + ">");
}


void
XmlRenderer::displayOpenTagAttrs(const char* tagName, const char* attrs)
{
  if (tagName != NULL) {
    heather::display(fPort, String() + "<" + tagName);
    if (attrs != NULL)
      heather::display(fPort, String() + " " + attrs + ">");
    else
      heather::display(fPort, ">");
  }
}


void
XmlRenderer::displayCloseTag(const char* tagName)
{
  if (tagName != NULL)
    heather::display(fPort, String() + "</" + tagName + ">");
}


void
XmlRenderer::displayEmptyTag(const char* tagName)
{
  if (tagName != NULL && ::strlen(tagName) > 0)
    heather::display(fPort, String() + "<" + tagName + "/>");
}


void
XmlRenderer::displayTag(const char* tagName, const String& value)
{
  displayOpenTag(tagName);
  heather::display(fPort, xmlEncode(value));
  displayCloseTag(tagName);
}


void
XmlRenderer::displayTagAttr(const char* tagName,
                            const char* attrs,
                            const String& value)
{
  displayOpenTagAttrs(tagName, attrs);
  heather::display(fPort, xmlEncode(value));
  displayCloseTag(tagName);
}


void
XmlRenderer::displayStringList(const char* outerTagName, const char* tagName,
                               const StringList& strlist)
{
  if (!strlist.empty())
    displayOpenTag(outerTagName);

  for (StringList::const_iterator it = strlist.begin();
       it != strlist.end();
       it++)
  {
    String str = (*it);
    displayOpenTag(tagName);
    heather::display(fPort, str);
    displayCloseTag(tagName);
  }

  if (!strlist.empty())
    displayCloseTag(outerTagName);
}


void
XmlRenderer::displayStringStringMap(const char* outerTagName,
                                    const char* tagName,
                                    const char* firstPairTagName,
                                    const char* secPairTagName,
                                    const StringStringMap& strMap)
{
  if (!strMap.empty())
    displayOpenTag(outerTagName);

  for (StringStringMap::const_iterator it = strMap.begin();
       it != strMap.end();
       it++)
  {
    displayOpenTag(tagName);
    displayTag(firstPairTagName, it->first);
    displayTag(secPairTagName, it->second);
    displayCloseTag(tagName);
  }

  if (!strMap.empty())
    displayCloseTag(outerTagName);
}


void
XmlRenderer::displayNode(const char* tagName, AptNode* node)
{
  if (node != NULL) {
    displayOpenTag(tagName);
    render(node);
    displayCloseTag(tagName);
  }
}


void
XmlRenderer::displayNodeList(const char* tagName, const NodeList& nodelist)
{
  if (!nodelist.empty()) {
    displayOpenTag(tagName);

    for (NodeList::const_iterator it = nodelist.begin();
         it != nodelist.end();
         it++)
    {
      AptNode* n = (*it);
      if (n != NULL)
        render(n);
    }

    displayCloseTag(tagName);
  }
}


void
XmlRenderer::displayType(const char* tagName, const Type& type)
{
  if (type.isDef()) {
    const char* attrs = "xmlns:ty='http://heather.eyestep.org/types'";
    displayOpenTagAttrs(tagName, attrs);
    heather::display(fPort, type.toString());
    displayCloseTag(tagName);
  }
}


void
XmlRenderer::displayTypeVector(const char* tagName, const TypeVector& types)
{
  if (!types.empty()) {
    const char* attrs = "xmlns:ty='http://heather.eyestep.org/types'";
    displayOpenTagAttrs(tagName, attrs);
    for (size_t i = 0; i < types.size(); i++)
      heather::display(fPort, types[i].toString());
    displayCloseTag(tagName);
  }
}


//----------------------------------------------------------------------------

void
XmlRenderer::renderNode(const KeywordNode* node)
{
  displayTag("keyw", node->fValue);
}


void
XmlRenderer::renderNode(const StringNode* node)
{
  displayTag("str", node->fValue);
}


void
XmlRenderer::renderNode(const SymbolNode* node)
{
  if (node->fGenerics.empty())
    displayTag("symbol", node->fValue);
  else {
    StringBuffer attrs;
    attrs << "nm='" << node->fValue << "'";

    displayOpenTagAttrs("symbol", StrHelper(attrs.toString()));
    displayTypeVector("gen", node->fGenerics);
    displayCloseTag("symbol");
  }
}


void
XmlRenderer::renderNode(const ArraySymbolNode* node)
{
  assert(node->fGenerics.empty());

  displayTagAttr("symbol", "array='t'", node->fValue);
}


void
XmlRenderer::renderNode(const IntNode* node)
{
  StringBuffer attrs;
  attrs << "ty='" << node->fType.typeName() << "'";
  if (node->fIsImaginary)
    attrs << " imag='t'";

  displayTagAttr("int", StrHelper(attrs.toString()),
                 String() + node->fValue);
}


void
XmlRenderer::renderNode(const RealNode* node)
{
  StringBuffer attrs;
  attrs << "ty='" << node->fType.typeName() << "'";
  if (node->fIsImaginary)
    attrs << " imag='t'";

  displayTagAttr("real", StrHelper(attrs.toString()),
                 String() + node->fValue);
}


void
XmlRenderer::renderNode(const RationalNode* node)
{
  StringBuffer attrs;
  attrs << "ty='" << node->fType.typeName() << "'";
  if (node->fIsImaginary)
    attrs << " imag='t'";

  String val = String() + node->fValue.numerator() + "/" + node->fValue.denominator();
  displayTagAttr("real", StrHelper(attrs.toString()), val);
}


void
XmlRenderer::renderNode(const CharNode* node)
{
  displayTag("char", fromInt(int(node->fValue)));
}


void
XmlRenderer::renderNode(const BoolNode* node)
{
  displayEmptyTag(node->fValue ? "true" : "false");
}


void
XmlRenderer::renderNode(const UnitConstNode* node)
{
  StringBuffer attrs;
  attrs << "unit='" << node->fUnit.name() << "'";

  displayOpenTagAttrs("uvalue", StrHelper(attrs.toString()));
  displayType("type", node->fUnit.effType());
  displayNode(NULL, node->fValue);
  displayCloseTag("uvalue");
}


void
XmlRenderer::renderNode(const CompileUnitNode* node)
{
  displayNodeList("compile-unit", node->children());
}


void
XmlRenderer::renderNode(const LetNode* node)
{
  displayOpenTag("let");
  displayNode(NULL, node->fDefined);
  displayCloseTag("let");
}


void
XmlRenderer::renderNode(const DefNode* node)
{
  displayOpenTag("def");
  displayNode(NULL, node->fDefined);
  displayCloseTag("def");
}




void
XmlRenderer::renderNode(const VardefNode* node)
{
  StringBuffer attrs;

  attrs << "sym='" << node->fSymbolName << "'";

  switch (node->fFlags) {
  case kNormalVar:
    break;
  case kFluidVar:
    attrs << " type='fluid'";
    break;
  case kConstVar:
    attrs << " type='const'";
    break;
  case kConfigVar:
    attrs << " type='config'";
    break;
  case kEnumVar:
    attrs << " type='enum'";
    break;
  }

  displayOpenTagAttrs("vardef", StrHelper(attrs.toString()));

  displayType("type", node->fType);
  displayNode("init", node->fInitExpr);

  displayCloseTag("vardef");
}


void
XmlRenderer::renderNode(const ParamNode* node)
{
  StringBuffer attrs;

  attrs << "sym='" << node->fSymbolName << "'";

  switch (node->fFlags) {
  case kPosArg:
    attrs << " type='pos'";
    break;
  case kSpecArg:
    attrs << " type='spec'";
    break;
  case kNamedArg:
    attrs << " type='key' key='" << node->fKey << "'";
    break;
  case kRestArg:
    attrs << " type='rest'";
    break;
  }

  displayOpenTagAttrs("param", StrHelper(attrs.toString()));

  displayType("type", node->fType);
  displayNode("init", node->fInitExpr);

  displayCloseTag("param");
}


void
XmlRenderer::renderNode(const SlotdefNode* node)
{
  StringBuffer attrs;

  attrs << "sym='" << node->fSymbolName << "'";

  if ((node->fFlags & kTransientSlot) != 0) {
    attrs << " transient='t'";
  }
  if ((node->fFlags & kReadonlySlot) != 0) {
    attrs << " readonly='t'";
  }
  if ((node->fFlags & kObservableSlot) != 0) {
    attrs << " observable='t'";
  }
  if ((node->fFlags & kAutoSlot) != 0) {
    attrs << " auto='t'";
  }

  if ((node->fFlags & kPublicSlot) != 0) {
    attrs << " viz='public'";
  }
  else if ((node->fFlags & kOuterSlot) != 0) {
    attrs << " viz='outer'";
  }
  else if ((node->fFlags & kInnerSlot) != 0) {
    attrs << " viz='inner'";
  }

  displayOpenTagAttrs("slot", StrHelper(attrs.toString()));
  displayType("type", node->fType);
  displayNode("init", node->fInitExpr);
  displayCloseTag("slot");
}


void
XmlRenderer::renderNode(const ArrayNode* node)
{
  displayOpenTag("array");
  displayNodeList(NULL, node->children());
  displayCloseTag("array");
}


void
XmlRenderer::renderNode(const VectorNode* node)
{
  displayOpenTag("vector");
  displayNodeList(NULL, node->children());
  displayCloseTag("vector");
}


void
XmlRenderer::renderNode(const DictNode* node)
{
  displayOpenTag("dict");
  displayNodeList(NULL, node->children());
  displayCloseTag("dict");
}


static const char*
operatorName(OperatorType type)
{
  switch (type) {
  case kOpAppend:       return "++";
  case kOpAs:           return "as";
  case kOpAssign:       return "=";
  case kOpBitAnd:       return "AND";
  case kOpBitOr:        return "OR";
  case kOpBitXor:       return "XOR";
  case kOpBy:           return "by";
  case kOpCompare:      return "<=>";
  case kOpDivide:       return "/";
  case kOpEqual:        return "==";
  case kOpExponent:     return "**";
  case kOpFold:         return "%";
  case kOpGreater:      return ">";
  case kOpGreaterEqual: return ">=";
  case kOpIn:           return "in";
  case kOpIsa:          return "isa";
  case kOpLess:         return "<";
  case kOpLessEqual:    return "<=";
  case kOpLogicalAnd:   return "and";
  case kOpLogicalOr:    return "or";
  case kOpMapTo:        return "->";
  case kOpMinus:        return "-";
  case kOpMod:          return "mod";
  case kOpMultiply:     return "*";
  case kOpPlus:         return "+";
  case kOpRange:        return "..";
  case kOpShiftLeft:    return "<<";
  case kOpShiftRight:   return ">>";
  case kOpUnequal:      return "<>";
  case kOpThen:         return "then";
  case kOpWhile:        return "while";

  case kOpInvalid:
    assert(0);
  }

  return NULL;
}


void
XmlRenderer::renderNode(const BinaryNode* node)
{
  StringBuffer attrs;
  attrs << "op='" << xmlEncode(operatorName(node->fOp)) << "'";
  displayOpenTagAttrs("binary", StrHelper(attrs.toString()));
  displayNode(NULL, node->fLeft);
  displayNode(NULL, node->fRight);
  displayCloseTag("binary");
}


void
XmlRenderer::renderNode(const NegateNode* node)
{
  displayOpenTag("neg");
  displayNode(NULL, node->fBase);
  displayCloseTag("neg");
}


void
XmlRenderer::renderNode(const RangeNode* node)
{
  displayOpenTag("range");
  displayNode(NULL, node->from());
  displayNode(NULL, node->to());
  displayNode(NULL, node->by());
  displayCloseTag("range");
}


void
XmlRenderer::renderNode(const ThenWhileNode* node)
{
  displayOpenTag("then-while");
  displayNode(NULL, node->fFirst);
  displayNode(NULL, node->fStep);
  displayNode(NULL, node->fTest);
  displayCloseTag("then-while");
}


void
XmlRenderer::renderNode(const AssignNode* node)
{
  displayOpenTag("assign");
  displayNode(NULL, node->fLValue);
  displayNode(NULL, node->fRValue);
  displayCloseTag("assign");
}


void
XmlRenderer::renderNode(const IfNode* node)
{
  displayOpenTag("if");
  displayNode("test", node->fTest);
  displayNode("then", node->fConsequent);
  displayNode("else", node->fAlternate);
  displayCloseTag("if");
}


void
XmlRenderer::renderNode(const SelectNode* node)
{
  displayOpenTag("select");
  displayNode("test", node->fTest);
  displayNode("comp", node->fComparator);
  for (size_t i = 0; i < node->fMappings.size(); i++) {
    if (node->fMappings[i].fTestValues.empty()) {
      displayNode("alternate", node->fMappings[i].fConsequent);
    }
    else {
      displayOpenTag("map");
      displayOpenTag("values");
      for (size_t j = 0; j < node->fMappings[i].fTestValues.size(); j++) {
        displayNode(NULL, node->fMappings[i].fTestValues[j]);
      }
      displayCloseTag("values");
      displayNode("cons", node->fMappings[i].fConsequent);
      displayCloseTag("map");
    }
  }
  displayCloseTag("select");
}


void
XmlRenderer::renderNode(const MatchNode* node)
{
  displayOpenTag("match");
  displayNode("test", node->fExpr);
  for (size_t i = 0; i < node->fMappings.size(); i++) {
    StringBuffer attrs;
    if (!node->fMappings[i].fVarName.isEmpty())
      attrs << "nm='" << xmlEncode(node->fMappings[i].fVarName) << "'";
    displayOpenTagAttrs("map", StrHelper(attrs.toString()));
    displayType("type", node->fMappings[i].fMatchType);
    displayNode("cons", node->fMappings[i].fConsequent);
    displayCloseTag("map");
  }
  displayCloseTag("match");
}


void
XmlRenderer::renderNode(const OnNode* node)
{
  StringBuffer attrs;
  attrs << "key='" << node->fKey << "'";
  displayOpenTagAttrs("on", StrHelper(attrs.toString()));
  displayNodeList("params", node->fParams);
  displayNode("body", node->fBody);
  displayCloseTag("on");
}


void
XmlRenderer::renderNode(const BlockNode* node)
{
  displayNodeList("block", node->children());
}


void
XmlRenderer::renderNode(const FunctionNode* node)
{
  displayOpenTag("function");
  displayNodeList("params", node->fParams);
  displayType("rettype", node->fRetType);
  displayNode("body", node->fBody);
  displayCloseTag("function");
}


void
XmlRenderer::renderNode(const FuncDefNode* node)
{
  const char* tag = node->isGeneric() ? "method" : "func";

  StringBuffer attrs;
  attrs << "sym='" << node->fSym << "'";

  if (node->isAbstract())
    attrs << " abstract='true'";

  displayOpenTagAttrs(tag, StrHelper(attrs.toString()));
  displayNodeList("params", node->fParams);
  displayType("rettype", node->fRetType);
  displayNode("body", node->fBody);
  displayCloseTag(tag);
}


void
XmlRenderer::renderNode(const ApplyNode* node)
{
  displayOpenTag("apply");
  displayNode(NULL, node->fBase);
  displayOpenTag("args");
  displayNodeList(NULL, node->children());
  displayCloseTag("args");
  displayCloseTag("apply");
}


void
XmlRenderer::renderNode(const KeyargNode* node)
{
  StringBuffer attrs;
  attrs << "key='" << node->fKey << "'";

  displayOpenTagAttrs("arg", StrHelper(attrs.toString()));
  displayNode(NULL, node->fValue);
  displayCloseTag("arg");
}


void
XmlRenderer::renderNode(const WhileNode* node)
{
  displayOpenTag("while");
  displayNode("test", node->fTest);
  displayNode("body", node->fBody);
  displayCloseTag("while");
}


void
XmlRenderer::renderNode(const TypeDefNode* node)
{
  const char* tagName = node->fIsClass ? "class" : "type";

  StringBuffer attrs;
  attrs << "nm='" << node->fTypeName << "'";

  displayOpenTagAttrs(tagName, StrHelper(attrs.toString()));
  displayNodeList("params", node->fParams);
  displayNodeList("slots", node->fSlots);
  displayNodeList("on", node->fOnExprs);
  displayNodeList("proto", node->fReqProtocol);
  displayType("isa", node->fIsa);
  displayCloseTag(tagName);
}


void
XmlRenderer::renderNode(const CastNode* node)
{
  displayOpenTag("cast");
  displayNode("base", node->base());
  displayType("as", node->type());
  displayCloseTag("cast");
}
