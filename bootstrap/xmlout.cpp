/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.
*/

#include "common.h"

#include "xmlout.h"
#include "port.h"
#include "apt.h"
#include "strbuf.h"

#include "string.h"
#include <typeinfo>

//----------------------------------------------------------------------------

using namespace herschel;


//----------------------------------------------------------------------------

void
herschel::xml::displayOpenTag(Port<Octet>* port,
                             const char* tagName, bool newline)
{
  if (tagName != NULL) {
    herschel::display(port, String() + "<" + tagName + ">");
    if (newline)
      herschel::display(port, String() + "\n");
  }
}


void
herschel::xml::displayOpenTagAttrs(Port<Octet>* port,
                                  const char* tagName, const char* attrs,
                                  bool newline)
{
  if (tagName != NULL) {
    herschel::display(port, String() + "<" + tagName);
    if (attrs != NULL && ::strlen(attrs) > 0)
      herschel::display(port, String() + " " + attrs + ">");
    else
      herschel::display(port, ">");
    if (newline)
      herschel::display(port, String() + "\n");
  }
}


void
herschel::xml::displayCloseTag(Port<Octet>* port,
                              const char* tagName)
{
  if (tagName != NULL)
    herschel::display(port, String() + "</" + tagName + ">\n");
}


void
herschel::xml::displayEmptyTag(Port<Octet>* port,
                              const char* tagName)
{
  if (tagName != NULL && ::strlen(tagName) > 0)
    herschel::display(port, String() + "<" + tagName + "/>\n");
}


void
herschel::xml::displayEmptyTagAttrs(Port<Octet>* port,
                                   const char* tagName, const char* attrs)
{
  if (tagName != NULL && ::strlen(tagName) > 0)
    herschel::display(port, String() + "<" + tagName + " " + attrs + "/>\n");
}


void
herschel::xml::displayTag(Port<Octet>* port,
                         const char* tagName, const String& value)
{
  displayOpenTag(port, tagName, false);
  herschel::display(port, xmlEncode(value));
  displayCloseTag(port, tagName);
}


void
herschel::xml::displayTagAttr(Port<Octet>* port,
                             const char* tagName,
                             const char* attrs,
                             const String& value)
{
  String encodedValue = xmlEncode(value);
  if (!encodedValue.isEmpty()) {
    displayOpenTagAttrs(port, tagName, attrs, false);
    herschel::display(port, encodedValue);
    displayCloseTag(port, tagName);
  }
  else
    displayEmptyTagAttrs(port, tagName, attrs);
}


void
herschel::xml::displayStringList(Port<Octet>* port,
                                const char* outerTagName, const char* tagName,
                                const StringList& strlist)
{
  if (!strlist.empty())
    displayOpenTag(port, outerTagName);

  for (StringList::const_iterator it = strlist.begin();
       it != strlist.end();
       it++)
  {
    String str = (*it);
    displayOpenTag(port, tagName, false);
    herschel::display(port, str);
    displayCloseTag(port, tagName);
  }

  if (!strlist.empty())
    displayCloseTag(port, outerTagName);
}


void
herschel::xml::displayStringStringMap(Port<Octet>* port,
                                     const char* outerTagName,
                                     const char* tagName,
                                     const char* firstPairTagName,
                                     const char* secPairTagName,
                                     const StringStringMap& strMap)
{
  if (!strMap.empty())
    displayOpenTag(port, outerTagName);

  for (StringStringMap::const_iterator it = strMap.begin();
       it != strMap.end();
       it++)
  {
    displayOpenTag(port, tagName);
    displayTag(port, firstPairTagName, it->first);
    displayTag(port, secPairTagName, it->second);
    displayCloseTag(port, tagName);
  }

  if (!strMap.empty())
    displayCloseTag(port, outerTagName);
}


void
herschel::xml::displayType(Port<Octet>* port, const char* tagName, const Type& type)
{
  if (type.isDef()) {
    displayOpenTag(port, tagName);
    herschel::display(port, type.toString());
    displayCloseTag(port, tagName);
  }
}


void
herschel::xml::displayTypeVector(Port<Octet>* port, const char* tagName, const TypeVector& types)
{
  if (!types.empty()) {
    displayOpenTag(port, tagName);
    for (size_t i = 0; i < types.size(); i++)
      herschel::display(port, types[i].toString());
    displayCloseTag(port, tagName);
  }
}


//----------------------------------------------------------------------------

XmlRenderer::XmlRenderer(Port<Octet>* port, bool showNodeType)
  : fPort(port),
    fShowNodeType(showNodeType)
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
XmlRenderer::displayOpenTag(const char* tagName, bool newline)
{
  xml::displayOpenTag(fPort, tagName, newline);
}


void
XmlRenderer::displayOpenTagAttrs(const char* tagName, const char* attrs,
                                 bool newline)
{
  xml::displayOpenTagAttrs(fPort, tagName, attrs, newline);
}


void
XmlRenderer::displayCloseTag(const char* tagName)
{
  xml::displayCloseTag(fPort, tagName);
}


void
XmlRenderer::displayEmptyTag(const char* tagName)
{
  xml::displayEmptyTag(fPort, tagName);
}


void
XmlRenderer::displayEmptyTagAttrs(const char* tagName, const char* attrs)
{
  xml::displayEmptyTagAttrs(fPort, tagName, attrs);
}


void
XmlRenderer::displayTag(const char* tagName, const String& value)
{
  xml::displayTag(fPort, tagName, value);
}


void
XmlRenderer::displayTagAttr(const char* tagName,
                            const char* attrs,
                            const String& value)
{
  xml::displayTagAttr(fPort, tagName, attrs, value);
}


void
XmlRenderer::displayStringList(const char* outerTagName, const char* tagName,
                               const StringList& strlist)
{
  xml::displayStringList(fPort, outerTagName, tagName, strlist);
}


void
XmlRenderer::displayStringStringMap(const char* outerTagName,
                                    const char* tagName,
                                    const char* firstPairTagName,
                                    const char* secPairTagName,
                                    const StringStringMap& strMap)
{
  xml::displayStringStringMap(fPort, outerTagName,
                              tagName, firstPairTagName, secPairTagName,
                              strMap);
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
  xml::displayType(fPort, tagName, type);
}


void
XmlRenderer::displayTypeVector(const char* tagName, const TypeVector& types)
{
  xml::displayTypeVector(fPort, tagName, types);
}


//----------------------------------------------------------------------------

void
XmlRenderer::renderNode(const KeywordNode* node)
{
  displayTag("keyw", node->value());
}


void
XmlRenderer::renderNode(const StringNode* node)
{
  displayTag("str", node->value());
}


void
XmlRenderer::renderNode(const SymbolNode* node)
{
  StringBuffer attrs;
  if (fShowNodeType && node->type().isDef())
    attrs << " ty='" << xmlEncode(node->type().typeId()) << "'";

  if (node->generics().empty()) {
    if (node->isShared())
      attrs << " acc='shared'";
    displayTagAttr("symbol", StrHelper(attrs.toString()), node->name());
  }
  else {
    attrs << " nm='" << node->name() << "'";
    if (node->isShared())
      attrs << " acc='shared'";

    displayOpenTagAttrs("symbol", StrHelper(attrs.toString()));
    displayTypeVector("gen", node->generics());
    displayCloseTag("symbol");
  }

  if (fShowNodeType && node->type().isDef())
    fReferencedTypes.insert(std::make_pair(node->type().typeId(),
                                           node->type()));
}


void
XmlRenderer::renderNode(const ArrayTypeNode* node)
{
  const AptNode* rootType = node->typeNode();

  if (const SymbolNode* sym = dynamic_cast<const SymbolNode*>(rootType)) {
    StringBuffer attrs;

    if (fShowNodeType && node->type().isDef()) {
      attrs << " ty='" << xmlEncode(node->type().typeId()) << "'";
      fReferencedTypes.insert(std::make_pair(node->type().typeId(),
                                             node->type()));
    }

    attrs << " array='t'";
    displayTagAttr("symbol", StrHelper(attrs.toString()), sym->name());
  }
  else if (const TypeNode* ty = dynamic_cast<const TypeNode*>(rootType)) {
    StringBuffer attrs;

    attrs << " array='t'";
    attrs << " ty='" << xmlEncode(ty->type().typeId()) << "'";
    displayTagAttr("type", StrHelper(attrs.toString()), String());
    fReferencedTypes.insert(std::make_pair(node->type().typeId(),
                                           node->type()));
  }
  else if (rootType) {
    fprintf(stderr, "Unexpected type node: %p %s\n", rootType, typeid(*rootType).name());
    assert(0 && "unexpected type node");
  }
}


void
XmlRenderer::renderNode(const TypeNode* node)
{
  StringBuffer attrs;
  if (node->type().isDef()) {
    attrs << " ty='" << xmlEncode(node->type().typeId()) << "'";
    fReferencedTypes.insert(std::make_pair(node->type().typeId(),
                                           node->type()));
  }

  displayTagAttr("type", StrHelper(attrs.toString()), String());
}


void
XmlRenderer::renderNode(const IntNode* node)
{
  StringBuffer attrs;
  attrs << "ty='" << xmlEncode(node->type().typeId()) << "'";

  if (node->isImaginary())
    attrs << " imag='t'";

  displayOpenTagAttrs("int", StrHelper(attrs.toString()), false);
  herschel::display(fPort, xmlEncode(String() + node->value()));
  displayCloseTag("int");

  if (fShowNodeType)
    fReferencedTypes.insert(std::make_pair(node->type().typeId(),
                                           node->type()));
}


void
XmlRenderer::renderNode(const RealNode* node)
{
  StringBuffer attrs;
  attrs << "ty='" << xmlEncode(node->type().typeId()) << "'";
  if (node->isImaginary())
    attrs << " imag='t'";

  displayTagAttr("real", StrHelper(attrs.toString()),
                 String() + node->value());

  if (fShowNodeType)
    fReferencedTypes.insert(std::make_pair(node->type().typeId(),
                                           node->type()));
}


void
XmlRenderer::renderNode(const RationalNode* node)
{
  StringBuffer attrs;
  attrs << "ty='" << xmlEncode(node->type().typeId()) << "'";
  if (node->isImaginary())
    attrs << " imag='t'";

  String val = String() + node->value().numerator() + "/" + node->value().denominator();
  displayTagAttr("real", StrHelper(attrs.toString()), val);

  if (fShowNodeType)
    fReferencedTypes.insert(std::make_pair(node->type().typeId(),
                                           node->type()));
}


void
XmlRenderer::renderNode(const CharNode* node)
{
  displayTag("char", fromInt(int(node->value())));
}


void
XmlRenderer::renderNode(const BoolNode* node)
{
  if (fShowNodeType) {
    StringBuffer attrs;
    attrs << " ty='" << xmlEncode(node->type().typeId()) << "'";
    displayEmptyTagAttrs(node->value() ? "true" : "false",
                         StrHelper(attrs.toString()));
  }
  else
    displayEmptyTag(node->value() ? "true" : "false");

  if (fShowNodeType)
    fReferencedTypes.insert(std::make_pair(node->type().typeId(),
                                           node->type()));
}


void
XmlRenderer::renderNode(const UnitConstNode* node)
{
  StringBuffer attrs;
  attrs << "unit='" << node->unit().name() << "'";

  displayOpenTagAttrs("uvalue", StrHelper(attrs.toString()));
  displayType("type", node->unit().effType());
  displayNode(NULL, node->value());
  displayCloseTag("uvalue");
}


void
XmlRenderer::renderNode(const CompileUnitNode* node)
{
  const char* attrs = "xmlns:ty='http://herschel.eyestep.org/types'";
  displayOpenTagAttrs("compile-unit", attrs);
  displayNodeList(NULL, node->children());

  if (fShowNodeType && !fReferencedTypes.empty()) {
    displayOpenTag("ty:node-types");
    for (std::map<String, Type>::iterator it = fReferencedTypes.begin(),
         e = fReferencedTypes.end();
         it != e;
         ++it)
    {
      displayType("ty:used-type", it->second);
    }
    displayCloseTag("ty:node-types");
  }

  displayCloseTag("compile-unit");
}


void
XmlRenderer::renderNode(const LetNode* node)
{
  displayOpenTag("let");
  displayNode(NULL, node->defNode());
  displayCloseTag("let");
}


void
XmlRenderer::renderNode(const DefNode* node)
{
  displayOpenTag("def");
  displayNode(NULL, node->defNode());
  displayCloseTag("def");
}




void
XmlRenderer::renderNode(const VardefNode* node)
{
  StringBuffer attrs;

  attrs << "sym='" << node->name() << "'";

  switch (node->flags()) {
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

  if (!node->linkage().isEmpty())
    attrs << " linkage='" << node->linkage() << "'";

  switch (node->allocType()) {
  case kAlloc_Local:
    break;
  case kAlloc_Shared:
    attrs << " alloc='shared'";
    break;
  }

  displayOpenTagAttrs("vardef", StrHelper(attrs.toString()));

  displayType("type", node->type());
  displayNode("init", node->initExpr());

  displayCloseTag("vardef");


}


void
XmlRenderer::renderNode(const ParamNode* node)
{
  StringBuffer attrs;

  attrs << "sym='" << node->name() << "'";

  switch (node->flags()) {
  case kPosArg:
    attrs << " type='pos'";
    break;
  case kSpecArg:
    attrs << " type='spec'";
    break;
  case kNamedArg:
    attrs << " type='key' key='" << node->key() << "'";
    break;
  case kRestArg:
    attrs << " type='rest'";
    break;
  }

  switch (node->allocType()) {
  case kAlloc_Local:
    break;
  case kAlloc_Shared:
    attrs << " alloc='shared'";
    break;
  }

  displayOpenTagAttrs("param", StrHelper(attrs.toString()));

  displayType("type", node->type());
  displayNode("init", node->initExpr());

  displayCloseTag("param");
}


void
XmlRenderer::renderNode(const SlotdefNode* node)
{
  StringBuffer attrs;

  attrs << "sym='" << node->name() << "'";

  if ((node->flags() & kTransientSlot) != 0) {
    attrs << " transient='t'";
  }
  if ((node->flags() & kReadonlySlot) != 0) {
    attrs << " readonly='t'";
  }
  if ((node->flags() & kObservableSlot) != 0) {
    attrs << " observable='t'";
  }
  if ((node->flags() & kAutoSlot) != 0) {
    attrs << " auto='t'";
  }

  if ((node->flags() & kPublicSlot) != 0) {
    attrs << " viz='public'";
  }
  else if ((node->flags() & kOuterSlot) != 0) {
    attrs << " viz='outer'";
  }
  else if ((node->flags() & kInnerSlot) != 0) {
    attrs << " viz='inner'";
  }

  switch (node->allocType()) {
  case kAlloc_Local:
    break;
  case kAlloc_Shared:
    attrs << " alloc='shared'";
    break;
  }

  displayOpenTagAttrs("slot", StrHelper(attrs.toString()));
  displayType("type", node->type());
  displayNode("init", node->initExpr());
  displayCloseTag("slot");
}


void
XmlRenderer::renderNode(const ArrayNode* node)
{
  StringBuffer attrs;

  if (fShowNodeType && node->type().isDef()) {
    attrs << " ty='" << xmlEncode(node->type().typeId()) << "'";
    fReferencedTypes.insert(std::make_pair(node->type().typeId(),
                                           node->type()));
  }

  displayOpenTagAttrs("array", StrHelper(attrs.toString()));
  displayNodeList(NULL, node->children());
  displayCloseTag("array");
}


void
XmlRenderer::renderNode(const VectorNode* node)
{
  StringBuffer attrs;

  if (fShowNodeType && node->type().isDef()) {
    attrs << " ty='" << xmlEncode(node->type().typeId()) << "'";
    fReferencedTypes.insert(std::make_pair(node->type().typeId(),
                                           node->type()));
  }

  displayOpenTagAttrs("vector", StrHelper(attrs.toString()));
  displayNodeList(NULL, node->children());
  displayCloseTag("vector");
}


void
XmlRenderer::renderNode(const DictNode* node)
{
  StringBuffer attrs;

  if (fShowNodeType && node->type().isDef()) {
    attrs << " ty='" << xmlEncode(node->type().typeId()) << "'";
    fReferencedTypes.insert(std::make_pair(node->type().typeId(),
                                           node->type()));
  }

  displayOpenTagAttrs("dict", StrHelper(attrs.toString()));
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
  case kOpRemove:       return "--";
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
  attrs << "op='" << xmlEncode(operatorName(node->op())) << "'";

  if (fShowNodeType && node->type().isDef()) {
    attrs << " ty='" << xmlEncode(node->type().typeId()) << "'";
    fReferencedTypes.insert(std::make_pair(node->type().typeId(),
                                           node->type()));
  }

  displayOpenTagAttrs("binary", StrHelper(attrs.toString()));
  displayNode(NULL, node->left());
  displayNode(NULL, node->right());
  displayCloseTag("binary");
}


void
XmlRenderer::renderNode(const NegateNode* node)
{
  StringBuffer attrs;
  if (fShowNodeType && node->type().isDef()) {
    attrs << " ty='" << xmlEncode(node->type().typeId()) << "'";
    fReferencedTypes.insert(std::make_pair(node->type().typeId(),
                                           node->type()));
  }

  displayOpenTagAttrs("neg", StrHelper(attrs.toString()));
  displayNode(NULL, node->base());
  displayCloseTag("neg");
}


void
XmlRenderer::renderNode(const RangeNode* node)
{
  StringBuffer attrs;
  if (fShowNodeType && node->type().isDef()) {
    attrs << " ty='" << xmlEncode(node->type().typeId()) << "'";
    fReferencedTypes.insert(std::make_pair(node->type().typeId(),
                                           node->type()));
  }

  displayOpenTagAttrs("range", StrHelper(attrs.toString()));
  displayNode(NULL, node->from());
  displayNode(NULL, node->to());
  displayNode(NULL, node->by());
  displayCloseTag("range");
}


void
XmlRenderer::renderNode(const AssignNode* node)
{
  StringBuffer attrs;
  if (fShowNodeType && node->type().isDef()) {
    attrs << " ty='" << xmlEncode(node->type().typeId()) << "'";
    fReferencedTypes.insert(std::make_pair(node->type().typeId(),
                                           node->type()));
  }

  displayOpenTagAttrs("assign", StrHelper(attrs.toString()));
  displayNode(NULL, node->lvalue());
  displayNode(NULL, node->rvalue());
  displayCloseTag("assign");
}


void
XmlRenderer::renderNode(const IfNode* node)
{
  StringBuffer attrs;
  if (fShowNodeType && node->type().isDef()) {
    attrs << " ty='" << xmlEncode(node->type().typeId()) << "'";
    fReferencedTypes.insert(std::make_pair(node->type().typeId(),
                                           node->type()));
  }

  displayOpenTagAttrs("if", StrHelper(attrs.toString()));
  displayNode("test", node->test());
  displayNode("then", node->consequent());
  displayNode("else", node->alternate());
  displayCloseTag("if");
}


void
XmlRenderer::renderNode(const SelectNode* node)
{
  displayOpenTag("select");
  displayNode("test", node->test());
  displayNode("comp", node->comparator());
  for (size_t i = 0; i < node->mappingCount(); i++) {
    if (node->mappingAt(i).fTestValues.empty()) {
      displayNode("alternate", node->mappingAt(i).fConsequent);
    }
    else {
      displayOpenTag("map");
      displayOpenTag("values");
      for (size_t j = 0; j < node->mappingAt(i).fTestValues.size(); j++) {
        displayNode(NULL, node->mappingAt(i).fTestValues[j]);
      }
      displayCloseTag("values");
      displayNode("cons", node->mappingAt(i).fConsequent);
      displayCloseTag("map");
    }
  }
  displayCloseTag("select");
}


void
XmlRenderer::renderNode(const MatchNode* node)
{
  displayOpenTag("match");
  displayNode("test", node->expr());
  for (size_t i = 0; i < node->mappingCount(); i++) {
    StringBuffer attrs;
    if (!node->mappingAt(i).fVarName.isEmpty())
      attrs << "nm='" << xmlEncode(node->mappingAt(i).fVarName) << "'";
    displayOpenTagAttrs("map", StrHelper(attrs.toString()));
    displayType("type", node->mappingAt(i).fMatchType);
    displayNode("cons", node->mappingAt(i).fConsequent);
    displayCloseTag("map");
  }
  displayCloseTag("match");
}


void
XmlRenderer::renderNode(const OnNode* node)
{
  StringBuffer attrs;
  attrs << "key='" << node->key() << "'";
  displayOpenTagAttrs("on", StrHelper(attrs.toString()));
  displayNodeList("params", node->params());
  displayNode("body", node->body());
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
  displayNodeList("params", node->params());
  displayType("rettype", node->retType());
  displayNode("body", node->body());
  displayCloseTag("function");
}


void
XmlRenderer::renderNode(const FuncDefNode* node)
{
  const char* tag = node->isGeneric() ? "method" : "func";

  StringBuffer attrs;
  attrs << "sym='" << node->name() << "'";

  if (node->isAbstract())
    attrs << " abstract='true'";

  if (!node->linkage().isEmpty())
    attrs << " linkage='" << node->linkage() << "'";

  displayOpenTagAttrs(tag, StrHelper(attrs.toString()));
  displayNodeList("params", node->params());
  displayType("rettype", node->retType());
  displayNode("body", node->body());
  displayCloseTag(tag);
}


void
XmlRenderer::renderNode(const ApplyNode* node)
{
  StringBuffer attrs;

  if (fShowNodeType && node->type().isDef())
    attrs << " ty='" << xmlEncode(node->type().typeId()) << "'";

  displayOpenTagAttrs("apply", StrHelper(attrs.toString()));
  displayNode(NULL, node->base());
  displayOpenTag("args");
  displayNodeList(NULL, node->children());
  displayCloseTag("args");
  displayCloseTag("apply");

  if (fShowNodeType && node->type().isDef())
    fReferencedTypes.insert(std::make_pair(node->type().typeId(),
                                           node->type()));
}


void
XmlRenderer::renderNode(const KeyargNode* node)
{
  StringBuffer attrs;
  attrs << "key='" << node->key() << "'";

  displayOpenTagAttrs("arg", StrHelper(attrs.toString()));
  displayNode(NULL, node->value());
  displayCloseTag("arg");
}


void
XmlRenderer::renderNode(const WhileNode* node)
{
  StringBuffer attrs;

  if (fShowNodeType && node->type().isDef()) {
    attrs << " ty='" << xmlEncode(node->type().typeId()) << "'";
    fReferencedTypes.insert(std::make_pair(node->type().typeId(),
                                           node->type()));
  }

  displayOpenTagAttrs("while", StrHelper(attrs.toString()));
  displayNode("test", node->test());
  displayNode("body", node->body());
  displayCloseTag("while");
}


void
XmlRenderer::renderNode(const TypeDefNode* node)
{
  const char* tagName = node->isClass() ? "class" : "type";

  StringBuffer attrs;
  attrs << "nm='" << node->name() << "'";

  displayOpenTagAttrs(tagName, StrHelper(attrs.toString()));
  displayNodeList("params", node->params());
  displayNodeList("slots", node->slots());
  displayNodeList("on", node->onExprs());
  displayNodeList("proto", node->reqProtocol());
  displayType("isa", node->defType());
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
