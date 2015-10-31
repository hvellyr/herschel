/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "common.h"

#include "xmlout.h"
#include "port.h"
#include "apt.h"
#include "strbuf.h"
#include "properties.h"

#include "string.h"
#include <typeinfo>

//----------------------------------------------------------------------------

using namespace herschel;


//----------------------------------------------------------------------------

void
herschel::xml::displayOpenTag(Port<Octet>* port,
                             zstring tagName, bool newline)
{
  if (tagName) {
    herschel::display(port, String() + "<" + tagName + ">");
    if (newline)
      herschel::display(port, String() + "\n");
  }
}


void
herschel::xml::displayOpenTagAttrs(Port<Octet>* port,
                                  zstring tagName, zstring attrs,
                                  bool newline)
{
  if (tagName) {
    herschel::display(port, String() + "<" + tagName);
    if (attrs && ::strlen(attrs) > 0)
      herschel::display(port, String() + " " + attrs + ">");
    else
      herschel::display(port, ">");
    if (newline)
      herschel::display(port, String() + "\n");
  }
}


void
herschel::xml::displayCloseTag(Port<Octet>* port,
                              zstring tagName)
{
  if (tagName)
    herschel::display(port, String() + "</" + tagName + ">\n");
}


void
herschel::xml::displayEmptyTag(Port<Octet>* port,
                              zstring tagName)
{
  if (tagName && ::strlen(tagName) > 0)
    herschel::display(port, String() + "<" + tagName + "/>\n");
}


void
herschel::xml::displayEmptyTagAttrs(Port<Octet>* port,
                                   zstring tagName, zstring attrs)
{
  if (tagName && ::strlen(tagName) > 0)
    herschel::display(port, String() + "<" + tagName + " " + attrs + "/>\n");
}


void
herschel::xml::displayTag(Port<Octet>* port,
                         zstring tagName, const String& value)
{
  displayOpenTag(port, tagName, !K(newline));
  herschel::display(port, xmlEncode(value));
  displayCloseTag(port, tagName);
}


void
herschel::xml::displayTagAttr(Port<Octet>* port,
                             zstring tagName,
                             zstring attrs,
                             const String& value)
{
  String encodedValue = xmlEncode(value);
  if (!encodedValue.isEmpty()) {
    displayOpenTagAttrs(port, tagName, attrs, !K(newline));
    herschel::display(port, encodedValue);
    displayCloseTag(port, tagName);
  }
  else
    displayEmptyTagAttrs(port, tagName, attrs);
}


void
herschel::xml::displayStringList(Port<Octet>* port,
                                zstring outerTagName, zstring tagName,
                                const StringList& strlist)
{
  if (!strlist.empty())
    displayOpenTag(port, outerTagName);

  for (StringList::const_iterator it = strlist.begin();
       it != strlist.end();
       it++)
  {
    String str = (*it);
    displayOpenTag(port, tagName, !K(newline));
    herschel::display(port, str);
    displayCloseTag(port, tagName);
  }

  if (!strlist.empty())
    displayCloseTag(port, outerTagName);
}


void
herschel::xml::displayStringStringMap(Port<Octet>* port,
                                     zstring outerTagName,
                                     zstring tagName,
                                     zstring firstPairTagName,
                                     zstring secPairTagName,
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
herschel::xml::displayType(Port<Octet>* port, zstring tagName, const Type& type)
{
  if (type.isDef()) {
    displayOpenTag(port, tagName);
    herschel::display(port, type.toString());
    displayCloseTag(port, tagName);
  }
}


void
herschel::xml::displayTypeVector(Port<Octet>* port, zstring tagName, const TypeVector& types)
{
  if (!types.empty()) {
    displayOpenTag(port, tagName);
    for (size_t i = 0; i < types.size(); i++)
      herschel::display(port, types[i].toString());
    displayCloseTag(port, tagName);
  }
}


//----------------------------------------------------------------------------

String
herschel::xml::displayTypeConv(const AptNode* node)
{
  StringBuffer attrs;

  switch (node->typeConv())
  {
  case kNoConv:         attrs << " conv='none'"; break;
  case kTypeCheckConv:  attrs << " conv='check'"; break;
  case kAtom2PlainConv: attrs << " conv='atom2plain'"; break;
  case kPlain2AtomConv: attrs << " conv='plain2atom'"; break;
  };

  if (node->type() != node->dstType())
    attrs << " dstty='" << node->dstType().typeId() << "'";

  return attrs.toString();
}


//------------------------------------------------------------------------------

void
herschel::xml::dump(const AptNode* node)
{
  Ptr<XmlRenderer> out = new XmlRenderer(new FilePort(stderr));
  out->render(node);
}


//----------------------------------------------------------------------------

XmlRenderer::XmlRenderer(Port<Octet>* port, bool showNodeType)
  : fPort(port),
    fShowNodeType(showNodeType)
{
  display(fPort, "<?xml version='1.0' encoding='utf-8'?>\n");
}


void
XmlRenderer::render(const AptNode* node)
{
  node->render(this);
}


//----------------------------------------------------------------------------

void
XmlRenderer::displayOpenTag(zstring tagName, bool newline)
{
  xml::displayOpenTag(fPort, tagName, newline);
}


void
XmlRenderer::displayOpenTagAttrs(zstring tagName, zstring attrs,
                                 bool newline)
{
  xml::displayOpenTagAttrs(fPort, tagName, attrs, newline);
}


void
XmlRenderer::displayCloseTag(zstring tagName)
{
  xml::displayCloseTag(fPort, tagName);
}


void
XmlRenderer::displayEmptyTag(zstring tagName)
{
  xml::displayEmptyTag(fPort, tagName);
}


void
XmlRenderer::displayEmptyTagAttrs(zstring tagName, zstring attrs)
{
  xml::displayEmptyTagAttrs(fPort, tagName, attrs);
}


void
XmlRenderer::displayTag(zstring tagName, const String& value)
{
  xml::displayTag(fPort, tagName, value);
}


void
XmlRenderer::displayTagAttr(zstring tagName, zstring attrs, const String& value)
{
  xml::displayTagAttr(fPort, tagName, attrs, value);
}


void
XmlRenderer::displayStringList(zstring outerTagName, zstring tagName,
                               const StringList& strlist)
{
  xml::displayStringList(fPort, outerTagName, tagName, strlist);
}


void
XmlRenderer::displayStringStringMap(zstring outerTagName,
                                    zstring tagName,
                                    zstring firstPairTagName,
                                    zstring secPairTagName,
                                    const StringStringMap& strMap)
{
  xml::displayStringStringMap(fPort, outerTagName,
                              tagName, firstPairTagName, secPairTagName,
                              strMap);
}


void
XmlRenderer::displayNode(zstring tagName, AptNode* node)
{
  if (node) {
    displayOpenTag(tagName);
    render(node);
    displayCloseTag(tagName);
  }
}


void
XmlRenderer::displayNodeList(zstring tagName, const NodeList& nodelist)
{
  if (!nodelist.empty()) {
    displayOpenTag(tagName);

    for (NodeList::const_iterator it = nodelist.begin();
         it != nodelist.end();
         it++)
    {
      AptNode* n = (*it);
      if (n)
        render(n);
    }

    displayCloseTag(tagName);
  }
}


void
XmlRenderer::displayType(zstring tagName, const Type& type)
{
  xml::displayType(fPort, tagName, type);
}


void
XmlRenderer::displayTypeVector(zstring tagName, const TypeVector& types)
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

  zstring referTag = nullptr;
  switch (node->refersTo()) {
  case kFreeVar:   referTag = nullptr; break;
  case kGlobalVar: referTag = "global"; break;
  case kLocalVar:  referTag = "local"; break;
  case kParam:     referTag = "param"; break;
  case kSlot:      referTag = "slot"; break;
  case kFunction:  referTag = "function"; break;
  case kGeneric:   referTag = "generic"; break;
  case kType:      referTag = "type"; break;
  }

  if (referTag)
    attrs << " refer='" << referTag << "'";

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
    hr_invalid("unexpected type node");
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

  displayOpenTagAttrs("int", StrHelper(attrs.toString()), !K(newline));
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
  displayNode(nullptr, node->value());
  displayCloseTag("uvalue");
}


void
XmlRenderer::renderNode(const CompileUnitNode* node)
{
  zstring attrs = "xmlns:ty='http://herschel.eyestep.org/types'";
  displayOpenTagAttrs("compile-unit", attrs);
  displayNodeList(nullptr, node->children());

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

  fPort->flush();
}


void
XmlRenderer::renderNode(const LetNode* node)
{
  displayOpenTag("let");
  displayNode(nullptr, node->defNode());
  displayCloseTag("let");
}


void
XmlRenderer::renderNode(const DefNode* node)
{
  displayOpenTag("def");
  displayNode(nullptr, node->defNode());
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
  displayNodeList(nullptr, node->children());
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
  displayNodeList(nullptr, node->children());
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
  displayNodeList(nullptr, node->children());
  displayCloseTag("dict");
}


void
XmlRenderer::renderNode(const BinaryNode* node)
{
  StringBuffer attrs;
  attrs << "op='" << xmlEncode(herschel::operatorName(node->op())) << "'";

  if (fShowNodeType && node->type().isDef()) {
    attrs << " ty='" << xmlEncode(node->type().typeId()) << "'";
    fReferencedTypes.insert(std::make_pair(node->type().typeId(),
                                           node->type()));
  }

  displayOpenTagAttrs("binary", StrHelper(attrs.toString()));
  displayNode(nullptr, node->left());
  displayNode(nullptr, node->right());
  displayCloseTag("binary");
}


static zstring
unary_operator_name(UnaryOperatorType op)
{
  switch (op) {
  case kUnaryOpNegate:
    return "neg";
  case kUnaryOpNot:
    return "not";
  case kUnaryOpInvalid:
    hr_invalid("unhandled unary operator");
    break;
  }

  return "?";
}


void
XmlRenderer::renderNode(const UnaryNode* node)
{
  zstring op_nm = unary_operator_name(node->op());
  StringBuffer attrs;
  if (fShowNodeType && node->type().isDef()) {
    attrs << " ty='" << xmlEncode(node->type().typeId()) << "'";
    fReferencedTypes.insert(std::make_pair(node->type().typeId(),
                                           node->type()));
  }

  displayOpenTagAttrs(op_nm, StrHelper(attrs.toString()));
  displayNode(nullptr, node->base());
  displayCloseTag(op_nm);
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
  displayNode(nullptr, node->from());
  displayNode(nullptr, node->to());
  displayNode(nullptr, node->by());
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
  displayNode(nullptr, node->lvalue());
  displayNode(nullptr, node->rvalue());
  displayCloseTag("assign");
}


void
XmlRenderer::renderNode(const SlotRefNode* node)
{
  StringBuffer attrs;
  attrs << " nm='" << xmlEncode(node->slotName()) << "'";

  if (fShowNodeType && node->type().isDef()) {
    attrs << " ty='" << xmlEncode(node->type().typeId()) << "'";
    fReferencedTypes.insert(std::make_pair(node->type().typeId(),
                                           node->type()));
  }

  displayOpenTagAttrs("slotref", StrHelper(attrs.toString()));
  displayNode(nullptr, node->base());
  displayCloseTag("slotref");
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
        displayNode(nullptr, node->mappingAt(i).fTestValues[j]);
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
  // StringBuffer attrs;

  // if (fShowNodeType && node->type().isDef())
  //   attrs << " ty='" << xmlEncode(node->type().typeId()) << "'";

  // displayOpenTagAttrs("block", StrHelper(attrs.toString()));
  // displayNodeList(nullptr, node->children());
  // displayCloseTag("block");
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
  zstring tag = nullptr;
  if (node->isGeneric())
    tag = "generic";
  else if (node->isMethod())
    tag = "method";
  else
    tag = "func";

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

  if (Properties::isTypeConvDump())
    attrs << xml::displayTypeConv(node);

  displayOpenTagAttrs("apply", StrHelper(attrs.toString()));
  displayNode(nullptr, node->base());
  displayOpenTag("args");
  displayNodeList(nullptr, node->children());
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
  displayNode(nullptr, node->value());
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
  zstring tagName = node->isClass() ? "class" : "type";

  StringBuffer attrs;
  attrs << "nm='" << node->name() << "'";

  displayOpenTagAttrs(tagName, StrHelper(attrs.toString()));
  displayNodeList("params", node->params());
  displayNodeList("slots", node->slots());
  displayNodeList("on", node->onExprs());
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


void
XmlRenderer::renderNode(const UndefNode* node)
{
  displayEmptyTag("undef");
}
