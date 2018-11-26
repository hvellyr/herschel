/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "xmlrenderer.hpp"

#include "ast.hpp"
#include "port.hpp"
#include "properties.hpp"
#include "strbuf.hpp"
#include "xmlout.hpp"
#include "xmlrenderer.hpp"

#include <cstring>
#include <typeinfo>


namespace herschel {

template <typename T>
struct NodeRenderer {
  static void render(XmlRenderer* renderer, T node);
};


template <>
struct NodeRenderer<const ApplyNode&> {
  static void render(XmlRenderer* renderer, const ApplyNode& node)
  {
    StringBuffer attrs;

    if (renderer->fShowNodeType && node.type().isDef())
      attrs << " ty='" << xmlEncode(node.type().typeId()) << "'";

    if (Properties::isTypeConvDump())
      attrs << xml::displayTypeConv(node);

    renderer->displayOpenTagAttrs("apply", StrHelper(attrs.toString()));
    renderer->displayNode(nullptr, node.base().get());
    renderer->displayOpenTag("args");
    renderer->displayNodeList(nullptr, node.children());
    renderer->displayCloseTag("args");
    renderer->displayCloseTag("apply");

    if (renderer->fShowNodeType && node.type().isDef())
      renderer->fReferencedTypes.insert(
          std::make_pair(node.type().typeId(), node.type()));
  }
};


template <>
struct NodeRenderer<const ArrayNode&> {
  static void render(XmlRenderer* renderer, const ArrayNode& node)
  {
    StringBuffer attrs;

    if (renderer->fShowNodeType && node.type().isDef()) {
      attrs << " ty='" << xmlEncode(node.type().typeId()) << "'";
      renderer->fReferencedTypes.insert(
          std::make_pair(node.type().typeId(), node.type()));
    }

    renderer->displayOpenTagAttrs("array", StrHelper(attrs.toString()));
    renderer->displayNodeList(nullptr, node.children());
    renderer->displayCloseTag("array");
  }
};


template <>
struct NodeRenderer<const ArrayTypeNode&> {
  static void render(XmlRenderer* renderer, const ArrayTypeNode& node)
  {
    auto rootType = node.typeNode();

    if (auto sym = dynamic_cast<SymbolNode*>(rootType.get())) {
      StringBuffer attrs;

      if (renderer->fShowNodeType && node.type().isDef()) {
        attrs << " ty='" << xmlEncode(node.type().typeId()) << "'";
        renderer->fReferencedTypes.insert(
            std::make_pair(node.type().typeId(), node.type()));
      }

      attrs << " array='t'";
      renderer->displayTagAttr("symbol", StrHelper(attrs.toString()), sym->name());
    }
    else if (auto ty = dynamic_cast<TypeNode*>(rootType.get())) {
      StringBuffer attrs;

      attrs << " array='t'";
      attrs << " ty='" << xmlEncode(ty->type().typeId()) << "'";
      renderer->displayTagAttr("type", StrHelper(attrs.toString()), String());
      renderer->fReferencedTypes.insert(
          std::make_pair(node.type().typeId(), node.type()));
    }
    else if (rootType) {
      fprintf(stderr, "Unexpected type node: %p %s\n", rootType.get(),
              typeid(rootType.get()).name());
      hr_invalid("unexpected type node");
    }
  }
};


template <>
struct NodeRenderer<const AssignNode&> {
  static void render(XmlRenderer* renderer, const AssignNode& node)
  {
    StringBuffer attrs;
    if (renderer->fShowNodeType && node.type().isDef()) {
      attrs << " ty='" << xmlEncode(node.type().typeId()) << "'";
      renderer->fReferencedTypes.insert(
          std::make_pair(node.type().typeId(), node.type()));
    }

    renderer->displayOpenTagAttrs("assign", StrHelper(attrs.toString()));
    renderer->displayNode(nullptr, node.lvalue().get());
    renderer->displayNode(nullptr, node.rvalue().get());
    renderer->displayCloseTag("assign");
  }
};


template <>
struct NodeRenderer<const BinaryNode&> {
  static void render(XmlRenderer* renderer, const BinaryNode& node)
  {
    StringBuffer attrs;
    attrs << "op='" << xmlEncode(herschel::operatorName(node.op())) << "'";

    if (renderer->fShowNodeType && node.type().isDef()) {
      attrs << " ty='" << xmlEncode(node.type().typeId()) << "'";
      renderer->fReferencedTypes.insert(
          std::make_pair(node.type().typeId(), node.type()));
    }

    renderer->displayOpenTagAttrs("binary", StrHelper(attrs.toString()));
    renderer->displayNode(nullptr, node.left().get());
    renderer->displayNode(nullptr, node.right().get());
    renderer->displayCloseTag("binary");
  }
};


template <>
struct NodeRenderer<const BlockNode&> {
  static void render(XmlRenderer* renderer, const BlockNode& node)
  {
    // StringBuffer attrs;

    // if (fShowNodeType && node.type().isDef())
    //   attrs << " ty='" << xmlEncode(node.type().typeId()) << "'";

    // displayOpenTagAttrs("block", StrHelper(attrs.toString()));
    // displayNodeList(nullptr, node.children());
    // displayCloseTag("block");
    renderer->displayNodeList("block", node.children());
  }
};


template <>
struct NodeRenderer<const BoolNode&> {
  static void render(XmlRenderer* renderer, const BoolNode& node)
  {
    if (renderer->fShowNodeType) {
      StringBuffer attrs;
      attrs << " ty='" << xmlEncode(node.type().typeId()) << "'";
      renderer->displayEmptyTagAttrs(node.value() ? "true" : "false",
                                     StrHelper(attrs.toString()));
    }
    else
      renderer->displayEmptyTag(node.value() ? "true" : "false");

    if (renderer->fShowNodeType)
      renderer->fReferencedTypes.insert(
          std::make_pair(node.type().typeId(), node.type()));
  }
};


template <>
struct NodeRenderer<const CastNode&> {
  static void render(XmlRenderer* renderer, const CastNode& node)
  {
    renderer->displayOpenTag("cast");
    renderer->displayNode("base", node.base().get());
    renderer->displayType("as", node.type());
    renderer->displayCloseTag("cast");
  }
};


template <>
struct NodeRenderer<const CharNode&> {
  static void render(XmlRenderer* renderer, const CharNode& node)
  {
    renderer->displayTag("char", fromInt(int(node.value())));
  }
};


template <>
struct NodeRenderer<const CompileUnitNode&> {
  static void render(XmlRenderer* renderer, const CompileUnitNode& node)
  {
    zstring attrs = "xmlns:ty='http://herschel.eyestep.org/types'";
    renderer->displayOpenTagAttrs("compile-unit", attrs);
    renderer->displayNodeList(nullptr, node.children());

    if (renderer->fShowNodeType && !renderer->fReferencedTypes.empty()) {
      renderer->displayOpenTag("ty:node-types");
      for (std::map<String, Type>::iterator it = renderer->fReferencedTypes.begin(),
                                            e = renderer->fReferencedTypes.end();
           it != e; ++it) {
        renderer->displayType("ty:used-type", it->second);
      }
      renderer->displayCloseTag("ty:node-types");
    }

    renderer->displayCloseTag("compile-unit");

    renderer->fPort->flush();
  }
};


template <>
struct NodeRenderer<const DefNode&> {
  static void render(XmlRenderer* renderer, const DefNode& node)
  {
    renderer->displayOpenTag("def");
    renderer->displayNode(nullptr, node.defNode().get());
    renderer->displayCloseTag("def");
  }
};


template <>
struct NodeRenderer<const DictNode&> {
  static void render(XmlRenderer* renderer, const DictNode& node)
  {
    StringBuffer attrs;

    if (renderer->fShowNodeType && node.type().isDef()) {
      attrs << " ty='" << xmlEncode(node.type().typeId()) << "'";
      renderer->fReferencedTypes.insert(
          std::make_pair(node.type().typeId(), node.type()));
    }

    renderer->displayOpenTagAttrs("dict", StrHelper(attrs.toString()));
    renderer->displayNodeList(nullptr, node.children());
    renderer->displayCloseTag("dict");
  }
};


template <>
struct NodeRenderer<const FuncDefNode&> {
  static void render(XmlRenderer* renderer, const FuncDefNode& node)
  {
    zstring tag = nullptr;
    if (node.isGeneric())
      tag = "generic";
    else if (node.isMethod())
      tag = "method";
    else
      tag = "func";

    StringBuffer attrs;
    attrs << "sym='" << node.name() << "'";

    if (node.isAbstract())
      attrs << " abstract='true'";

    if (!node.linkage().isEmpty())
      attrs << " linkage='" << node.linkage() << "'";

    renderer->displayOpenTagAttrs(tag, StrHelper(attrs.toString()));
    renderer->displayNodeList("params", node.params());
    renderer->displayType("rettype", node.retType());
    renderer->displayNode("body", node.body().get());
    renderer->displayCloseTag(tag);
  }
};


template <>
struct NodeRenderer<const FunctionNode&> {
  static void render(XmlRenderer* renderer, const FunctionNode& node)
  {
    renderer->displayOpenTag("function");
    renderer->displayNodeList("params", node.params());
    renderer->displayType("rettype", node.retType());
    renderer->displayNode("body", node.body().get());
    renderer->displayCloseTag("function");
  }
};


template <>
struct NodeRenderer<const IfNode&> {
  static void render(XmlRenderer* renderer, const IfNode& node)
  {
    StringBuffer attrs;
    if (renderer->fShowNodeType && node.type().isDef()) {
      attrs << " ty='" << xmlEncode(node.type().typeId()) << "'";
      renderer->fReferencedTypes.insert(
          std::make_pair(node.type().typeId(), node.type()));
    }

    renderer->displayOpenTagAttrs("if", StrHelper(attrs.toString()));
    renderer->displayNode("test", node.test().get());
    renderer->displayNode("then", node.consequent().get());
    renderer->displayNode("else", node.alternate().get());
    renderer->displayCloseTag("if");
  }
};


template <>
struct NodeRenderer<const IntNode&> {
  static void render(XmlRenderer* renderer, const IntNode& node)
  {
    StringBuffer attrs;
    attrs << "ty='" << xmlEncode(node.type().typeId()) << "'";

    if (node.isImaginary())
      attrs << " imag='t'";

    renderer->displayOpenTagAttrs("int", StrHelper(attrs.toString()), !K(newline));
    herschel::display(*renderer->fPort, xmlEncode(String() + node.value()));
    renderer->displayCloseTag("int");

    if (renderer->fShowNodeType)
      renderer->fReferencedTypes.insert(
          std::make_pair(node.type().typeId(), node.type()));
  }
};


template <>
struct NodeRenderer<const KeyargNode&> {
  static void render(XmlRenderer* renderer, const KeyargNode& node)
  {
    StringBuffer attrs;
    attrs << "key='" << node.key() << "'";

    renderer->displayOpenTagAttrs("arg", StrHelper(attrs.toString()));
    renderer->displayNode(nullptr, node.value().get());
    renderer->displayCloseTag("arg");
  }
};


template <>
struct NodeRenderer<const KeywordNode&> {
  static void render(XmlRenderer* renderer, const KeywordNode& node)
  {
    renderer->displayTag("keyw", node.value());
  }
};


template <>
struct NodeRenderer<const LetNode&> {
  static void render(XmlRenderer* renderer, const LetNode& node)
  {
    renderer->displayOpenTag("let");
    renderer->displayNode(nullptr, node.defNode().get());
    renderer->displayCloseTag("let");
  }
};


template <>
struct NodeRenderer<const MatchNode&> {
  static void render(XmlRenderer* renderer, const MatchNode& node)
  {
    renderer->displayOpenTag("match");
    renderer->displayNode("test", node.expr().get());

    for (size_t i = 0; i < node.mappingCount(); i++) {
      StringBuffer attrs;
      if (!node.mappingAt(i).fVarName.isEmpty())
        attrs << "nm='" << xmlEncode(node.mappingAt(i).fVarName) << "'";
      renderer->displayOpenTagAttrs("map", StrHelper(attrs.toString()));
      renderer->displayType("type", node.mappingAt(i).fMatchType);
      renderer->displayNode("cons", node.mappingAt(i).fConsequent.get());
      renderer->displayCloseTag("map");
    }

    renderer->displayCloseTag("match");
  }
};


template <>
struct NodeRenderer<const ParamNode&> {
  static void render(XmlRenderer* renderer, const ParamNode& node)
  {
    StringBuffer attrs;

    attrs << "sym='" << node.name() << "'";

    switch (node.flags()) {
    case kPosArg: attrs << " type='pos'"; break;
    case kSpecArg: attrs << " type='spec'"; break;
    case kNamedArg: attrs << " type='key' key='" << node.key() << "'"; break;
    case kRestArg: attrs << " type='rest'"; break;
    }

    switch (node.allocType()) {
    case kAlloc_Local: break;
    case kAlloc_Shared: attrs << " alloc='shared'"; break;
    }

    renderer->displayOpenTagAttrs("param", StrHelper(attrs.toString()));

    renderer->displayType("type", node.type());
    renderer->displayNode("init", node.initExpr().get());

    renderer->displayCloseTag("param");
  }
};


template <>
struct NodeRenderer<const RangeNode&> {
  static void render(XmlRenderer* renderer, const RangeNode& node)
  {
    StringBuffer attrs;
    if (renderer->fShowNodeType && node.type().isDef()) {
      attrs << " ty='" << xmlEncode(node.type().typeId()) << "'";
      renderer->fReferencedTypes.insert(
          std::make_pair(node.type().typeId(), node.type()));
    }

    renderer->displayOpenTagAttrs("range", StrHelper(attrs.toString()));
    renderer->displayNode(nullptr, node.from().get());
    renderer->displayNode(nullptr, node.to().get());
    renderer->displayNode(nullptr, node.by().get());
    renderer->displayCloseTag("range");
  }
};


template <>
struct NodeRenderer<const RationalNode&> {
  static void render(XmlRenderer* renderer, const RationalNode& node)
  {
    StringBuffer attrs;
    attrs << "ty='" << xmlEncode(node.type().typeId()) << "'";
    if (node.isImaginary())
      attrs << " imag='t'";

    String val = String() + node.value().numerator() + "/" + node.value().denominator();
    renderer->displayTagAttr("real", StrHelper(attrs.toString()), val);

    if (renderer->fShowNodeType)
      renderer->fReferencedTypes.insert(
          std::make_pair(node.type().typeId(), node.type()));
  }
};


template <>
struct NodeRenderer<const RealNode&> {
  static void render(XmlRenderer* renderer, const RealNode& node)
  {
    StringBuffer attrs;
    attrs << "ty='" << xmlEncode(node.type().typeId()) << "'";
    if (node.isImaginary())
      attrs << " imag='t'";

    renderer->displayTagAttr("real", StrHelper(attrs.toString()),
                             String() + node.value());

    if (renderer->fShowNodeType)
      renderer->fReferencedTypes.insert(
          std::make_pair(node.type().typeId(), node.type()));
  }
};


template <>
struct NodeRenderer<const SelectNode&> {
  static void render(XmlRenderer* renderer, const SelectNode& node)
  {
    renderer->displayOpenTag("select");
    renderer->displayNode("test", node.test().get());
    renderer->displayNode("comp", node.comparator().get());

    for (size_t i = 0; i < node.mappingCount(); i++) {
      if (node.mappingAt(i).fTestValues.empty()) {
        renderer->displayNode("alternate", node.mappingAt(i).fConsequent.get());
      }
      else {
        renderer->displayOpenTag("map");
        renderer->displayOpenTag("values");
        for (size_t j = 0; j < node.mappingAt(i).fTestValues.size(); j++) {
          renderer->displayNode(nullptr, node.mappingAt(i).fTestValues[j].get());
        }
        renderer->displayCloseTag("values");
        renderer->displayNode("cons", node.mappingAt(i).fConsequent.get());
        renderer->displayCloseTag("map");
      }
    }

    renderer->displayCloseTag("select");
  }
};


template <>
struct NodeRenderer<const SlotdefNode&> {
  static void render(XmlRenderer* renderer, const SlotdefNode& node)
  {
    StringBuffer attrs;

    attrs << "sym='" << node.name() << "'";

    if ((node.flags() & kTransientSlot) != 0) {
      attrs << " transient='t'";
    }
    if ((node.flags() & kReadonlySlot) != 0) {
      attrs << " readonly='t'";
    }
    if ((node.flags() & kAutoSlot) != 0) {
      attrs << " auto='t'";
    }

    if ((node.flags() & kPublicSlot) != 0) {
      attrs << " viz='public'";
    }
    else if ((node.flags() & kOuterSlot) != 0) {
      attrs << " viz='outer'";
    }
    else if ((node.flags() & kInnerSlot) != 0) {
      attrs << " viz='inner'";
    }

    switch (node.allocType()) {
    case kAlloc_Local: break;
    case kAlloc_Shared: attrs << " alloc='shared'"; break;
    }

    renderer->displayOpenTagAttrs("slot", StrHelper(attrs.toString()));
    renderer->displayType("type", node.type());
    renderer->displayNode("init", node.initExpr().get());
    renderer->displayCloseTag("slot");
  }
};


template <>
struct NodeRenderer<const SlotRefNode&> {
  static void render(XmlRenderer* renderer, const SlotRefNode& node)
  {
    StringBuffer attrs;
    attrs << " nm='" << xmlEncode(node.slotName()) << "'";

    if (renderer->fShowNodeType && node.type().isDef()) {
      attrs << " ty='" << xmlEncode(node.type().typeId()) << "'";
      renderer->fReferencedTypes.insert(
          std::make_pair(node.type().typeId(), node.type()));
    }

    renderer->displayOpenTagAttrs("slotref", StrHelper(attrs.toString()));
    renderer->displayNode(nullptr, node.base().get());
    renderer->displayCloseTag("slotref");
  }
};


template <>
struct NodeRenderer<const StringNode&> {
  static void render(XmlRenderer* renderer, const StringNode& node)
  {
    renderer->displayTag("str", node.value());
  }
};


template <>
struct NodeRenderer<const SymbolNode&> {
  static void render(XmlRenderer* renderer, const SymbolNode& node)
  {
    StringBuffer attrs;
    if (renderer->fShowNodeType && node.type().isDef())
      attrs << " ty='" << xmlEncode(node.type().typeId()) << "'";

    zstring referTag = nullptr;
    switch (node.refersTo()) {
    case kFreeVar: referTag = nullptr; break;
    case kGlobalVar: referTag = "global"; break;
    case kLocalVar: referTag = "local"; break;
    case kParam: referTag = "param"; break;
    case kSlot: referTag = "slot"; break;
    case kFunction: referTag = "function"; break;
    case kGeneric: referTag = "generic"; break;
    case kType: referTag = "type"; break;
    }

    if (referTag)
      attrs << " refer='" << referTag << "'";

    if (node.generics().empty()) {
      if (node.isShared())
        attrs << " acc='shared'";
      renderer->displayTagAttr("symbol", StrHelper(attrs.toString()), node.name());
    }
    else {
      attrs << " nm='" << node.name() << "'";
      if (node.isShared())
        attrs << " acc='shared'";

      renderer->displayOpenTagAttrs("symbol", StrHelper(attrs.toString()));
      renderer->displayTypeVector("gen", node.generics());
      renderer->displayCloseTag("symbol");
    }

    if (renderer->fShowNodeType && node.type().isDef())
      renderer->fReferencedTypes.insert(
          std::make_pair(node.type().typeId(), node.type()));
  }
};


template <>
struct NodeRenderer<const TypeDefNode&> {
  static void render(XmlRenderer* renderer, const TypeDefNode& node)
  {
    zstring tagName = node.isClass() ? "class" : "type";

    StringBuffer attrs;
    attrs << "nm='" << node.name() << "'";

    renderer->displayOpenTagAttrs(tagName, StrHelper(attrs.toString()));
    renderer->displayNodeList("params", node.params());
    renderer->displayNodeList("slots", node.slots());
    renderer->displayNodeList("on", node.onExprs());
    renderer->displayType("isa", node.defType());
    renderer->displayCloseTag(tagName);
  }
};


template <>
struct NodeRenderer<const TypeNode&> {
  static void render(XmlRenderer* renderer, const TypeNode& node)
  {
    StringBuffer attrs;
    if (node.type().isDef()) {
      attrs << " ty='" << xmlEncode(node.type().typeId()) << "'";
      renderer->fReferencedTypes.insert(
          std::make_pair(node.type().typeId(), node.type()));
    }

    renderer->displayTagAttr("type", StrHelper(attrs.toString()), String());
  }
};


static zstring unary_operator_name(UnaryOperatorType op)
{
  switch (op) {
  case kUnaryOpNegate: return "neg";
  case kUnaryOpNot: return "not";
  case kUnaryOpInvalid: hr_invalid("unhandled unary operator"); break;
  }

  return "?";
}


template <>
struct NodeRenderer<const UnaryNode&> {
  static void render(XmlRenderer* renderer, const UnaryNode& node)
  {
    zstring op_nm = unary_operator_name(node.op());
    StringBuffer attrs;
    if (renderer->fShowNodeType && node.type().isDef()) {
      attrs << " ty='" << xmlEncode(node.type().typeId()) << "'";
      renderer->fReferencedTypes.insert(
          std::make_pair(node.type().typeId(), node.type()));
    }

    renderer->displayOpenTagAttrs(op_nm, StrHelper(attrs.toString()));
    renderer->displayNode(nullptr, node.base().get());
    renderer->displayCloseTag(op_nm);
  }
};


template <>
struct NodeRenderer<const UndefNode&> {
  static void render(XmlRenderer* renderer, const UndefNode& node)
  {
    renderer->displayEmptyTag("undef");
  }
};


template <>
struct NodeRenderer<const UnitConstNode&> {
  static void render(XmlRenderer* renderer, const UnitConstNode& node)
  {
    StringBuffer attrs;
    attrs << "unit='" << node.unit().name() << "'";

    renderer->displayOpenTagAttrs("uvalue", StrHelper(attrs.toString()));
    renderer->displayType("type", node.unit().effType());
    renderer->displayNode(nullptr, node.value().get());
    renderer->displayCloseTag("uvalue");
  }
};


template <>
struct NodeRenderer<const VardefNode&> {
  static void render(XmlRenderer* renderer, const VardefNode& node)
  {
    StringBuffer attrs;

    attrs << "sym='" << node.name() << "'";

    switch (node.flags()) {
    case kNormalVar: break;
    case kConstVar: attrs << " type='const'"; break;
    case kConfigVar: attrs << " type='config'"; break;
    case kEnumVar: attrs << " type='enum'"; break;
    }

    if (!node.linkage().isEmpty())
      attrs << " linkage='" << node.linkage() << "'";

    switch (node.allocType()) {
    case kAlloc_Local: break;
    case kAlloc_Shared: attrs << " alloc='shared'"; break;
    }

    renderer->displayOpenTagAttrs("vardef", StrHelper(attrs.toString()));

    renderer->displayType("type", node.type());
    renderer->displayNode("init", node.initExpr().get());

    renderer->displayCloseTag("vardef");
  }
};


template <>
struct NodeRenderer<const VectorNode&> {
  static void render(XmlRenderer* renderer, const VectorNode& node)
  {
    StringBuffer attrs;

    if (renderer->fShowNodeType && node.type().isDef()) {
      attrs << " ty='" << xmlEncode(node.type().typeId()) << "'";
      renderer->fReferencedTypes.insert(
          std::make_pair(node.type().typeId(), node.type()));
    }

    renderer->displayOpenTagAttrs("vector", StrHelper(attrs.toString()));
    renderer->displayNodeList(nullptr, node.children());
    renderer->displayCloseTag("vector");
  }
};


template <>
struct NodeRenderer<const WhileNode&> {
  static void render(XmlRenderer* renderer, const WhileNode& node)
  {
    StringBuffer attrs;

    if (renderer->fShowNodeType && node.type().isDef()) {
      attrs << " ty='" << xmlEncode(node.type().typeId()) << "'";
      renderer->fReferencedTypes.insert(
          std::make_pair(node.type().typeId(), node.type()));
    }

    renderer->displayOpenTagAttrs("while", StrHelper(attrs.toString()));
    renderer->displayNode("test", node.test().get());
    renderer->displayNode("body", node.body().get());
    renderer->displayCloseTag("while");
  }
};  // namespace herschel


//----------------------------------------------------------------------------------------

void xml::displayType(Port<Octet>& port, zstring tagName, const Type& type)
{
  if (type.isDef()) {
    displayOpenTag(port, tagName);
    herschel::display(port, type.toString());
    displayCloseTag(port, tagName);
  }
}


void xml::displayTypeVector(Port<Octet>& port, zstring tagName, const TypeVector& types)
{
  if (!types.empty()) {
    displayOpenTag(port, tagName);
    for (size_t i = 0; i < types.size(); i++)
      herschel::display(port, types[i].toString());
    displayCloseTag(port, tagName);
  }
}


String xml::displayTypeConv(const AstNode& node)
{
  StringBuffer attrs;

  switch (node.typeConv()) {
  case kNoConv: attrs << " conv='none'"; break;
  case kTypeCheckConv: attrs << " conv='check'"; break;
  case kAtom2PlainConv: attrs << " conv='atom2plain'"; break;
  case kPlain2AtomConv: attrs << " conv='plain2atom'"; break;
  };

  if (node.type() != node.dstType())
    attrs << " dstty='" << node.dstType().typeId() << "'";

  return attrs.toString();
}


void xml::dump(const AstNode& node)
{
  XmlRenderer out{ std::make_shared<FilePort>(stderr) };
  out.render(node);
}


XmlRenderer::XmlRenderer(std::shared_ptr<Port<Octet>> port, bool showNodeType)
    : fPort(port)
    , fShowNodeType(showNodeType)
{
  display(*fPort, "<?xml version='1.0' encoding='utf-8'?>\n");
}


void XmlRenderer::render(const AstNode& node)
{
  if (auto* applynd = dynamic_cast<const ApplyNode*>(&node)) {
    NodeRenderer<decltype(*applynd)>::render(this, *applynd);
  }
  else if (auto* arraynd = dynamic_cast<const ArrayNode*>(&node)) {
    NodeRenderer<decltype(*arraynd)>::render(this, *arraynd);
  }
  else if (auto* arraytynd = dynamic_cast<const ArrayTypeNode*>(&node)) {
    NodeRenderer<decltype(*arraytynd)>::render(this, *arraytynd);
  }
  else if (auto* assignnd = dynamic_cast<const AssignNode*>(&node)) {
    NodeRenderer<decltype(*assignnd)>::render(this, *assignnd);
  }
  else if (auto* binnd = dynamic_cast<const BinaryNode*>(&node)) {
    NodeRenderer<decltype(*binnd)>::render(this, *binnd);
  }
  else if (auto* blocknd = dynamic_cast<const BlockNode*>(&node)) {
    NodeRenderer<decltype(*blocknd)>::render(this, *blocknd);
  }
  else if (auto* boolnd = dynamic_cast<const BoolNode*>(&node)) {
    NodeRenderer<decltype(*boolnd)>::render(this, *boolnd);
  }
  else if (auto* charnd = dynamic_cast<const CharNode*>(&node)) {
    NodeRenderer<decltype(*charnd)>::render(this, *charnd);
  }
  else if (auto* comunitnd = dynamic_cast<const CompileUnitNode*>(&node)) {
    NodeRenderer<decltype(*comunitnd)>::render(this, *comunitnd);
  }
  else if (auto* defnd = dynamic_cast<const DefNode*>(&node)) {
    NodeRenderer<decltype(*defnd)>::render(this, *defnd);
  }
  else if (auto* dictnd = dynamic_cast<const DictNode*>(&node)) {
    NodeRenderer<decltype(*dictnd)>::render(this, *dictnd);
  }
  else if (auto* funcdefnd = dynamic_cast<const FuncDefNode*>(&node)) {
    NodeRenderer<decltype(*funcdefnd)>::render(this, *funcdefnd);
  }
  else if (auto* functionnd = dynamic_cast<const FunctionNode*>(&node)) {
    NodeRenderer<decltype(*functionnd)>::render(this, *functionnd);
  }
  else if (auto* ifnd = dynamic_cast<const IfNode*>(&node)) {
    NodeRenderer<decltype(*ifnd)>::render(this, *ifnd);
  }
  else if (auto* intnd = dynamic_cast<const IntNode*>(&node)) {
    NodeRenderer<decltype(*intnd)>::render(this, *intnd);
  }
  else if (auto* keyargnd = dynamic_cast<const KeyargNode*>(&node)) {
    NodeRenderer<decltype(*keyargnd)>::render(this, *keyargnd);
  }
  else if (auto* keywnd = dynamic_cast<const KeywordNode*>(&node)) {
    NodeRenderer<decltype(*keywnd)>::render(this, *keywnd);
  }
  else if (auto* letnd = dynamic_cast<const LetNode*>(&node)) {
    NodeRenderer<decltype(*letnd)>::render(this, *letnd);
  }
  else if (auto* matchnd = dynamic_cast<const MatchNode*>(&node)) {
    NodeRenderer<decltype(*matchnd)>::render(this, *matchnd);
  }
  else if (auto* unarynd = dynamic_cast<const UnaryNode*>(&node)) {
    NodeRenderer<decltype(*unarynd)>::render(this, *unarynd);
  }
  else if (auto* paramnd = dynamic_cast<const ParamNode*>(&node)) {
    NodeRenderer<decltype(*paramnd)>::render(this, *paramnd);
  }
  else if (auto* rangend = dynamic_cast<const RangeNode*>(&node)) {
    NodeRenderer<decltype(*rangend)>::render(this, *rangend);
  }
  else if (auto* ratnd = dynamic_cast<const RationalNode*>(&node)) {
    NodeRenderer<decltype(*ratnd)>::render(this, *ratnd);
  }
  else if (auto* realnd = dynamic_cast<const RealNode*>(&node)) {
    NodeRenderer<decltype(*realnd)>::render(this, *realnd);
  }
  else if (auto* selnd = dynamic_cast<const SelectNode*>(&node)) {
    NodeRenderer<decltype(*selnd)>::render(this, *selnd);
  }
  else if (auto* slotdefnd = dynamic_cast<const SlotdefNode*>(&node)) {
    NodeRenderer<decltype(*slotdefnd)>::render(this, *slotdefnd);
  }
  else if (auto* slotrefnd = dynamic_cast<const SlotRefNode*>(&node)) {
    NodeRenderer<decltype(*slotrefnd)>::render(this, *slotrefnd);
  }
  else if (auto* strnd = dynamic_cast<const StringNode*>(&node)) {
    NodeRenderer<decltype(*strnd)>::render(this, *strnd);
  }
  else if (auto* symbolnd = dynamic_cast<const SymbolNode*>(&node)) {
    NodeRenderer<decltype(*symbolnd)>::render(this, *symbolnd);
  }
  else if (auto* typedefnd = dynamic_cast<const TypeDefNode*>(&node)) {
    NodeRenderer<decltype(*typedefnd)>::render(this, *typedefnd);
  }
  else if (auto* typend = dynamic_cast<const TypeNode*>(&node)) {
    NodeRenderer<decltype(*typend)>::render(this, *typend);
  }
  else if (auto* unitconstnd = dynamic_cast<const UnitConstNode*>(&node)) {
    NodeRenderer<decltype(*unitconstnd)>::render(this, *unitconstnd);
  }
  else if (auto* vardefnd = dynamic_cast<const VardefNode*>(&node)) {
    NodeRenderer<decltype(*vardefnd)>::render(this, *vardefnd);
  }
  else if (auto* vectornd = dynamic_cast<const VectorNode*>(&node)) {
    NodeRenderer<decltype(*vectornd)>::render(this, *vectornd);
  }
  else if (auto* whilend = dynamic_cast<const WhileNode*>(&node)) {
    NodeRenderer<decltype(*whilend)>::render(this, *whilend);
  }
  else if (auto* castnd = dynamic_cast<const CastNode*>(&node)) {
    NodeRenderer<decltype(*castnd)>::render(this, *castnd);
  }
  else if (auto* undefnd = dynamic_cast<const UndefNode*>(&node)) {
    NodeRenderer<decltype(*undefnd)>::render(this, *undefnd);
  }
}


void XmlRenderer::displayOpenTag(zstring tagName, bool newline)
{
  xml::displayOpenTag(*fPort, tagName, newline);
}


void XmlRenderer::displayOpenTagAttrs(zstring tagName, zstring attrs, bool newline)
{
  xml::displayOpenTagAttrs(*fPort, tagName, attrs, newline);
}


void XmlRenderer::displayCloseTag(zstring tagName)
{
  xml::displayCloseTag(*fPort, tagName);
}


void XmlRenderer::displayEmptyTag(zstring tagName)
{
  xml::displayEmptyTag(*fPort, tagName);
}


void XmlRenderer::displayEmptyTagAttrs(zstring tagName, zstring attrs)
{
  xml::displayEmptyTagAttrs(*fPort, tagName, attrs);
}


void XmlRenderer::displayTag(zstring tagName, const String& value)
{
  xml::displayTag(*fPort, tagName, value);
}


void XmlRenderer::displayTagAttr(zstring tagName, zstring attrs, const String& value)
{
  xml::displayTagAttr(*fPort, tagName, attrs, value);
}


void XmlRenderer::displayStringList(zstring outerTagName, zstring tagName,
                                    const StringList& strlist)
{
  xml::displayStringList(*fPort, outerTagName, tagName, strlist);
}


void XmlRenderer::displayStringStringMap(zstring outerTagName, zstring tagName,
                                         zstring firstPairTagName, zstring secPairTagName,
                                         const StringStringMap& strMap)
{
  xml::displayStringStringMap(*fPort, outerTagName, tagName, firstPairTagName,
                              secPairTagName, strMap);
}


void XmlRenderer::displayNode(zstring tagName, AstNode* node)
{
  if (node) {
    displayOpenTag(tagName);
    render(*node);
    displayCloseTag(tagName);
  }
}


void XmlRenderer::displayNodeList(zstring tagName, const NodeList& nodelist)
{
  if (!nodelist.empty()) {
    displayOpenTag(tagName);

    for (auto& nd : nodelist) {
      if (nd)
        render(*nd);
    }

    displayCloseTag(tagName);
  }
}


void XmlRenderer::displayType(zstring tagName, const Type& type)
{
  xml::displayType(*fPort, tagName, type);
}


void XmlRenderer::displayTypeVector(zstring tagName, const TypeVector& types)
{
  xml::displayTypeVector(*fPort, tagName, types);
}


}  // namespace herschel
