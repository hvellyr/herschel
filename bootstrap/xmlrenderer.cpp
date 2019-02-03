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
#include "traverse.hpp"
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
struct NodeRenderer<std::shared_ptr<ApplyNode>> {
  static void render(XmlRenderer* renderer, const ApplyNode& node)
  {
    StringBuffer attrs;

    if (renderer->fShowNodeType && node.type().isDef())
      attrs << "ty='" << xmlEncode(node.type().typeId()) << "'";

    if (Properties::isTypeConvDump())
      attrs << xml::displayTypeConv(node);

    renderer->displayOpenTagAttrs("apply", StrHelper(attrs.toString()));
    renderer->displayNode(nullptr, node.base());
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
struct NodeRenderer<std::shared_ptr<ArrayNode>> {
  static void render(XmlRenderer* renderer, const ArrayNode& node)
  {
    StringBuffer attrs;

    if (renderer->fShowNodeType && node.type().isDef()) {
      attrs << "ty='" << xmlEncode(node.type().typeId()) << "'";
      renderer->fReferencedTypes.insert(
          std::make_pair(node.type().typeId(), node.type()));
    }

    renderer->displayOpenTagAttrs("array", StrHelper(attrs.toString()));
    renderer->displayNodeList(nullptr, node.children());
    renderer->displayCloseTag("array");
  }
};


template <>
struct NodeRenderer<std::shared_ptr<ArrayTypeNode>> {
  static void render(XmlRenderer* renderer, const ArrayTypeNode& node)
  {
    auto rootType = node.typeNode();

    if (auto sym = dynamic_cast<SymbolNode*>(rootType.get())) {
      StringBuffer attrs;

      auto gap = "";
      if (renderer->fShowNodeType && node.type().isDef()) {
        attrs << "ty='" << xmlEncode(node.type().typeId()) << "'";
        renderer->fReferencedTypes.insert(
            std::make_pair(node.type().typeId(), node.type()));
        gap = " ";
      }

      attrs << gap << "array='t'";
      renderer->displayTagAttr("symbol", StrHelper(attrs.toString()), sym->name());
    }
    else if (auto ty = dynamic_cast<TypeNode*>(rootType.get())) {
      StringBuffer attrs;

      attrs << "array='t'";
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
struct NodeRenderer<std::shared_ptr<AssignNode>> {
  static void render(XmlRenderer* renderer, const AssignNode& node)
  {
    StringBuffer attrs;
    if (renderer->fShowNodeType && node.type().isDef()) {
      attrs << "ty='" << xmlEncode(node.type().typeId()) << "'";
      renderer->fReferencedTypes.insert(
          std::make_pair(node.type().typeId(), node.type()));
    }

    renderer->displayOpenTagAttrs("assign", StrHelper(attrs.toString()));
    renderer->displayNode(nullptr, node.lvalue());
    renderer->displayNode(nullptr, node.rvalue());
    renderer->displayCloseTag("assign");
  }
};


template <>
struct NodeRenderer<std::shared_ptr<BinaryNode>> {
  static void render(XmlRenderer* renderer, const BinaryNode& node)
  {
    StringBuffer attrs;
    attrs << "op='" << xmlEncode(herschel::operatorName(node.op())) << "'";

    if (renderer->fShowNodeType && node.type().isDef()) {
      attrs << " ty='" << xmlEncode(node.type().typeId()) << "'";
      renderer->fReferencedTypes.insert(
          std::make_pair(node.type().typeId(), node.type()));
    }

    if (auto funcNode = node.refFunction().lock()) {
      attrs << " opfunc='" << xmlEncode(funcNode->type().typeId()) << "'";
    }

    renderer->displayOpenTagAttrs("binary", StrHelper(attrs.toString()));
    renderer->displayNode(nullptr, node.left());
    renderer->displayNode(nullptr, node.right());
    renderer->displayCloseTag("binary");
  }
};


template <>
struct NodeRenderer<std::shared_ptr<BlockNode>> {
  static void render(XmlRenderer* renderer, const BlockNode& node)
  {
    // StringBuffer attrs;

    // if (fShowNodeType && node.type().isDef())
    //   attrs << "ty='" << xmlEncode(node.type().typeId()) << "'";

    // displayOpenTagAttrs("block", StrHelper(attrs.toString()));
    // displayNodeList(nullptr, node.children());
    // displayCloseTag("block");
    renderer->displayNodeList("block", node.children());
  }
};


template <>
struct NodeRenderer<std::shared_ptr<BoolNode>> {
  static void render(XmlRenderer* renderer, const BoolNode& node)
  {
    if (renderer->fShowNodeType) {
      StringBuffer attrs;
      attrs << "ty='" << xmlEncode(node.type().typeId()) << "'";
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
struct NodeRenderer<std::shared_ptr<CastNode>> {
  static void render(XmlRenderer* renderer, const CastNode& node)
  {
    renderer->displayOpenTag("cast");
    renderer->displayNode("base", node.base());
    renderer->displayType("as", node.type());
    renderer->displayCloseTag("cast");
  }
};


template <>
struct NodeRenderer<std::shared_ptr<CharNode>> {
  static void render(XmlRenderer* renderer, const CharNode& node)
  {
    renderer->displayTag("char", fromInt(int(node.value())));
  }
};


template <>
struct NodeRenderer<std::shared_ptr<CompileUnitNode>> {
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
struct NodeRenderer<std::shared_ptr<DefNode>> {
  static void render(XmlRenderer* renderer, const DefNode& node)
  {
    renderer->displayOpenTag("def");
    renderer->displayNode(nullptr, node.defNode());
    renderer->displayCloseTag("def");
  }
};


template <>
struct NodeRenderer<std::shared_ptr<DictNode>> {
  static void render(XmlRenderer* renderer, const DictNode& node)
  {
    StringBuffer attrs;

    if (renderer->fShowNodeType && node.type().isDef()) {
      attrs << "ty='" << xmlEncode(node.type().typeId()) << "'";
      renderer->fReferencedTypes.insert(
          std::make_pair(node.type().typeId(), node.type()));
    }

    renderer->displayOpenTagAttrs("dict", StrHelper(attrs.toString()));
    renderer->displayNodeList(nullptr, node.children());
    renderer->displayCloseTag("dict");
  }
};


template <>
struct NodeRenderer<std::shared_ptr<FuncDefNode>> {
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
    renderer->displayNode("body", node.body());
    renderer->displayCloseTag(tag);
  }
};


template <>
struct NodeRenderer<std::shared_ptr<FunctionNode>> {
  static void render(XmlRenderer* renderer, const FunctionNode& node)
  {
    renderer->displayOpenTag("function");
    renderer->displayNodeList("params", node.params());
    renderer->displayType("rettype", node.retType());
    renderer->displayNode("body", node.body());
    renderer->displayCloseTag("function");
  }
};


template <>
struct NodeRenderer<std::shared_ptr<IfNode>> {
  static void render(XmlRenderer* renderer, const IfNode& node)
  {
    StringBuffer attrs;
    if (renderer->fShowNodeType && node.type().isDef()) {
      attrs << "ty='" << xmlEncode(node.type().typeId()) << "'";
      renderer->fReferencedTypes.insert(
          std::make_pair(node.type().typeId(), node.type()));
    }

    renderer->displayOpenTagAttrs("if", StrHelper(attrs.toString()));
    renderer->displayNode("test", node.test());
    renderer->displayNode("then", node.consequent());
    renderer->displayNode("else", node.alternate());
    renderer->displayCloseTag("if");
  }
};


template <>
struct NodeRenderer<std::shared_ptr<IntNode>> {
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
struct NodeRenderer<std::shared_ptr<KeyargNode>> {
  static void render(XmlRenderer* renderer, const KeyargNode& node)
  {
    StringBuffer attrs;
    attrs << "ty='" << xmlEncode(node.type().typeId()) << "'";
    attrs << " key='" << node.key() << "'";

    renderer->displayOpenTagAttrs("arg", StrHelper(attrs.toString()));
    renderer->displayNode(nullptr, node.value());
    renderer->displayCloseTag("arg");
  }
};


template <>
struct NodeRenderer<std::shared_ptr<KeywordNode>> {
  static void render(XmlRenderer* renderer, const KeywordNode& node)
  {
    renderer->displayTag("keyw", node.value());
  }
};


template <>
struct NodeRenderer<std::shared_ptr<LetNode>> {
  static void render(XmlRenderer* renderer, const LetNode& node)
  {
    renderer->displayOpenTag("let");
    renderer->displayNode(nullptr, node.defNode());
    renderer->displayCloseTag("let");
  }
};


template <>
struct NodeRenderer<std::shared_ptr<MatchNode>> {
  static void render(XmlRenderer* renderer, const MatchNode& node)
  {
    renderer->displayOpenTag("match");
    renderer->displayNode("test", node.expr());

    for (size_t i = 0; i < node.mappingCount(); i++) {
      StringBuffer attrs;
      if (!node.mappingAt(i).fVarName.isEmpty())
        attrs << "nm='" << xmlEncode(node.mappingAt(i).fVarName) << "'";
      renderer->displayOpenTagAttrs("map", StrHelper(attrs.toString()));
      renderer->displayType("type", node.mappingAt(i).fMatchType);
      renderer->displayNode("cons", node.mappingAt(i).fConsequent);
      renderer->displayCloseTag("map");
    }

    renderer->displayCloseTag("match");
  }
};


template <>
struct NodeRenderer<std::shared_ptr<ParamNode>> {
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
    renderer->displayNode("init", node.initExpr());

    renderer->displayCloseTag("param");
  }
};


template <>
struct NodeRenderer<std::shared_ptr<RangeNode>> {
  static void render(XmlRenderer* renderer, const RangeNode& node)
  {
    StringBuffer attrs;
    if (renderer->fShowNodeType && node.type().isDef()) {
      attrs << "ty='" << xmlEncode(node.type().typeId()) << "'";
      renderer->fReferencedTypes.insert(
          std::make_pair(node.type().typeId(), node.type()));
    }

    renderer->displayOpenTagAttrs("range", StrHelper(attrs.toString()));
    renderer->displayNode(nullptr, node.from());
    renderer->displayNode(nullptr, node.to());
    renderer->displayNode(nullptr, node.by());
    renderer->displayCloseTag("range");
  }
};


template <>
struct NodeRenderer<std::shared_ptr<RationalNode>> {
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
struct NodeRenderer<std::shared_ptr<RealNode>> {
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
struct NodeRenderer<std::shared_ptr<SelectNode>> {
  static void render(XmlRenderer* renderer, const SelectNode& node)
  {
    renderer->displayOpenTag("select");
    renderer->displayNode("test", node.test());
    renderer->displayNode("comp", node.comparator());

    for (size_t i = 0; i < node.mappingCount(); i++) {
      if (node.mappingAt(i).fTestValues.empty()) {
        renderer->displayNode("alternate", node.mappingAt(i).fConsequent);
      }
      else {
        renderer->displayOpenTag("map");
        renderer->displayOpenTag("values");
        for (size_t j = 0; j < node.mappingAt(i).fTestValues.size(); j++) {
          renderer->displayNode(nullptr, node.mappingAt(i).fTestValues[j]);
        }
        renderer->displayCloseTag("values");
        renderer->displayNode("cons", node.mappingAt(i).fConsequent);
        renderer->displayCloseTag("map");
      }
    }

    renderer->displayCloseTag("select");
  }
};


template <>
struct NodeRenderer<std::shared_ptr<SlotdefNode>> {
  static void render(XmlRenderer* renderer, const SlotdefNode& node)
  {
    StringBuffer attrs;

    attrs << "sym='" << node.name() << "'";

    switch (node.allocType()) {
    case kAlloc_Local: break;
    case kAlloc_Shared: attrs << " alloc='shared'"; break;
    }

    renderer->displayOpenTagAttrs("slot", StrHelper(attrs.toString()));
    renderer->displayType("type", node.type());
    renderer->displayNode("init", node.initExpr());
    renderer->displayCloseTag("slot");
  }
};


template <>
struct NodeRenderer<std::shared_ptr<SlotRefNode>> {
  static void render(XmlRenderer* renderer, const SlotRefNode& node)
  {
    StringBuffer attrs;
    attrs << "nm='" << xmlEncode(node.slotName()) << "'";

    if (renderer->fShowNodeType && node.type().isDef()) {
      attrs << " ty='" << xmlEncode(node.type().typeId()) << "'";
      renderer->fReferencedTypes.insert(
          std::make_pair(node.type().typeId(), node.type()));
    }

    renderer->displayOpenTagAttrs("slotref", StrHelper(attrs.toString()));
    renderer->displayNode(nullptr, node.base());
    renderer->displayCloseTag("slotref");
  }
};


template <>
struct NodeRenderer<std::shared_ptr<StringNode>> {
  static void render(XmlRenderer* renderer, const StringNode& node)
  {
    renderer->displayTag("str", node.value());
  }
};


template <>
struct NodeRenderer<std::shared_ptr<SymbolNode>> {
  static void render(XmlRenderer* renderer, const SymbolNode& node)
  {
    StringBuffer attrs;

    auto gap = "";
    if (renderer->fShowNodeType && node.type().isDef()) {
      attrs << "ty='" << xmlEncode(node.type().typeId()) << "'";
      gap = " ";
    }

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

    if (referTag) {
      attrs << gap << "refer='" << referTag << "'";
      gap = " ";
    }

    if (node.generics().empty()) {
      if (node.isShared())
        attrs << gap << "acc='shared'";
      renderer->displayTagAttr("symbol", StrHelper(attrs.toString()), node.name());
    }
    else {
      attrs << gap << "nm='" << node.name() << "'";
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
struct NodeRenderer<std::shared_ptr<TypeDefNode>> {
  static void render(XmlRenderer* renderer, const TypeDefNode& node)
  {
    zstring tagName = node.isRecord() ? "record" : "type";

    StringBuffer attrs;
    attrs << "nm='" << node.name() << "'";

    renderer->displayOpenTagAttrs(tagName, StrHelper(attrs.toString()));
    renderer->displayNodeList("slots", node.slots());
    renderer->displayType("isa", node.defType());
    renderer->displayCloseTag(tagName);
  }
};


template <>
struct NodeRenderer<std::shared_ptr<TypeNode>> {
  static void render(XmlRenderer* renderer, const TypeNode& node)
  {
    StringBuffer attrs;
    if (node.type().isDef()) {
      attrs << "ty='" << xmlEncode(node.type().typeId()) << "'";
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
struct NodeRenderer<std::shared_ptr<UnaryNode>> {
  static void render(XmlRenderer* renderer, const UnaryNode& node)
  {
    zstring op_nm = unary_operator_name(node.op());
    StringBuffer attrs;
    if (renderer->fShowNodeType && node.type().isDef()) {
      attrs << "ty='" << xmlEncode(node.type().typeId()) << "'";
      renderer->fReferencedTypes.insert(
          std::make_pair(node.type().typeId(), node.type()));
    }

    renderer->displayOpenTagAttrs(op_nm, StrHelper(attrs.toString()));
    renderer->displayNode(nullptr, node.base());
    renderer->displayCloseTag(op_nm);
  }
};


template <>
struct NodeRenderer<std::shared_ptr<UndefNode>> {
  static void render(XmlRenderer* renderer, const UndefNode& node)
  {
    renderer->displayEmptyTag("undef");
  }
};


template <>
struct NodeRenderer<std::shared_ptr<VardefNode>> {
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
    renderer->displayNode("init", node.initExpr());

    renderer->displayCloseTag("vardef");
  }
};


template <>
struct NodeRenderer<std::shared_ptr<VectorNode>> {
  static void render(XmlRenderer* renderer, const VectorNode& node)
  {
    StringBuffer attrs;

    if (renderer->fShowNodeType && node.type().isDef()) {
      attrs << "ty='" << xmlEncode(node.type().typeId()) << "'";
      renderer->fReferencedTypes.insert(
          std::make_pair(node.type().typeId(), node.type()));
    }

    renderer->displayOpenTagAttrs("vector", StrHelper(attrs.toString()));
    renderer->displayNodeList(nullptr, node.children());
    renderer->displayCloseTag("vector");
  }
};


template <>
struct NodeRenderer<std::shared_ptr<WeakNode>> {
  static void render(XmlRenderer* renderer, const WeakNode& node)
  {
    if (node.refNode())
      renderer->displayNode(nullptr, node.refNode());
  }
};


template <>
struct NodeRenderer<std::shared_ptr<WhileNode>> {
  static void render(XmlRenderer* renderer, const WhileNode& node)
  {
    StringBuffer attrs;

    if (renderer->fShowNodeType && node.type().isDef()) {
      attrs << "ty='" << xmlEncode(node.type().typeId()) << "'";
      renderer->fReferencedTypes.insert(
          std::make_pair(node.type().typeId(), node.type()));
    }

    renderer->displayOpenTagAttrs("while", StrHelper(attrs.toString()));
    renderer->displayNode("test", node.test());
    renderer->displayNode("body", node.body());
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


void xml::dump(std::shared_ptr<AstNode> node)
{
  XmlRenderer out{ std::make_shared<FilePort>(stderr), true };
  out.render(node);
}


XmlRenderer::XmlRenderer(std::shared_ptr<Port<Octet>> port, bool showNodeType)
    : fPort(port)
    , fShowNodeType(showNodeType)
{
  display(*fPort, "<?xml version='1.0' encoding='utf-8'?>\n");
}


void XmlRenderer::render(std::shared_ptr<AstNode> node)
{
  dispatchNode<void>(node,
                     [&](auto nd) { NodeRenderer<decltype(nd)>::render(this, *nd); });
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


void XmlRenderer::displayNode(zstring tagName, std::shared_ptr<AstNode> node)
{
  if (node) {
    displayOpenTag(tagName);
    render(node);
    displayCloseTag(tagName);
  }
}


void XmlRenderer::displayNodeList(zstring tagName, const NodeList& nodelist)
{
  if (!nodelist.empty()) {
    displayOpenTag(tagName);

    for (auto& nd : nodelist) {
      if (nd)
        render(nd);
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
