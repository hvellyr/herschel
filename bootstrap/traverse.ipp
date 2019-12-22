/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "ast.hpp"


namespace herschel {

template <typename Ret, typename Functor>
Ret dispatchNode(std::shared_ptr<AstNode> node, Functor&& functor)
{
  if (auto applynd = std::dynamic_pointer_cast<ApplyNode>(node)) {
    return std::forward<Functor>(functor)(applynd);
  }
  else if (auto arraynd = std::dynamic_pointer_cast<ArrayNode>(node)) {
    return std::forward<Functor>(functor)(arraynd);
  }
  else if (auto arraytynd = std::dynamic_pointer_cast<ArrayTypeNode>(node)) {
    return std::forward<Functor>(functor)(arraytynd);
  }
  else if (auto assignnd = std::dynamic_pointer_cast<AssignNode>(node)) {
    return std::forward<Functor>(functor)(assignnd);
  }
  else if (auto binnd = std::dynamic_pointer_cast<BinaryNode>(node)) {
    return std::forward<Functor>(functor)(binnd);
  }
  else if (auto blocknd = std::dynamic_pointer_cast<BlockNode>(node)) {
    return std::forward<Functor>(functor)(blocknd);
  }
  else if (auto boolnd = std::dynamic_pointer_cast<BoolNode>(node)) {
    return std::forward<Functor>(functor)(boolnd);
  }
  else if (auto charnd = std::dynamic_pointer_cast<CharNode>(node)) {
    return std::forward<Functor>(functor)(charnd);
  }
  else if (auto comunitnd = std::dynamic_pointer_cast<CompileUnitNode>(node)) {
    return std::forward<Functor>(functor)(comunitnd);
  }
  else if (auto appnd = std::dynamic_pointer_cast<ApplicationNode>(node)) {
    return std::forward<Functor>(functor)(appnd);
  }
  else if (auto defnd = std::dynamic_pointer_cast<DefNode>(node)) {
    return std::forward<Functor>(functor)(defnd);
  }
  else if (auto dictnd = std::dynamic_pointer_cast<DictNode>(node)) {
    return std::forward<Functor>(functor)(dictnd);
  }
  else if (auto funcdefnd = std::dynamic_pointer_cast<FuncDefNode>(node)) {
    return std::forward<Functor>(functor)(funcdefnd);
  }
  else if (auto functionnd = std::dynamic_pointer_cast<FunctionNode>(node)) {
    return std::forward<Functor>(functor)(functionnd);
  }
  else if (auto ifnd = std::dynamic_pointer_cast<IfNode>(node)) {
    return std::forward<Functor>(functor)(ifnd);
  }
  else if (auto intnd = std::dynamic_pointer_cast<IntNode>(node)) {
    return std::forward<Functor>(functor)(intnd);
  }
  else if (auto keyargnd = std::dynamic_pointer_cast<KeyargNode>(node)) {
    return std::forward<Functor>(functor)(keyargnd);
  }
  else if (auto keywnd = std::dynamic_pointer_cast<KeywordNode>(node)) {
    return std::forward<Functor>(functor)(keywnd);
  }
  else if (auto letnd = std::dynamic_pointer_cast<LetNode>(node)) {
    return std::forward<Functor>(functor)(letnd);
  }
  else if (auto matchnd = std::dynamic_pointer_cast<MatchNode>(node)) {
    return std::forward<Functor>(functor)(matchnd);
  }
  else if (auto unarynd = std::dynamic_pointer_cast<UnaryNode>(node)) {
    return std::forward<Functor>(functor)(unarynd);
  }
  else if (auto paramnd = std::dynamic_pointer_cast<ParamNode>(node)) {
    return std::forward<Functor>(functor)(paramnd);
  }
  else if (auto rangend = std::dynamic_pointer_cast<RangeNode>(node)) {
    return std::forward<Functor>(functor)(rangend);
  }
  else if (auto ratnd = std::dynamic_pointer_cast<RationalNode>(node)) {
    return std::forward<Functor>(functor)(ratnd);
  }
  else if (auto realnd = std::dynamic_pointer_cast<RealNode>(node)) {
    return std::forward<Functor>(functor)(realnd);
  }
  else if (auto selnd = std::dynamic_pointer_cast<SelectNode>(node)) {
    return std::forward<Functor>(functor)(selnd);
  }
  else if (auto slotdefnd = std::dynamic_pointer_cast<SlotdefNode>(node)) {
    return std::forward<Functor>(functor)(slotdefnd);
  }
  else if (auto slotrefnd = std::dynamic_pointer_cast<SlotRefNode>(node)) {
    return std::forward<Functor>(functor)(slotrefnd);
  }
  else if (auto strnd = std::dynamic_pointer_cast<StringNode>(node)) {
    return std::forward<Functor>(functor)(strnd);
  }
  else if (auto symbolnd = std::dynamic_pointer_cast<SymbolNode>(node)) {
    return std::forward<Functor>(functor)(symbolnd);
  }
  else if (auto typedefnd = std::dynamic_pointer_cast<TypeDefNode>(node)) {
    return std::forward<Functor>(functor)(typedefnd);
  }
  else if (auto typend = std::dynamic_pointer_cast<TypeNode>(node)) {
    return std::forward<Functor>(functor)(typend);
  }
  else if (auto vectornd = std::dynamic_pointer_cast<VectorNode>(node)) {
    return std::forward<Functor>(functor)(vectornd);
  }
  else if (auto whilend = std::dynamic_pointer_cast<WhileNode>(node)) {
    return std::forward<Functor>(functor)(whilend);
  }
  else if (auto castnd = std::dynamic_pointer_cast<CastNode>(node)) {
    return std::forward<Functor>(functor)(castnd);
  }
  else if (auto undefnd = std::dynamic_pointer_cast<UndefNode>(node)) {
    return std::forward<Functor>(functor)(undefnd);
  }
  else if (auto vardefnd = std::dynamic_pointer_cast<VardefNode>(node)) {
    return std::forward<Functor>(functor)(vardefnd);
  }
  else if (auto weaknd = std::dynamic_pointer_cast<WeakNode>(node)) {
    return std::forward<Functor>(functor)(weaknd);
  }

  return Ret();
}


template <typename T>
struct NodeTraversator {
  template <typename Delegate>
  static void traverse(Traversator<Delegate>* trv, T node)
  {
    // nop
  }
};


template <>
struct NodeTraversator<std::shared_ptr<CompileUnitNode>> {
  template <typename Delegate>
  static void traverse(Traversator<Delegate>* trv, std::shared_ptr<CompileUnitNode> node)
  {
    trv->traverseNodeList(node->children());
  }
};


template <>
struct NodeTraversator<std::shared_ptr<ApplicationNode>> {
  template <typename Delegate>
  static void traverse(Traversator<Delegate>* trv, std::shared_ptr<ApplicationNode> node)
  {
    trv->traverseNodeList(node->children());
  }
};


template <>
struct NodeTraversator<std::shared_ptr<ArrayTypeNode>> {
  template <typename Delegate>
  static void traverse(Traversator<Delegate>* trv, std::shared_ptr<ArrayTypeNode> node)
  {
    trv->traverseNode(node->typeNode());
  }
};


template <>
struct NodeTraversator<std::shared_ptr<DefNode>> {
  template <typename Delegate>
  static void traverse(Traversator<Delegate>* trv, std::shared_ptr<DefNode> node)
  {
    trv->traverseNode(node->defNode());
  }
};


template <>
struct NodeTraversator<std::shared_ptr<LetNode>> {
  template <typename Delegate>
  static void traverse(Traversator<Delegate>* trv, std::shared_ptr<LetNode> node)
  {
    trv->traverseNode(node->defNode());
  }
};


template <>
struct NodeTraversator<std::shared_ptr<VardefNode>> {
  template <typename Delegate>
  static void traverse(Traversator<Delegate>* trv, std::shared_ptr<VardefNode> node)
  {
    if (node->initExpr())
      trv->traverseNode(node->initExpr());
  }
};


template <>
struct NodeTraversator<std::shared_ptr<FuncDefNode>> {
  template <typename Delegate>
  static void traverse(Traversator<Delegate>* trv, std::shared_ptr<FuncDefNode> node)
  {
    trv->traverseNodeList(node->params());
    if (node->body())
      trv->traverseNode(node->body());
  }
};


template <>
struct NodeTraversator<std::shared_ptr<FunctionNode>> {
  template <typename Delegate>
  static void traverse(Traversator<Delegate>* trv, std::shared_ptr<FunctionNode> node)
  {
    trv->traverseNodeList(node->params());
    if (node->body())
      trv->traverseNode(node->body());
  }
};


template <>
struct NodeTraversator<std::shared_ptr<SlotdefNode>> {
  template <typename Delegate>
  static void traverse(Traversator<Delegate>* trv, std::shared_ptr<SlotdefNode> node)
  {
    if (node->initExpr())
      trv->traverseNode(node->initExpr());
  }
};


template <>
struct NodeTraversator<std::shared_ptr<BlockNode>> {
  template <typename Delegate>
  static void traverse(Traversator<Delegate>* trv, std::shared_ptr<BlockNode> node)
  {
    trv->traverseNodeList(node->children());
  }
};


template <>
struct NodeTraversator<std::shared_ptr<ParamNode>> {
  template <typename Delegate>
  static void traverse(Traversator<Delegate>* trv, std::shared_ptr<ParamNode> node)
  {
    if (node->initExpr())
      trv->traverseNode(node->initExpr());
  }
};


template <>
struct NodeTraversator<std::shared_ptr<ApplyNode>> {
  template <typename Delegate>
  static void traverse(Traversator<Delegate>* trv, std::shared_ptr<ApplyNode> node)
  {
    trv->traverseNode(node->base());
    trv->traverseNodeList(node->children());
  }
};


template <>
struct NodeTraversator<std::shared_ptr<ArrayNode>> {
  template <typename Delegate>
  static void traverse(Traversator<Delegate>* trv, std::shared_ptr<ArrayNode> node)
  {
    trv->traverseNodeList(node->children());
  }
};


template <>
struct NodeTraversator<std::shared_ptr<AssignNode>> {
  template <typename Delegate>
  static void traverse(Traversator<Delegate>* trv, std::shared_ptr<AssignNode> node)
  {
    trv->traverseNode(node->lvalue());
    trv->traverseNode(node->rvalue());
  }
};


template <>
struct NodeTraversator<std::shared_ptr<BinaryNode>> {
  template <typename Delegate>
  static void traverse(Traversator<Delegate>* trv, std::shared_ptr<BinaryNode> node)
  {
    trv->traverseNode(node->left());
    trv->traverseNode(node->right());
  }
};


template <>
struct NodeTraversator<std::shared_ptr<UnaryNode>> {
  template <typename Delegate>
  static void traverse(Traversator<Delegate>* trv, std::shared_ptr<UnaryNode> node)
  {
    trv->traverseNode(node->base());
  }
};


template <>
struct NodeTraversator<std::shared_ptr<IfNode>> {
  template <typename Delegate>
  static void traverse(Traversator<Delegate>* trv, std::shared_ptr<IfNode> node)
  {
    trv->traverseNode(node->test());
    trv->traverseNode(node->consequent());
    if (node->alternate())
      trv->traverseNode(node->alternate());
  }
};


template <>
struct NodeTraversator<std::shared_ptr<KeyargNode>> {
  template <typename Delegate>
  static void traverse(Traversator<Delegate>* trv, std::shared_ptr<KeyargNode> node)
  {
    trv->traverseNode(node->value());
  }
};


template <>
struct NodeTraversator<std::shared_ptr<MatchNode>> {
  template <typename Delegate>
  static void traverse(Traversator<Delegate>* trv, std::shared_ptr<MatchNode> node)
  {
    trv->traverseNode(node->expr());
    for (size_t i = 0; i < node->mappingCount(); i++) {
      trv->traverseNode(node->mappingAt(i).fConsequent);
    }
  }
};


template <>
struct NodeTraversator<std::shared_ptr<SelectNode>> {
  template <typename Delegate>
  static void traverse(Traversator<Delegate>* trv, std::shared_ptr<SelectNode> node)
  {
    trv->traverseNode(node->test());
    if (node->comparator())
      trv->traverseNode(node->comparator());

    for (size_t i = 0; i < node->mappingCount(); i++) {
      if (node->mappingAt(i).fTestValues.empty()) {
        trv->traverseNode(node->mappingAt(i).fConsequent);
      }
      else {
        for (size_t j = 0; j < node->mappingAt(i).fTestValues.size(); j++)
          trv->traverseNode(node->mappingAt(i).fTestValues[j]);
      }
      trv->traverseNode(node->mappingAt(i).fConsequent);
    }
  }
};


template <>
struct NodeTraversator<std::shared_ptr<RangeNode>> {
  template <typename Delegate>
  static void traverse(Traversator<Delegate>* trv, std::shared_ptr<RangeNode> node)
  {
    trv->traverseNode(node->from());
    trv->traverseNode(node->to());
    if (node->by())
      trv->traverseNode(node->by());
  }
};


template <>
struct NodeTraversator<std::shared_ptr<WhileNode>> {
  template <typename Delegate>
  static void traverse(Traversator<Delegate>* trv, std::shared_ptr<WhileNode> node)
  {
    trv->traverseNode(node->test());
    trv->traverseNode(node->body());
  }
};


template <>
struct NodeTraversator<std::shared_ptr<VectorNode>> {
  template <typename Delegate>
  static void traverse(Traversator<Delegate>* trv, std::shared_ptr<VectorNode> node)
  {
    trv->traverseNodeList(node->children());
  }
};


template <>
struct NodeTraversator<std::shared_ptr<DictNode>> {
  template <typename Delegate>
  static void traverse(Traversator<Delegate>* trv, std::shared_ptr<DictNode> node)
  {
    trv->traverseNodeList(node->children());
  }
};


template <>
struct NodeTraversator<std::shared_ptr<CastNode>> {
  template <typename Delegate>
  static void traverse(Traversator<Delegate>* trv, std::shared_ptr<CastNode> node)
  {
    trv->traverseNode(node->base());
  }
};


template <>
struct NodeTraversator<std::shared_ptr<SlotRefNode>> {
  template <typename Delegate>
  static void traverse(Traversator<Delegate>* trv, std::shared_ptr<SlotRefNode> node)
  {
    trv->traverseNode(node->base());
  }
};


template <typename Delegate>
inline void Traversator<Delegate>::traverseNode(std::shared_ptr<AstNode> node)
{
  if (apply(node, TraversePhase::before)) {
    dispatchNode<void>(
        node, [&](auto nd) { NodeTraversator<decltype(nd)>::traverse(this, nd); });
    apply(node, TraversePhase::after);
  }
}


template <typename Delegate>
inline void Traversator<Delegate>::traverseNodeList(NodeList& nl)
{
  for (auto& nd : nl)
    traverseNode(nd);
}


template <typename Delegate>
bool Traversator<Delegate>::apply(std::shared_ptr<AstNode> node, TraversePhase phase)
{
  return dispatchNode<bool>(node, [&](auto nd) { return fDelegate.apply(nd, phase); });
}


}  // namespace herschel
