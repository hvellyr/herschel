/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.

   - look up all name references and complete their namespaces
*/

#include "typecheck.hpp"

#include "ast.hpp"
#include "errcodes.hpp"
#include "log.hpp"
#include "port.hpp"
#include "predefined.hpp"
#include "properties.hpp"
#include "scope.hpp"
#include "str.hpp"
#include "symbol.hpp"
#include "traverse.hpp"
#include "typectx.hpp"
#include "utils.hpp"
#include "xmlrenderer.hpp"

#include <set>


namespace herschel {

template <typename T>
struct NodeTypeChecker {
  static void check(TypeChecker* typf, T node)
  {
    typf->checkNodeList(node->child_nodes());
  }
};


template <>
struct NodeTypeChecker<std::shared_ptr<VardefNode>> {
  static void check(TypeChecker* typf, std::shared_ptr<VardefNode> node)
  {
    typf->checkNodeList(node->child_nodes());

    hr_assert(!node->isTypeSpecDelayed());
  }
};


template <>
struct NodeTypeChecker<std::shared_ptr<FuncDefNode>> {
  static void check(TypeChecker* typf, std::shared_ptr<FuncDefNode> node)
  {
    typf->checkNodeList(node->child_nodes());

    typf->checkFunctionReturnType(node);

    if (node->isMethod()) {
      // for methods check that the function signature matches the generic
      // function implementation.  The following conditions are checked in
      // annotate.cpp already.
      const FuncDefNode* genericDef;
      const AstNode* var = node->scope()->lookupVarOrFunc(node->srcpos(), node->name(),
                                                          K(showAmbiguousSymDef));
      if (var && (genericDef = dynamic_cast<const FuncDefNode*>(var)) &&
          genericDef->isGeneric()) {
        if (!isContravariant(genericDef->type(), node->type(), *node->scope(),
                             node->srcpos())) {
          // tyerror(genericDef->type(), "genericdef type");
          // tyerror(node->type(), "node type");

          errorf(node->srcpos(), E_TypeMismatch,
                 "method does not match generic function definition");
          // tyerror(genericDef->type(), "Generic");
          // tyerror(node->type(), "This");
        }
      }
    }
  }
};


template <>
struct NodeTypeChecker<std::shared_ptr<FunctionNode>> {
  static void check(TypeChecker* typf, std::shared_ptr<FunctionNode> node)
  {
    typf->checkNodeList(node->child_nodes());

    typf->checkFunctionReturnType(node);
  }
};


template <>
struct NodeTypeChecker<std::shared_ptr<BlockNode>> {
  static void check(TypeChecker* typf, std::shared_ptr<BlockNode> node)
  {
    hr_assert(!node->children().empty());
    typf->checkNodeList(node->child_nodes());
  }
};


template <>
struct NodeTypeChecker<std::shared_ptr<AssignNode>> {
  static void check(TypeChecker* typf, std::shared_ptr<AssignNode> node)
  {
    //    typf->checkNodeList(node->child_nodes());
    typf->checkNode(node->rvalue());

    if (node->isTypeSpecDelayed()) {
      auto symNode = std::dynamic_pointer_cast<SymbolNode>(node->lvalue());
      hr_assert(symNode);

      auto var = node->scope()->lookupVarOrFunc(symNode->srcpos(), symNode->name(),
                                                K(showAmbiguousSymDef));
      auto vardefNode = const_cast<VardefNode*>(dynamic_cast<const VardefNode*>(var));
      hr_assert(vardefNode);

      symNode->setType(node->rvalue()->type());
      vardefNode->setType(node->rvalue()->type());

      node->setTypeSpecDelayed(false);
      vardefNode->setTypeSpecDelayed(false);
    }
    else
      typf->checkNode(node->lvalue());
  }
};


template <>
struct NodeTypeChecker<std::shared_ptr<IfNode>> {
  static void check(TypeChecker* typf, std::shared_ptr<IfNode> node)
  {
    typf->checkNodeList(node->child_nodes());

    // TODO
    // if (!isSameType(Type::makeBool(true), node->test()->type())) {
    //   errorf(node->test(), E_BoolTypeExpected,
    //          "Bool type in if test expected");
    // }
  }
};


//------------------------------------------------------------------------------

TypeChecker::TypeChecker() {}


void TypeChecker::checkNode(std::shared_ptr<AstNode> node)
{
  dispatchNode<void>(node,
                     [&](auto nd) { NodeTypeChecker<decltype(nd)>::check(this, nd); });
}


void TypeChecker::checkNodeList(const NodeList& nl)
{
  for (size_t i = 0; i < nl.size(); i++)
    checkNode(nl[i]);
}


//------------------------------------------------------------------------------

class FindReturnTraverseDelegate {
public:
  bool apply(std::shared_ptr<AstNode> node, TraversePhase phase)
  {
    if (phase == TraversePhase::before) {
      // don't dive into nested functions
      if (std::dynamic_pointer_cast<FunctionNode>(node))
        return false;

      auto apply = std::dynamic_pointer_cast<ApplyNode>(node);
      if (apply && apply->isSimpleCall() && apply->simpleCallName() == Names::kLangReturn)
        fReturns.push_back(apply);
    }
    return true;
  }

  std::vector<std::shared_ptr<AstNode>> fReturns;
};


void TypeChecker::checkFunctionReturnType(std::shared_ptr<FunctionNode> node)
{
  if (node->body()) {
    if (!isContravariant(node->retType(), node->body()->type(), *node->scope(),
                         node->srcpos()) &&
        !containsAny(node->body()->type(), node->srcpos())) {
      errorf(node->srcpos(), E_TypeMismatch,
             "function's body type does not match its return type");
    }


    FindReturnTraverseDelegate delegate;
    Traversator<FindReturnTraverseDelegate>(delegate).traverseNode(node);

    for (auto ret : delegate.fReturns) {
      if (!isContravariant(node->retType(), ret->type(), *ret->scope(), ret->srcpos())) {
        errorf(ret->srcpos(), E_TypeMismatch,
               "return's type does not match outer block type");
      }
    }
  }
}


//------------------------------------------------------------------------------

TypeCheckPass::TypeCheckPass(int level)
    : AstNodeCompilePass(level, K(showNodeType))
{
}


std::shared_ptr<AstNode> TypeCheckPass::doApply(std::shared_ptr<AstNode> src)
{
  auto ty = TypeChecker{};
  ty.checkNode(src);
  return src;
}

}  // namespace herschel
