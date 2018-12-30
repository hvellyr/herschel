/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.

   - look up all name references and complete their namespaces
 */

#include "annotate.hpp"

#include "ast.hpp"
#include "compiler.hpp"
#include "errcodes.hpp"
#include "log.hpp"
#include "predefined.hpp"
#include "properties.hpp"
#include "rootscope.hpp"
#include "scope.hpp"
#include "symbol.hpp"
#include "traverse.hpp"

#include <algorithm>
#include <iterator>
#include <typeinfo>  //for 'typeid' to work


namespace herschel {


template <typename T>
struct NodeAnnotator {
  static void annotate(Annotator* ann, T node)
  {
    // Nothing to annotate here
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<CompileUnitNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<CompileUnitNode> node)
  {
    ann->annotateNodeList(node->children(), !K(markTailPos), !K(markSingleType));
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<ArrayTypeNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<ArrayTypeNode> node)
  {
    ann->annotateNode(node->typeNode());
  }
};


static void annotateVardefNode(Annotator* ann, std::shared_ptr<VardefNode> node,
                               bool isLocal)
{
  if (isLocal) {
    if (!ann->fScope->checkForRedefinition(node->srcpos(), Scope::kNormal, node->name()))
      ann->fScope->registerVar(node->srcpos(), node->name(), node);
  }

  if (node->initExpr()) {
    ann->annotateNode(node->initExpr());
  }
}


static void annotateFuncdefNode(Annotator* ann, std::shared_ptr<FuncDefNode> node,
                                bool isLocal)
{
  if (isLocal) {
    ann->fScope->registerFunction(node->srcpos(), node->name(), node);
  }
  else if (node->isMethod()) {
    auto var = node->scope()->lookupVarOrFunc(node->srcpos(), node->name(),
                                              K(showAmbiguousSymDef));
    if (!var) {
      errorf(node->srcpos(), E_NoGenericFunction,
             "No generic function definition found for method");
    }
    else if (auto funcdef = dynamic_cast<const FuncDefNode*>(var)) {
      if (!funcdef->isGeneric()) {
        errorf(node->srcpos(), E_BadGenericReferrer,
               "Bad method binding type (referred symbol is not a generic function).");
        errorf(var->srcpos(), E_BadGenericReferrer,
               "Referred function definition was here");
      }
    }
    else {
      errorf(node->srcpos(), E_BadGenericReferrer,
             "Bad method binding type (referred symbol is not a generic function).");
      errorf(var->srcpos(), E_BadGenericReferrer, "Referred symbol definition was here");
    }
  }

  ScopeHelper scopeHelper(ann->fScope, !K(doExport), K(isInnerScope), kScopeL_Function);

  ann->annotateNodeList(node->params(), !K(markTailPos), K(markSingleType));
  if (node->body()) {
    node->body()->setIsInTailPos(true);
    ann->annotateNode(node->body());
  }
}


template <>
struct NodeAnnotator<std::shared_ptr<DefNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<DefNode> node)
  {
    auto vardefNode = std::dynamic_pointer_cast<VardefNode>(node->defNode());
    if (vardefNode) {
      vardefNode->setScope(ann->fScope);
      annotateVardefNode(ann, vardefNode, !K(isLocal));
      return;
    }

    auto funcNode = std::dynamic_pointer_cast<FuncDefNode>(node->defNode());
    if (funcNode) {
      funcNode->setScope(ann->fScope);
      annotateFuncdefNode(ann, funcNode, !K(isLocal));
      return;
    }

    ann->annotateNode(node->defNode());
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<LetNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<LetNode> node)
  {
    auto vardefNode = std::dynamic_pointer_cast<VardefNode>(node->defNode());
    if (vardefNode) {
      vardefNode->setScope(ann->fScope);
      annotateVardefNode(ann, vardefNode, K(isLocal));
      return;
    }

    auto funcNode = std::dynamic_pointer_cast<FuncDefNode>(node->defNode());
    if (funcNode) {
      funcNode->setScope(ann->fScope);
      annotateFuncdefNode(ann, funcNode, K(isLocal));
      return;
    }

    ann->annotateNode(node->defNode());
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<FunctionNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<FunctionNode> node)
  {
    ScopeHelper scopeHelper(ann->fScope, !K(doExport), !K(isInnerScope),
                            kScopeL_Function);

    ann->annotateNodeList(node->params(), !K(markTailPos), K(markSingleType));
    if (node->body()) {
      node->body()->setIsInTailPos(true);
      ann->annotateNode(node->body());
    }
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<BlockNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<BlockNode> node)
  {
    ScopeHelper scopeHelper(ann->fScope, !K(doExport), K(isInnerScope), kScopeL_Local);
    ann->annotateNodeList(node->children(), node->isInTailPos(), !K(markSingleType));
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<ParamNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<ParamNode> node)
  {
    if (!ann->fScope->checkForRedefinition(node->srcpos(), Scope::kNormal, node->name()))
      ann->fScope->registerVar(node->srcpos(), node->name(), node);

    if (node->initExpr()) {
      ann->annotateNode(node->initExpr());
    }
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<ApplyNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<ApplyNode> node)
  {
    ann->annotateNode(node->base());
    ann->annotateNodeList(node->children(), !K(markTailPos), K(markSingleType));
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<WeakNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<WeakNode> node)
  {
    if (node->refNode())
      ann->annotateNode(node->refNode());
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<ArrayNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<ArrayNode> node)
  {
    ann->annotateNodeList(node->children(), !K(markTailPos), K(markSingleType));
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<AssignNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<AssignNode> node)
  {
    ann->annotateNode(node->lvalue());
    ann->annotateNode(node->rvalue());
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<BinaryNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<BinaryNode> node)
  {
    ann->annotateNode(node->left());
    ann->annotateNode(node->right());
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<UnaryNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<UnaryNode> node)
  {
    ann->annotateNode(node->base());
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<IfNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<IfNode> node)
  {
    ann->annotateNode(node->test());

    node->consequent()->setIsInTailPos(node->isInTailPos());
    ann->annotateNode(node->consequent());
    if (node->alternate()) {
      node->alternate()->setIsInTailPos(node->isInTailPos());
      ann->annotateNode(node->alternate());
    }
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<KeyargNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<KeyargNode> node)
  {
    ann->annotateNode(node->value());
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<SelectNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<SelectNode> node)
  {
    // TODO : set tail node position
    ann->annotateNode(node->test());
    if (node->comparator())
      ann->annotateNode(node->comparator());

    for (size_t i = 0; i < node->mappingCount(); i++) {
      if (node->mappingAt(i).fTestValues.empty()) {
        ann->annotateNode(node->mappingAt(i).fConsequent);
      }
      else {
        for (auto& testValue : node->mappingAt(i).fTestValues)
          ann->annotateNode(testValue);
      }
      ann->annotateNode(node->mappingAt(i).fConsequent);
    }
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<RangeNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<RangeNode> node)
  {
    // TODO : set tail node position
    ann->annotateNode(node->from());
    ann->annotateNode(node->to());
    if (node->by())
      ann->annotateNode(node->by());
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<TypeDefNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<TypeDefNode> node)
  {
    // TODO : set tail node position

    // don't re-register the type if global; it is registered in pass2 already
    // ann->fScope->registerType(node->srcpos(), node->name(), node->defType());
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<WhileNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<WhileNode> node)
  {
    ann->annotateNode(node->test());

    node->body()->setIsInTailPos(node->isInTailPos());
    ann->annotateNode(node->body());
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<VectorNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<VectorNode> node)
  {
    ann->annotateNodeList(node->children(), !K(markTailPos), K(markSingleType));
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<DictNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<DictNode> node)
  {
    ann->annotateNodeList(node->children(), !K(markTailPos), K(markSingleType));
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<CastNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<CastNode> node)
  {
    ann->annotateNode(node->base());
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<SlotRefNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<SlotRefNode> node)
  {
    ann->annotateNode(node->base());
  }
};


//----------------------------------------------------------------------------

AnnotatePass::AnnotatePass(int level, std::shared_ptr<Scope> scope, Compiler& compiler)
    : AstNodeCompilePass(level)
    , fScope(std::move(scope))
    , fCompiler(compiler)
{
}


std::shared_ptr<AstNode> AnnotatePass::doApply(std::shared_ptr<AstNode> src)
{
  Annotator an{ fScope, fCompiler };
  an.annotateNode(src);
  return src;
}


//----------------------------------------------------------------------------

Annotator::Annotator(std::shared_ptr<Scope> scope, Compiler& compiler)
    : fScope(std::move(scope))
    , fCompiler(compiler)
{
}


void Annotator::annotateNode(std::shared_ptr<AstNode> node)
{
  node->setScope(fScope);

  dispatchNode<void>(node,
                     [&](auto nd) { NodeAnnotator<decltype(nd)>::annotate(this, nd); });
}


void Annotator::annotateNodeList(NodeList& nl, bool marktailpos, bool marksingletype)
{
  const size_t nlsize = nl.size();

  for (size_t i = 0; i < nlsize; i++) {
    if (marktailpos && i == nlsize - 1)
      nl[i]->setIsInTailPos(true);

    if (marksingletype)
      nl[i]->setIsSingleTypeRequired(true);

    annotateNode(nl[i]);
  }
}

}  // namespace herschel
