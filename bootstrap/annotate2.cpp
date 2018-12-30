/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
 */

#include "annotate2.hpp"

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
struct NodeAnnotator2 {
  static void annotate(Annotator2* ann, T node)
  {
    ann->annotateNodeList(node->child_nodes());
  }
};


static void takeFullNameFromNode(SymbolNode* node, const AstNode* otherNode)
{
  auto nn = dynamic_cast<const NamedNode*>(otherNode);
  if (nn) {
    node->setName(nn->name());
    return;
  }

  // TODO: unexpected type here.
  logf(kError, "Unexpected type here: %s", typeid(*otherNode).name());
  hr_invalid("Unexpected type");
}


static bool updateAllocType(SymbolNode* usingNode, const AstNode* referedNode)
{
  if (usingNode->scope()->isVarInOuterFunction(usingNode->name())) {
    auto bindNode = dynamic_cast<const BindingNode*>(referedNode);
    hr_assert(bindNode);

    const_cast<BindingNode*>(bindNode)->setAllocType(kAlloc_Shared);
    return true;
  }
  return false;
}


template <>
struct NodeAnnotator2<std::shared_ptr<SymbolNode>> {
  static void annotate(Annotator2* ann, std::shared_ptr<SymbolNode> node)
  {
    const AstNode* var =
        node->scope()->lookupVarOrFunc(node->srcpos(), node->name(), K(showAmbiguousSymDef));
    if (var) {
      takeFullNameFromNode(node.get(), var);

      auto vardef = dynamic_cast<const VardefNode*>(var);
      if (vardef) {
        bool isShared = updateAllocType(node.get(), vardef);
        node->setRefersTo(vardef->isLocal() ? kLocalVar : kGlobalVar, isShared);
      }
      else if (auto funcdef = dynamic_cast<const FuncDefNode*>(var)) {
        if (funcdef->isGeneric())
          node->setRefersTo(kGeneric, !K(isShared));
        else
          node->setRefersTo(kFunction, !K(isShared));

        // keep an additional link to this function (which is obviously
        // referenced), such that the codegen can produce extern declaration
        // for it if needed
        SrcPos srcpos;
        if (!ann->fCompiler.referredFunctionCache()->hasName(Scope::kNormal, node->name(),
                                                             &srcpos)) {
          ann->fCompiler.referredFunctionCache()->registerFunction(
              funcdef->srcpos(), node->name(), funcdef->clone());
        }
      }
      else if (dynamic_cast<const ParamNode*>(var)) {
        bool isShared = updateAllocType(node.get(), var);
        node->setRefersTo(kParam, isShared);
      }
      else if (dynamic_cast<const SlotdefNode*>(var)) {
        bool isShared = updateAllocType(node.get(), var);
        node->setRefersTo(kSlot, isShared);
      }
      else {
        hr_invalid("unhandled registered symbol def");
      }

      return;
    }

    Type type = node->scope()->lookupType(node->name(), K(showAmbiguousSymDef));
    if (type.isDef()) {
      node->setName(type.typeName());
      return;
    }

#if defined(UNITTESTS)
    if (!node->isRemoveable() && Properties::test_passLevel() > 2) {
      errorf(node->srcpos(), E_UndefinedVar, "Unknown symbol '%s'",
             (zstring)StrHelper(node->name()));
      // node->scope()->dumpDebug(true);
    }
#endif
  }
};


template <>
struct NodeAnnotator2<std::shared_ptr<FuncDefNode>> {
  static void annotate(Annotator2* ann, std::shared_ptr<FuncDefNode> node)
  {
    ScopeHelper scopeHelper(ann->fScope, !K(doExport), K(isInnerScope), kScopeL_Function);

    ann->annotateNodeList(node->child_nodes());
  }
};


template <>
struct NodeAnnotator2<std::shared_ptr<FunctionNode>> {
  static void annotate(Annotator2* ann, std::shared_ptr<FunctionNode> node)
  {
    ScopeHelper scopeHelper(ann->fScope, !K(doExport), !K(isInnerScope),
                            kScopeL_Function);

    ann->annotateNodeList(node->child_nodes());
  }
};


template <>
struct NodeAnnotator2<std::shared_ptr<SlotdefNode>> {
  static void annotate(Annotator2* ann, std::shared_ptr<SlotdefNode> node)
  {
    // TODO
    // if (!ann->fScope->checkForRedefinition(node->srcpos(),
    //                                        Scope::kNormal, node->name()))
    //   ann->fScope->registerVar(node->srcpos(), node->name(), node);
  }
};


template <>
struct NodeAnnotator2<std::shared_ptr<BlockNode>> {
  static void annotate(Annotator2* ann, std::shared_ptr<BlockNode> node)
  {
    ScopeHelper scopeHelper(ann->fScope, !K(doExport), K(isInnerScope), kScopeL_Local);

    ann->annotateNodeList(node->child_nodes());
  }
};


template <>
struct NodeAnnotator2<std::shared_ptr<ApplyNode>> {
  static void annotate(Annotator2* ann, std::shared_ptr<ApplyNode> node)
  {
    ScopeHelper scopeHelper(ann->fScope, !K(doExport), K(isInnerScope), kScopeL_Local);

    ann->annotateNodeList(node->child_nodes());

    if (std::shared_ptr<FunctionNode> refFuncNd = node->refFunction().lock()) {
      node->base()->setType(refFuncNd->type());
    }
  }
};


//----------------------------------------------------------------------------

Annotate2Pass::Annotate2Pass(int level, std::shared_ptr<Scope> scope, Compiler& compiler)
    : AstNodeCompilePass(level)
    , fScope(std::move(scope))
    , fCompiler(compiler)
{
}


std::shared_ptr<AstNode> Annotate2Pass::doApply(std::shared_ptr<AstNode> src)
{
  Annotator2 an{ fScope, fCompiler };
  an.annotateNode(src);
  return src;
}


//----------------------------------------------------------------------------

Annotator2::Annotator2(std::shared_ptr<Scope> scope, Compiler& compiler)
    : fScope(std::move(scope))
    , fCompiler(compiler)
{
}


void Annotator2::annotateNode(std::shared_ptr<AstNode> node)
{
  dispatchNode<void>(node,
                     [&](auto nd) { NodeAnnotator2<decltype(nd)>::annotate(this, nd); });
}


void Annotator2::annotateNodeList(const NodeList& nl)
{
  const size_t nlsize = nl.size();

  for (size_t i = 0; i < nlsize; i++) {
    annotateNode(nl[i]);
  }
}

}  // namespace herschel
