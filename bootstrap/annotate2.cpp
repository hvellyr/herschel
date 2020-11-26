/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
 */

#include "annotate2.hpp"

#include "annotate.hpp"
#include "compiler.hpp"
#include "errcodes.hpp"
#include "log.hpp"
#include "predefined.hpp"
#include "properties.hpp"
#include "rootscope.hpp"
#include "scope.hpp"
#include "symbol.hpp"
#include "traverse.hpp"
#include "typify.hpp"
#include "xmlrenderer.hpp"

#include <algorithm>
#include <iterator>
#include <typeinfo>  //for 'typeid' to work


namespace herschel {


namespace {
  bool isMoveable(std::shared_ptr<AstNode> valExpr)
  {
    if (auto symbnd = std::dynamic_pointer_cast<SymbolNode>(valExpr)) {
      return symbnd->isInMovePos();
    }
    else if (auto applynd = std::dynamic_pointer_cast<ApplyNode>(valExpr)) {
      return true;
    }
    return false;
  }

  std::shared_ptr<AstNode> wrapAsCopy(Compiler& compiler,
                                      std::shared_ptr<AstNode> valExpr)
  {
    auto wrapNode = [&](std::shared_ptr<AstNode> expr) {
      auto scope = expr->scope();
      auto srcpos = expr->srcpos();

      ScopeHelper scopeHelper(scope, !K(doExport), K(isInnerScope), !K(doPropIntern),
                              kScopeL_Local);
      auto block = makeBlockNode(scope, srcpos);
      auto localVarSym = uniqueName("cpy");
      auto localVar = makeVardefNode(scope, srcpos, localVarSym, kNormalVar, K(isLocal),
                                     expr->type(), nullptr);
      scope->registerVar(srcpos, localVarSym, localVar);
      auto localVarNd = makeLetNode(scope, localVar);
      block->appendNode(localVarNd);

      auto symNd = makeSymbolNode(scope, srcpos, Names::kCopyFuncName);
      symNd->setRefersTo(kFunction, !K(isShared));

      auto copyExpr = makeApplyNode(expr->scope(), expr->srcpos(), symNd);
      copyExpr->appendNode(makeSymbolNode(scope, srcpos, localVarSym));
      copyExpr->appendNode(expr);
      copyExpr->setType(expr->type());
      block->appendNode(copyExpr);

      block->appendNode(makeSymbolNode(scope, srcpos, localVarSym));
      block->markReturnNode(scope);

      std::shared_ptr<AstNode> nd =
          makeScopeNode(scope, srcpos, block, !K(doExport), K(isInnerScope),
                        !K(doPropIntern), kScopeL_Local);

      //std::shared_ptr<AstNode> nd = copyExpr;

      {
        auto an = Annotator{compiler};
        nd = an.annotateNode(nd);
      }
      {
        auto ty = Typifier{compiler};
        nd = ty.typifyNode(nd);
      }

      if (auto applyNd = std::dynamic_pointer_cast<ApplyNode>(nd)) {
        if (std::shared_ptr<FunctionNode> refFuncNd = applyNd->refFunction().lock()) {
          applyNd->base()->setType(refFuncNd->type());
        }
      }

      return nd;
    };

    auto tryWrapNode = [&](std::shared_ptr<AstNode> expr) {
      if (!expr->isTempValue() && !isMoveable(expr))
        return wrapNode(expr);
      return expr;
    };

    if (auto keyedParam = std::dynamic_pointer_cast<KeyargNode>(valExpr)) {
      keyedParam->setValue(tryWrapNode(keyedParam->value()));
      return keyedParam;
    }
    else
      return tryWrapNode(valExpr);
  }


  std::shared_ptr<AstNode> wrapAsCopyIf(Compiler& compiler, const Type& destType,
                                        std::shared_ptr<AstNode> expr)
  {
    if (destType.isValueType()) {
      if (destType.isRecord()) {
        return wrapAsCopy(compiler, expr);
      }
      else {
        // TODO: handle ANY/union/etc.
      }
    }

    return expr;
  }

}  // namespace


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
  HR_LOG(kError, node->srcpos()) << "Unexpected type here: " << typeid(*otherNode).name();
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
    const AstNode* var = node->scope()->lookupVarOrFunc(node->srcpos(), node->name(),
                                                        K(showAmbiguousSymDef));
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
      HR_LOG(kError, node->srcpos(), E_UndefinedVar)
          << "Unknown symbol '" << node->name() << "'";
    }
#endif
  }
};


template <>
struct NodeAnnotator2<std::shared_ptr<SlotdefNode>> {
  static void annotate(Annotator2* ann, std::shared_ptr<SlotdefNode> node) {}
};


template <>
struct NodeAnnotator2<std::shared_ptr<ApplyNode>> {
  static void annotate(Annotator2* ann, std::shared_ptr<ApplyNode> node)
  {
    // first annotate base and arguments, only then wrap for copy/move
    // operators.  Otherwise we would annotate the just created
    // copy(x) calls into copy(copy(x)), etc.
    ann->annotateNodeList(node->child_nodes());

    FunctionParamVector params = node->funSign().parameters();
    if (params.size() < node->children().size()) {
      HR_LOG(kError, node->srcpos()) << "Not enough arguments for function";
    }

    if (params.size() >= node->children().size()) {
      // rewrite arguments to copy/move where necessary
      auto& args = node->children();
      for (auto i = 0; i < args.size(); ++i) {
        args[i] = wrapAsCopyIf(ann->fCompiler, params[i].type(), args[i]);
      }
    }

    if (std::shared_ptr<FunctionNode> refFuncNd = node->refFunction().lock()) {
      node->base()->setType(refFuncNd->type());
    }
  }
};


template <>
struct NodeAnnotator2<std::shared_ptr<AssignNode>> {
  static void annotate(Annotator2* ann, std::shared_ptr<AssignNode> node)
  {
    ann->annotateNodeList(node->child_nodes());
    node->setRvalue(wrapAsCopyIf(ann->fCompiler, node->lvalue()->type(), node->rvalue()));
  }
};


template <>
struct NodeAnnotator2<std::shared_ptr<VardefNode>> {
  static void annotate(Annotator2* ann, std::shared_ptr<VardefNode> node)
  {
    ann->annotateNodeList(node->child_nodes());
    if (node->initExpr()) {
      node->setInitExpr(wrapAsCopyIf(ann->fCompiler, node->type(), node->initExpr()));
    }
  }
};


//----------------------------------------------------------------------------

Annotate2Pass::Annotate2Pass(int level, Compiler& compiler)
    : AstNodeCompilePass(level)
    , fCompiler(compiler)
{
}


std::shared_ptr<AstNode> Annotate2Pass::doApply(std::shared_ptr<AstNode> src)
{
  Annotator2 an{fCompiler};
  an.annotateNode(src);
  return src;
}


//----------------------------------------------------------------------------

Annotator2::Annotator2(Compiler& compiler)
    : fCompiler(compiler)
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
