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
  static void annotate(Annotator* ann, T node, Annotator::Phase phase)
  {
    // Nothing to annotate here
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<CompileUnitNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<CompileUnitNode> node,
                       Annotator::Phase phase)
  {
    ann->annotateNodeList(node->children(), !K(markTailPos), !K(markSingleType), phase);
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
struct NodeAnnotator<std::shared_ptr<SymbolNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<SymbolNode> node,
                       Annotator::Phase phase)
  {
    if (phase == Annotator::kLookup) {
      const AstNode* var =
          node->scope()->lookupVarOrFunc(node->name(), K(showAmbiguousSymDef));
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
          if (!ann->fCompiler.referredFunctionCache()->hasName(Scope::kNormal,
                                                               node->name(), &srcpos)) {
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
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<ArrayTypeNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<ArrayTypeNode> node,
                       Annotator::Phase phase)
  {
    ann->annotateNode(node->typeNode(), phase);
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<TypeNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<TypeNode> node,
                       Annotator::Phase phase)
  {
    // TODO
  }
};


static void annotateVardefNode(Annotator* ann, std::shared_ptr<VardefNode> node,
                               Annotator::Phase phase, bool isLocal)
{
  if (phase == Annotator::kRegister) {
    if (isLocal) {
      if (!ann->fScope->checkForRedefinition(node->srcpos(), Scope::kNormal,
                                             node->name()))
        ann->fScope->registerVar(node->srcpos(), node->name(), node);
    }
  }

  if (node->initExpr()) {
    ann->annotateNode(node->initExpr(), phase);
  }
}


static void annotateFuncdefNode(Annotator* ann, std::shared_ptr<FuncDefNode> node,
                                Annotator::Phase phase, bool isLocal)
{
  if (phase == Annotator::kRegister) {
    if (isLocal) {
      ann->fScope->registerFunction(node->srcpos(), node->name(), node);
    }
    else if (node->isMethod()) {
      auto var = node->scope()->lookupVarOrFunc(node->name(), K(showAmbiguousSymDef));
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
        errorf(var->srcpos(), E_BadGenericReferrer,
               "Referred symbol definition was here");
      }
    }
  }

  ScopeHelper scopeHelper(ann->fScope, !K(doExport), K(isInnerScope), kScopeL_Function);

  ann->annotateNodeList(node->params(), !K(markTailPos), K(markSingleType), phase);
  if (node->body()) {
    node->body()->setIsInTailPos(true);
    ann->annotateNode(node->body(), phase);
  }
}


template <>
struct NodeAnnotator<std::shared_ptr<DefNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<DefNode> node,
                       Annotator::Phase phase)
  {
    auto vardefNode = std::dynamic_pointer_cast<VardefNode>(node->defNode());
    if (vardefNode) {
      if (phase == Annotator::kRegister)
        vardefNode->setScope(ann->fScope);
      annotateVardefNode(ann, vardefNode, phase, !K(isLocal));
      return;
    }

    auto funcNode = std::dynamic_pointer_cast<FuncDefNode>(node->defNode());
    if (funcNode) {
      if (phase == Annotator::kRegister)
        funcNode->setScope(ann->fScope);
      annotateFuncdefNode(ann, funcNode, phase, !K(isLocal));
      return;
    }

    ann->annotateNode(node->defNode(), phase);
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<LetNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<LetNode> node,
                       Annotator::Phase phase)
  {
    auto vardefNode = std::dynamic_pointer_cast<VardefNode>(node->defNode());
    if (vardefNode) {
      if (phase == Annotator::kRegister)
        vardefNode->setScope(ann->fScope);
      annotateVardefNode(ann, vardefNode, phase, K(isLocal));
      return;
    }

    auto funcNode = std::dynamic_pointer_cast<FuncDefNode>(node->defNode());
    if (funcNode) {
      if (phase == Annotator::kRegister)
        funcNode->setScope(ann->fScope);
      annotateFuncdefNode(ann, funcNode, phase, K(isLocal));
      return;
    }

    ann->annotateNode(node->defNode(), phase);
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<FunctionNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<FunctionNode> node,
                       Annotator::Phase phase)
  {
    ScopeHelper scopeHelper(ann->fScope, !K(doExport), !K(isInnerScope),
                            kScopeL_Function);

    ann->annotateNodeList(node->params(), !K(markTailPos), K(markSingleType), phase);
    if (node->body()) {
      node->body()->setIsInTailPos(true);
      ann->annotateNode(node->body(), phase);
    }
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<SlotdefNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<SlotdefNode> node,
                       Annotator::Phase phase)
  {
    // TODO
    // if (phase == Annotator::kRegister) {
    //   if (!ann->fScope->checkForRedefinition(node->srcpos(),
    //                                          Scope::kNormal, node->name()))
    //     ann->fScope->registerVar(node->srcpos(), node->name(), node);
    // }
  }
};


class StripLoopNodesTraverseDelegate {
public:
  StripLoopNodesTraverseDelegate(int loopId)
      : fLoopId(loopId)
  {
  }

  bool apply(std::shared_ptr<AstNode> node, TraversePhase phase)
  {
    if (phase == TraversePhase::before) {
      if (auto blockNode = std::dynamic_pointer_cast<BlockNode>(node)) {
        NodeList& nl = blockNode->children();
        for (size_t i = 0; i < nl.size();) {
          if (auto symNode = std::dynamic_pointer_cast<SymbolNode>(nl[i])) {
            if (symNode->loopId() == fLoopId) {
              // replace the return symbol with a simple lang.unspecified.
              // This is required to give the expression a concrete return
              // value.  Otherwise SSA compilation in codegen becomes more
              // complicated.
              nl[i] = makeSymbolNode(nl[i]->srcpos(), Names::kLangUnspecified);
            }
          }
          else if (auto letNode = std::dynamic_pointer_cast<LetNode>(nl[i])) {
            if (letNode->loopId() == fLoopId) {
              nl.erase(nl.begin() + i);
              continue;
            }
          }
          else if (auto assignNode = std::dynamic_pointer_cast<AssignNode>(nl[i])) {
            if (assignNode->loopId() == fLoopId)
              nl[i] = assignNode->rvalue();
          }

          i++;
        }
      }
    }

    return true;
  }

  int fLoopId;
};


template <>
struct NodeAnnotator<std::shared_ptr<BlockNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<BlockNode> node,
                       Annotator::Phase phase)
  {
    ScopeHelper scopeHelper(ann->fScope, !K(doExport), K(isInnerScope), kScopeL_Local);

    if (!node->isInTailPos()) {
      int loopId = 0;

      NodeList& nl = node->children();
      const size_t nlsize = nl.size();
      for (size_t i = 0; i < nlsize; i++) {
        if (auto symNode = std::dynamic_pointer_cast<SymbolNode>(nl[i])) {
          if (symNode->loopId() > 0) {
            loopId = symNode->loopId();
            break;
          }
        }
        else if (auto letNode = std::dynamic_pointer_cast<LetNode>(nl[i])) {
          if (letNode->loopId() > 0) {
            loopId = letNode->loopId();
            break;
          }
        }
      }

      if (loopId > 0) {
        StripLoopNodesTraverseDelegate delegate(loopId);
        Traversator<StripLoopNodesTraverseDelegate>(delegate).traverseNode(node);
      }
    }

    ann->annotateNodeList(node->children(), node->isInTailPos(), !K(markSingleType),
                          phase);
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<ParamNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<ParamNode> node,
                       Annotator::Phase phase)
  {
    if (phase == Annotator::kRegister) {
      if (!ann->fScope->checkForRedefinition(node->srcpos(), Scope::kNormal,
                                             node->name()))
        ann->fScope->registerVar(node->srcpos(), node->name(), node);
    }

    if (node->initExpr()) {
      ann->annotateNode(node->initExpr(), phase);
    }
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<ApplyNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<ApplyNode> node,
                       Annotator::Phase phase)
  {
    ann->annotateNode(node->base(), phase);
    ann->annotateNodeList(node->children(), !K(markTailPos), K(markSingleType), phase);
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<ArrayNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<ArrayNode> node,
                       Annotator::Phase phase)
  {
    ann->annotateNodeList(node->children(), !K(markTailPos), K(markSingleType), phase);
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<AssignNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<AssignNode> node,
                       Annotator::Phase phase)
  {
    ann->annotateNode(node->lvalue(), phase);
    ann->annotateNode(node->rvalue(), phase);
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<BinaryNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<BinaryNode> node,
                       Annotator::Phase phase)
  {
    ann->annotateNode(node->left(), phase);
    ann->annotateNode(node->right(), phase);
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<UnaryNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<UnaryNode> node,
                       Annotator::Phase phase)
  {
    ann->annotateNode(node->base(), phase);
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<IfNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<IfNode> node,
                       Annotator::Phase phase)
  {
    ann->annotateNode(node->test(), phase);

    node->consequent()->setIsInTailPos(node->isInTailPos());
    ann->annotateNode(node->consequent(), phase);
    if (node->alternate()) {
      node->alternate()->setIsInTailPos(node->isInTailPos());
      ann->annotateNode(node->alternate(), phase);
    }
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<KeyargNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<KeyargNode> node,
                       Annotator::Phase phase)
  {
    ann->annotateNode(node->value(), phase);
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<SelectNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<SelectNode> node,
                       Annotator::Phase phase)
  {
    // TODO : set tail node position
    ann->annotateNode(node->test(), phase);
    if (node->comparator())
      ann->annotateNode(node->comparator(), phase);

    for (size_t i = 0; i < node->mappingCount(); i++) {
      if (node->mappingAt(i).fTestValues.empty()) {
        ann->annotateNode(node->mappingAt(i).fConsequent, phase);
      }
      else {
        for (auto& testValue : node->mappingAt(i).fTestValues)
          ann->annotateNode(testValue, phase);
      }
      ann->annotateNode(node->mappingAt(i).fConsequent, phase);
    }
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<RangeNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<RangeNode> node,
                       Annotator::Phase phase)
  {
    // TODO : set tail node position
    ann->annotateNode(node->from(), phase);
    ann->annotateNode(node->to(), phase);
    if (node->by())
      ann->annotateNode(node->by(), phase);
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<TypeDefNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<TypeDefNode> node,
                       Annotator::Phase phase)
  {
    // TODO : set tail node position

    // don't re-register the type if global; it is registered in pass2 already
    // if (phase == Annotator::kRegister)
    //   ann->fScope->registerType(node->srcpos(), node->name(), node->defType());
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<WhileNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<WhileNode> node,
                       Annotator::Phase phase)
  {
    ann->annotateNode(node->test(), phase);

    node->body()->setIsInTailPos(node->isInTailPos());
    ann->annotateNode(node->body(), phase);
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<VectorNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<VectorNode> node,
                       Annotator::Phase phase)
  {
    ann->annotateNodeList(node->children(), !K(markTailPos), K(markSingleType), phase);
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<DictNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<DictNode> node,
                       Annotator::Phase phase)
  {
    ann->annotateNodeList(node->children(), !K(markTailPos), K(markSingleType), phase);
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<CastNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<CastNode> node,
                       Annotator::Phase phase)
  {
    ann->annotateNode(node->base(), phase);
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<SlotRefNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<SlotRefNode> node,
                       Annotator::Phase phase)
  {
    ann->annotateNode(node->base(), phase);
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
  an.annotateNode(src, Annotator::kRegister);
  an.annotateNode(src, Annotator::kLookup);
  return src;
}


//----------------------------------------------------------------------------

Annotator::Annotator(std::shared_ptr<Scope> scope, Compiler& compiler)
    : fScope(std::move(scope))
    , fCompiler(compiler)
{
}


void Annotator::annotateNode(std::shared_ptr<AstNode> node, Phase phase)
{
  if (phase == Annotator::kRegister)
    node->setScope(fScope);

  dispatchNode<void>(
      node, [&](auto nd) { NodeAnnotator<decltype(nd)>::annotate(this, nd, phase); });
}


void Annotator::annotateNodeList(NodeList& nl, bool marktailpos, bool marksingletype,
                                 Phase phase)
{
  const size_t nlsize = nl.size();

  for (size_t i = 0; i < nlsize; i++) {
    if (marktailpos && i == nlsize - 1)
      nl[i]->setIsInTailPos(true);

    if (marksingletype)
      nl[i]->setIsSingleTypeRequired(true);

    annotateNode(nl[i], phase);
  }
}

}  // namespace herschel
