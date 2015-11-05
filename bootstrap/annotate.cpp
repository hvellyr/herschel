/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.

   - look up all name references and complete their namespaces
 */

#include "annotate.h"
#include "apt.h"
#include "errcodes.h"
#include "log.h"
#include "properties.h"
#include "scope.h"
#include "symbol.h"
#include "compiler.h"
#include "rootscope.h"
#include "traverse.h"
#include "predefined.h"


#include <typeinfo>  //for 'typeid' to work


using namespace herschel;


//----------------------------------------------------------------------------

AnnotatePass::AnnotatePass(int level, std::shared_ptr<Scope> scope,
                           Compiler& compiler)
  : AptNodeCompilePass(level),
    fScope(std::move(scope)),
    fCompiler(compiler)
{
}


std::shared_ptr<AptNode>
AnnotatePass::doApply(std::shared_ptr<AptNode> src)
{
  Annotator an{fScope, fCompiler};
  an.annotateRecursively(src);
  return src;
}


//----------------------------------------------------------------------------

Annotator::Annotator(std::shared_ptr<Scope> scope, Compiler& compiler)
  : fScope(std::move(scope)),
    fPhase(kRegister),
    fCompiler(compiler)
{
}


void
Annotator::annotateRecursively(std::shared_ptr<AptNode> node)
{
  fPhase = kRegister;
  annotateNode(node);

  fPhase = kLookup;
  annotateNode(node);
}


void
Annotator::annotateNode(std::shared_ptr<AptNode> node)
{
  if (fPhase == kRegister)
    node->setScope(fScope);
  node->annotate(this, node);
}


void
Annotator::annotate(std::shared_ptr<CompileUnitNode> node)
{
  annotateNodeList(node->children(), !K(markTailPos), !K(markSingleType));
}


void
Annotator::annotateNodeList(NodeList& nl, bool marktailpos, bool marksingletype)
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


//------------------------------------------------------------------------------

void
Annotator::takeFullNameFromNode(SymbolNode* node, const AptNode* otherNode)
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


bool
Annotator::updateAllocType(SymbolNode* usingNode, const AptNode* referedNode)
{
  if (usingNode->scope()->isVarInOuterFunction(usingNode->name())) {
    auto bindNode = dynamic_cast<const BindingNode*>(referedNode);
    hr_assert(bindNode);

    const_cast<BindingNode*>(bindNode)->setAllocType(kAlloc_Shared);
    return true;
  }
  return false;
}


void
Annotator::annotate(std::shared_ptr<SymbolNode> node)
{
  if (fPhase == kLookup) {
    const AptNode* var = node->scope()->lookupVarOrFunc(node->name(),
                                                        K(showAmbiguousSymDef));
    if (var) {
      takeFullNameFromNode(node.get(), var);

      auto vardef = dynamic_cast<const VardefNode*>(var);
      if (vardef) {
        bool isShared = updateAllocType(node.get(), vardef);
        node->setRefersTo(vardef->isLocal() ? kLocalVar : kGlobalVar,
                          isShared);
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
        if (!fCompiler.referredFunctionCache()->hasName(Scope::kNormal,
                                                        node->name(),
                                                        &srcpos))
        {
          fCompiler.referredFunctionCache()->registerFunction(funcdef->srcpos(),
                                                              node->name(),
                                                              funcdef->clone());
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

    Type type = node->scope()->lookupType(node->name(),
                                          K(showAmbiguousSymDef));
    if (type.isDef()) {
      node->setName(type.typeName());
      return;
    }

#if defined(UNITTESTS)
    if (Properties::test_passLevel() > 2) {
      errorf(node->srcpos(), E_UndefinedVar,
             "Unknown symbol '%s'", (zstring)StrHelper(node->name()));
      // node->scope()->dumpDebug(true);
    }
#endif
  }
}


void
Annotator::annotate(std::shared_ptr<ArrayTypeNode> node)
{
  annotateNode(node->typeNode());
}


void
Annotator::annotate(std::shared_ptr<TypeNode> node)
{
  // TODO
}


//------------------------------------------------------------------------------

void
Annotator::annotate(std::shared_ptr<DefNode> node)
{
  auto vardefNode = std::dynamic_pointer_cast<VardefNode>(node->defNode());
  if (vardefNode) {
    if (fPhase == kRegister)
      vardefNode->setScope(fScope);
    annotate(vardefNode, !K(isLocal));
    return;
  }

  auto funcNode = std::dynamic_pointer_cast<FuncDefNode>(node->defNode());
  if (funcNode) {
    if (fPhase == kRegister)
      funcNode->setScope(fScope);
    annotate(funcNode, !K(isLocal));
    return;
  }

  annotateNode(node->defNode());
}


void
Annotator::annotate(std::shared_ptr<LetNode> node)
{
  auto vardefNode = std::dynamic_pointer_cast<VardefNode>(node->defNode());
  if (vardefNode) {
    if (fPhase == kRegister)
      vardefNode->setScope(fScope);
    annotate(vardefNode, K(isLocal));
    return;
  }

  auto funcNode = std::dynamic_pointer_cast<FuncDefNode>(node->defNode());
  if (funcNode) {
    if (fPhase == kRegister)
      funcNode->setScope(fScope);
    annotate(funcNode, K(isLocal));
    return;
  }

  annotateNode(node->defNode());
}


void
Annotator::annotate(std::shared_ptr<VardefNode> node, bool isLocal)
{
  if (fPhase == kRegister) {
    if (isLocal) {
      if (!fScope->checkForRedefinition(node->srcpos(),
                                        Scope::kNormal, node->name()))
        fScope->registerVar(node->srcpos(), node->name(), node);
    }
  }

  if (node->initExpr()) {
    node->initExpr()->setIsSingleTypeRequired(true);
    annotateNode(node->initExpr());
  }
}


void
Annotator::annotate(std::shared_ptr<FuncDefNode> node, bool isLocal)
{
  if (fPhase == kRegister) {
    if (isLocal) {
      fScope->registerFunction(node->srcpos(), node->name(), node);
    }
    else if (node->isMethod()) {
      auto var = node->scope()->lookupVarOrFunc(node->name(),
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
        errorf(var->srcpos(), E_BadGenericReferrer,
               "Referred symbol definition was here");
      }
    }
  }

  ScopeHelper scopeHelper(fScope, !K(doExport), K(isInnerScope),
                          kScopeL_Function);

  annotateNodeList(node->params(), !K(markTailPos), K(markSingleType));
  if (node->body()) {
    node->body()->setIsInTailPos(true);
    annotateNode(node->body());
  }
}


void
Annotator::annotate(std::shared_ptr<FunctionNode> node)
{
  ScopeHelper scopeHelper(fScope, !K(doExport), !K(isInnerScope),
                          kScopeL_Function);

  annotateNodeList(node->params(), !K(markTailPos), K(markSingleType));
  if (node->body()) {
    node->body()->setIsInTailPos(true);
    annotateNode(node->body());
  }
}


void
Annotator::annotate(std::shared_ptr<SlotdefNode> node)
{
  // if (fPhase == kRegister) {
  //   if (!fScope->checkForRedefinition(node->srcpos(),
  //                                     Scope::kNormal, node->name()))
  //     fScope->registerVar(node->srcpos(), node->name(), node);
  // }
  // TODO
}


namespace herschel
{
  class StripLoopNodesTraverseDelegate : public TraverseDelegate
  {
  public:
    StripLoopNodesTraverseDelegate(int loopId)
      : fLoopId(loopId)
    {}

    bool preApply(AptNode& node) override
    {
      if (auto blockNode = dynamic_cast<BlockNode*>(&node)) {
        NodeList& nl = blockNode->children();
        for (size_t i = 0; i < nl.size(); ) {
          if (auto symNode = dynamic_cast<SymbolNode*>(nl[i].get())) {
            if (symNode->loopId() == fLoopId) {
              // replace the return symbol with a simple lang|unspecified.
              // This is required to give the expression a concrete return
              // value.  Otherwise SSA compilation in codegen becomes more
              // complicated.
              nl[i] = makeSymbolNode(nl[i]->srcpos(), Names::kLangUnspecified);
            }
          }
          else if (auto letNode = dynamic_cast<LetNode*>(nl[i].get())) {
            if (letNode->loopId() == fLoopId) {
              nl.erase(nl.begin() + i);
              continue;
            }
          }
          else if (auto assignNode = dynamic_cast<AssignNode*>(nl[i].get())) {
            if (assignNode->loopId() == fLoopId)
              nl[i] = assignNode->rvalue();
          }

          i++;
        }
      }
      return true;
    }

    void postApply(AptNode& node) override
    {
    }

    int fLoopId;
  };
};


void
Annotator::annotate(std::shared_ptr<BlockNode> node)
{
  ScopeHelper scopeHelper(fScope, !K(doExport), K(isInnerScope),
                          kScopeL_Local);

  if (!node->isInTailPos()) {
    int loopId = 0;

    NodeList& nl = node->children();
    const size_t nlsize = nl.size();
    for (size_t i = 0; i < nlsize; i++) {
      if (auto symNode = dynamic_cast<SymbolNode*>(nl[i].get())) {
        if (symNode->loopId() > 0) {
          loopId = symNode->loopId();
          break;
        }
      }
      else if (auto letNode = dynamic_cast<LetNode*>(nl[i].get())) {
        if (letNode->loopId() > 0) {
          loopId = letNode->loopId();
          break;
        }
      }
    }

    if (loopId > 0) {
      StripLoopNodesTraverseDelegate delegate(loopId);
      Traversator(delegate).traverseNode(*node);
    }
  }

  annotateNodeList(node->children(), node->isInTailPos(), !K(markSingleType));
}


void
Annotator::annotate(std::shared_ptr<ParamNode> node)
{
  if (fPhase == kRegister) {
    if (!fScope->checkForRedefinition(node->srcpos(),
                                      Scope::kNormal, node->name()))
      fScope->registerVar(node->srcpos(), node->name(), node);
  }

  if (node->initExpr()) {
    node->initExpr()->setIsSingleTypeRequired(true);
    annotateNode(node->initExpr());
  }
}


void
Annotator::annotate(std::shared_ptr<ApplyNode> node)
{
  node->base()->setIsSingleTypeRequired(true);
  annotateNode(node->base());
  annotateNodeList(node->children(), !K(markTailPos), K(markSingleType));
}


void
Annotator::annotate(std::shared_ptr<ArrayNode> node)
{
  annotateNodeList(node->children(), !K(markTailPos), K(markSingleType));
}


void
Annotator::annotate(std::shared_ptr<AssignNode> node)
{
  annotateNode(node->lvalue());

  node->rvalue()->setIsSingleTypeRequired(true);
  annotateNode(node->rvalue());
}


void
Annotator::annotate(std::shared_ptr<BinaryNode> node)
{
  annotateNode(node->left());
  annotateNode(node->right());
}


void
Annotator::annotate(std::shared_ptr<UnaryNode> node)
{
  annotateNode(node->base());
}


void
Annotator::annotate(std::shared_ptr<IfNode> node)
{
  annotateNode(node->test());

  node->consequent()->setIsInTailPos(node->isInTailPos());
  annotateNode(node->consequent());
  if (node->alternate()) {
    node->alternate()->setIsInTailPos(node->isInTailPos());
    annotateNode(node->alternate());
  }
}


void
Annotator::annotate(std::shared_ptr<KeyargNode> node)
{
  node->value()->setIsSingleTypeRequired(true);
  annotateNode(node->value());
}


void
Annotator::annotate(std::shared_ptr<MatchNode> node)
{
  hr_invalid("there should be no match mode anymore in this phase");
}


void
Annotator::annotate(std::shared_ptr<SelectNode> node)
{
  // TODO : set tail node position
  annotateNode(node->test());
  if (node->comparator())
    annotateNode(node->comparator());

  for (size_t i = 0; i < node->mappingCount(); i++) {
    if (node->mappingAt(i).fTestValues.empty()) {
      annotateNode(node->mappingAt(i).fConsequent);
    }
    else {
      for (auto& testValue : node->mappingAt(i).fTestValues)
        annotateNode(testValue);
    }
    annotateNode(node->mappingAt(i).fConsequent);
  }
}


void
Annotator::annotate(std::shared_ptr<OnNode> node)
{
  // TODO : set tail node position
  ScopeHelper scopeHelper(fScope, !K(doExport), K(isInnerScope),
                          kScopeL_Local);

  annotateNodeList(node->params(), !K(markTailPos), K(markSingleType));
  annotateNode(node->body());
}


void
Annotator::annotate(std::shared_ptr<RangeNode> node)
{
  // TODO : set tail node position
  annotateNode(node->from());
  annotateNode(node->to());
  if (node->by())
    annotateNode(node->by());
}


void
Annotator::annotate(std::shared_ptr<TypeDefNode> node)
{
  // TODO : set tail node position

  // don't re-register the type if global; it is registered in pass2 already
  // if (fPhase == kRegister)
  //   fScope->registerType(node->srcpos(), node->name(), node->defType());
}


void
Annotator::annotate(std::shared_ptr<WhileNode> node)
{
  node->test()->setIsSingleTypeRequired(true);
  annotateNode(node->test());

  node->body()->setIsInTailPos(node->isInTailPos());
  annotateNode(node->body());
}


void
Annotator::annotate(std::shared_ptr<VectorNode> node)
{
  annotateNodeList(node->children(), !K(markTailPos), K(markSingleType));
}


void
Annotator::annotate(std::shared_ptr<DictNode> node)
{
  annotateNodeList(node->children(), !K(markTailPos), K(markSingleType));
}


void
Annotator::annotate(std::shared_ptr<CastNode> node)
{
  node->base()->setIsSingleTypeRequired(true);
  annotateNode(node->base());
}


//------------------------------------------------------------------------------

void
Annotator::annotate(std::shared_ptr<BoolNode> node)
{
  // Nothing to annotate here
}


void
Annotator::annotate(std::shared_ptr<CharNode> node)
{
  // Nothing to annotate here
}


void
Annotator::annotate(std::shared_ptr<StringNode> node)
{
  // Nothing to annotate here
}


void
Annotator::annotate(std::shared_ptr<RationalNode> node)
{
  // Nothing to annotate here
}


void
Annotator::annotate(std::shared_ptr<RealNode> node)
{
  // Nothing to annotate here
}


void
Annotator::annotate(std::shared_ptr<IntNode> node)
{
  // Nothing to annotate here
}


void
Annotator::annotate(std::shared_ptr<KeywordNode> node)
{
  // Nothing to annotate here
}


void
Annotator::annotate(std::shared_ptr<UnitConstNode> node)
{
  annotateNode(node->value());
}


void
Annotator::annotate(std::shared_ptr<UndefNode> node)
{
  // Nothing to annotate here
}


void
Annotator::annotate(std::shared_ptr<SlotRefNode> node)
{
  annotateNode(node->base());
}
