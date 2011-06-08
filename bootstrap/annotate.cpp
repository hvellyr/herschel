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

AnnotatePass::AnnotatePass(int level, Scope* scope, Compiler* compiler)
  : AptNodeCompilePass(level),
    fScope(scope),
    fCompiler(compiler)
{
  hr_assert(fCompiler != NULL);
}


AptNode*
AnnotatePass::doApply(AptNode* src)
{
  Ptr<AptNode> node = src;
  Ptr<Annotator> an = new Annotator(fScope, fCompiler);
  an->annotateRecursively(node);
  return node.release();
}


//----------------------------------------------------------------------------

Annotator::Annotator(Scope* scope, Compiler* compiler)
  : fScope(scope),
    fPhase(kRegister),
    fCompiler(compiler)
{
  hr_assert(fCompiler != NULL);
}


void
Annotator::annotateRecursively(AptNode* node)
{
  fPhase = kRegister;
  annotateNode(node);

  fPhase = kLookup;
  annotateNode(node);
}


void
Annotator::annotateNode(AptNode* node)
{
  if (fPhase == kRegister)
    node->setScope(fScope);
  node->annotate(this);
}


void
Annotator::annotate(CompileUnitNode* node)
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
  const NamedNode* nn = dynamic_cast<const NamedNode*>(otherNode);
  if (nn != NULL) {
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
    const BindingNode* bindNode = dynamic_cast<const BindingNode*>(referedNode);
    hr_assert(bindNode != NULL);

    const_cast<BindingNode*>(bindNode)->setAllocType(kAlloc_Shared);
    return true;
  }
  return false;
}


void
Annotator::annotate(SymbolNode* node)
{
  if (fPhase == kLookup) {
    const AptNode* var = node->scope()->lookupVarOrFunc(node->name(),
                                                        K(showAmbiguousSymDef));
    if (var != NULL) {
      takeFullNameFromNode(node, var);

      const VardefNode* vardef = dynamic_cast<const VardefNode*>(var);
      if (vardef != NULL) {
        bool isShared = updateAllocType(node, vardef);
        node->setRefersTo(vardef->isLocal() ? kLocalVar : kGlobalVar,
                          isShared);
      }
      else if (const FuncDefNode* funcdef = dynamic_cast<const FuncDefNode*>(var)) {
        if (funcdef->isGeneric())
          node->setRefersTo(kGeneric, !K(isShared));
        else
          node->setRefersTo(kFunction, !K(isShared));

        // keep an additional link to this function (which is obviously
        // referenced), such that the codegen can produce extern declaration
        // for it if needed
        SrcPos srcpos;
        if (!fCompiler->referredFunctionCache()->hasName(Scope::kNormal,
                                                         node->name(),
                                                         &srcpos))
        {
          fCompiler->referredFunctionCache()->registerFunction(funcdef->srcpos(),
                                                               node->name(),
                                                               funcdef->clone());
        }
      }
      else if (dynamic_cast<const ParamNode*>(var) != NULL) {
        bool isShared = updateAllocType(node, var);
        node->setRefersTo(kParam, isShared);
      }
      else if (dynamic_cast<const SlotdefNode*>(var) != NULL) {
        bool isShared = updateAllocType(node, var);
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
             "Unknown symbol '%s'", (const char*)StrHelper(node->name()));
      // node->scope()->dumpDebug(true);
    }
#endif
  }
}


void
Annotator::annotate(ArrayTypeNode* node)
{
  annotateNode(node->typeNode());
}


void
Annotator::annotate(TypeNode* node)
{
  // TODO
}


//------------------------------------------------------------------------------

void
Annotator::annotate(DefNode* node)
{
  VardefNode* vardefNode = dynamic_cast<VardefNode*>(node->defNode());
  if (vardefNode != NULL) {
    if (fPhase == kRegister)
      vardefNode->setScope(fScope);
    annotate(vardefNode, !K(isLocal));
    return;
  }

  FuncDefNode* funcNode = dynamic_cast<FuncDefNode*>(node->defNode());
  if (funcNode != NULL) {
    if (fPhase == kRegister)
      funcNode->setScope(fScope);
    annotate(funcNode, !K(isLocal));
    return;
  }

  annotateNode(node->defNode());
}


void
Annotator::annotate(LetNode* node)
{
  VardefNode* vardefNode = dynamic_cast<VardefNode*>(node->defNode());
  if (vardefNode != NULL) {
    if (fPhase == kRegister)
      vardefNode->setScope(fScope);
    annotate(vardefNode, K(isLocal));
    return;
  }

  FuncDefNode* funcNode = dynamic_cast<FuncDefNode*>(node->defNode());
  if (funcNode != NULL) {
    if (fPhase == kRegister)
      funcNode->setScope(fScope);
    annotate(funcNode, K(isLocal));
    return;
  }

  annotateNode(node->defNode());
}


void
Annotator::annotate(VardefNode* node, bool isLocal)
{
  if (fPhase == kRegister) {
    if (isLocal) {
      if (!fScope->checkForRedefinition(node->srcpos(),
                                        Scope::kNormal, node->name()))
        fScope->registerVar(node->srcpos(), node->name(), node);
    }
  }

  if (node->initExpr() != NULL) {
    node->initExpr()->setIsSingleTypeRequired(true);
    annotateNode(node->initExpr());
  }
}


void
Annotator::annotate(FuncDefNode* node, bool isLocal)
{
  if (fPhase == kRegister) {
    if (isLocal) {
      fScope->registerFunction(node->srcpos(), node->name(), node);
    }
    else if (node->isMethod()) {
      const AptNode* var = node->scope()->lookupVarOrFunc(node->name(),
                                                          K(showAmbiguousSymDef));
      if (var == NULL) {
        errorf(node->srcpos(), E_NoGenericFunction,
               "No generic function definition found for method");
      }
      else if (const FuncDefNode* funcdef = dynamic_cast<const FuncDefNode*>(var)) {
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
  if (node->body() != NULL) {
    node->body()->setIsInTailPos(true);
    annotateNode(node->body());
  }
}


void
Annotator::annotate(FunctionNode* node)
{
  ScopeHelper scopeHelper(fScope, !K(doExport), !K(isInnerScope),
                          kScopeL_Function);

  annotateNodeList(node->params(), !K(markTailPos), K(markSingleType));
  if (node->body() != NULL) {
    node->body()->setIsInTailPos(true);
    annotateNode(node->body());
  }
}


void
Annotator::annotate(SlotdefNode* node)
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

    virtual bool preApply(AptNode* node)
    {
      if (BlockNode* blockNode = dynamic_cast<BlockNode*>(node)) {
        NodeList& nl = blockNode->children();
        for (size_t i = 0; i < nl.size(); ) {
          if (SymbolNode* symNode = dynamic_cast<SymbolNode*>(nl[i].obj())) {
            if (symNode->loopId() == fLoopId) {
              // replace the return symbol with a simple lang|unspecified.
              // This is required to give the expression a concrete return
              // value.  Otherwise SSA compilation in codegen becomes more
              // complicated.
              nl[i] = new SymbolNode(nl[i]->srcpos(), Names::kLangUnspecified);
            }
          }
          else if (LetNode* letNode = dynamic_cast<LetNode*>(nl[i].obj())) {
            if (letNode->loopId() == fLoopId) {
              nl.erase(nl.begin() + i);
              continue;
            }
          }
          else if (AssignNode* assignNode = dynamic_cast<AssignNode*>(nl[i].obj())) {
            if (assignNode->loopId() == fLoopId)
              nl[i] = assignNode->rvalue();
          }

          i++;
        }
      }
      return true;
    }

    virtual void postApply(AptNode* node)
    {
    }

    int fLoopId;
  };
};


void
Annotator::annotate(BlockNode* node)
{
  ScopeHelper scopeHelper(fScope, !K(doExport), K(isInnerScope),
                          kScopeL_Local);

  if (!node->isInTailPos()) {
    int loopId = 0;

    NodeList& nl = node->children();
    const size_t nlsize = nl.size();
    for (size_t i = 0; i < nlsize; i++) {
      if (SymbolNode* symNode = dynamic_cast<SymbolNode*>(nl[i].obj())) {
        if (symNode->loopId() > 0) {
          loopId = symNode->loopId();
          break;
        }
      }
      else if (LetNode* letNode = dynamic_cast<LetNode*>(nl[i].obj())) {
        if (letNode->loopId() > 0) {
          loopId = letNode->loopId();
          break;
        }
      }
    }

    if (loopId > 0) {
      StripLoopNodesTraverseDelegate delegate(loopId);
      Traversator(delegate).traverseNode(node);
    }
  }

  annotateNodeList(node->children(), node->isInTailPos(), !K(markSingleType));
}


void
Annotator::annotate(ParamNode* node)
{
  if (fPhase == kRegister) {
    if (!fScope->checkForRedefinition(node->srcpos(),
                                      Scope::kNormal, node->name()))
      fScope->registerVar(node->srcpos(), node->name(), node);
  }

  if (node->initExpr() != NULL) {
    node->initExpr()->setIsSingleTypeRequired(true);
    annotateNode(node->initExpr());
  }
}


void
Annotator::annotate(ApplyNode* node)
{
  node->base()->setIsSingleTypeRequired(true);
  annotateNode(node->base());
  annotateNodeList(node->children(), !K(markTailPos), K(markSingleType));
}


void
Annotator::annotate(ArrayNode* node)
{
  annotateNodeList(node->children(), !K(markTailPos), K(markSingleType));
}


void
Annotator::annotate(AssignNode* node)
{
  annotateNode(node->lvalue());

  node->rvalue()->setIsSingleTypeRequired(true);
  annotateNode(node->rvalue());
}


void
Annotator::annotate(BinaryNode* node)
{
  annotateNode(node->left());
  annotateNode(node->right());
}


void
Annotator::annotate(UnaryNode* node)
{
  annotateNode(node->base());
}


void
Annotator::annotate(IfNode* node)
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
Annotator::annotate(KeyargNode* node)
{
  node->value()->setIsSingleTypeRequired(true);
  annotateNode(node->value());
}


void
Annotator::annotate(MatchNode* node)
{
  // TODO : set tail node position
  annotateNode(node->expr());
  for (size_t i = 0; i < node->mappingCount(); i++) {
    annotateNode(node->mappingAt(i).fConsequent);
  }
}


void
Annotator::annotate(SelectNode* node)
{
  // TODO : set tail node position
  annotateNode(node->test());
  if (node->comparator() != NULL)
    annotateNode(node->comparator());

  for (size_t i = 0; i < node->mappingCount(); i++) {
    if (node->mappingAt(i).fTestValues.empty()) {
      annotateNode(node->mappingAt(i).fConsequent);
    }
    else {
      for (size_t j = 0; j < node->mappingAt(i).fTestValues.size(); j++)
        annotateNode(node->mappingAt(i).fTestValues[j]);
    }
    annotateNode(node->mappingAt(i).fConsequent);
  }
}


void
Annotator::annotate(OnNode* node)
{
  // TODO : set tail node position
  ScopeHelper scopeHelper(fScope, !K(doExport), K(isInnerScope),
                          kScopeL_Local);

  annotateNodeList(node->params(), !K(markTailPos), K(markSingleType));
  annotateNode(node->body());
}


void
Annotator::annotate(RangeNode* node)
{
  // TODO : set tail node position
  annotateNode(node->from());
  annotateNode(node->to());
  if (node->by() != NULL)
    annotateNode(node->by());
}


void
Annotator::annotate(TypeDefNode* node)
{
  // TODO : set tail node position

  // don't re-register the type if global; it is registered in pass2 already
  // if (fPhase == kRegister)
  //   fScope->registerType(node->srcpos(), node->name(), node->defType());
}


void
Annotator::annotate(WhileNode* node)
{
  node->test()->setIsSingleTypeRequired(true);
  annotateNode(node->test());

  node->body()->setIsInTailPos(node->isInTailPos());
  annotateNode(node->body());
}


void
Annotator::annotate(VectorNode* node)
{
  annotateNodeList(node->children(), !K(markTailPos), K(markSingleType));
}


void
Annotator::annotate(DictNode* node)
{
  annotateNodeList(node->children(), !K(markTailPos), K(markSingleType));
}


void
Annotator::annotate(CastNode* node)
{
  node->base()->setIsSingleTypeRequired(true);
  annotateNode(node->base());
}


//------------------------------------------------------------------------------

void
Annotator::annotate(BoolNode* node)
{
  // Nothing to annotate here
}


void
Annotator::annotate(CharNode* node)
{
  // Nothing to annotate here
}


void
Annotator::annotate(StringNode* node)
{
  // Nothing to annotate here
}


void
Annotator::annotate(RationalNode* node)
{
  // Nothing to annotate here
}


void
Annotator::annotate(RealNode* node)
{
  // Nothing to annotate here
}


void
Annotator::annotate(IntNode* node)
{
  // Nothing to annotate here
}


void
Annotator::annotate(KeywordNode* node)
{
  // Nothing to annotate here
}


void
Annotator::annotate(UnitConstNode* node)
{
  annotateNode(node->value());
}


void
Annotator::annotate(UndefNode* node)
{
  // Nothing to annotate here
}
