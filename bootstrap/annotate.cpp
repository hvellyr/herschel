/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.

   - look up all name references and complete their namespaces
 */

#include "annotate.h"
#include "apt.h"
#include "errcodes.h"
#include "log.h"
#include "properties.h"
#include "scope.h"
#include "symbol.h"
#include "parser.h"


#include <typeinfo>  //for 'typeid' to work


using namespace heather;

//----------------------------------------------------------------------------

Annotator::Annotator()
  : fPhase(kRegister)
{
}


void
Annotator::annotateRecursively(AptNode* node)
{
  {
    fScope = Parser::newRootScope();

    fPhase = kRegister;
    annotateNode(node);
  }

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
  annotateNodeList(node->children());
}


void
Annotator::annotateNodeList(NodeList& nl)
{
  for (size_t i = 0; i < nl.size(); i++)
    annotateNode(nl[i]);
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
  assert(0);
}


bool
Annotator::updateAllocType(SymbolNode* usingNode, const AptNode* referedNode)
{
  if (usingNode->scope()->isVarInOuterFunction(usingNode->name())) {
    const BindingNode* bindNode = dynamic_cast<const BindingNode*>(referedNode);
    assert(bindNode != NULL);

    const_cast<BindingNode*>(bindNode)->setAllocType(kAlloc_Shared);
    return true;
  }
  return false;
}


void
Annotator::annotate(SymbolNode* node)
{
  if (fPhase == kLookup) {
    const AptNode* var = node->scope()->lookupVarOrFunc(node->name(), true);
    if (var != NULL) {
      takeFullNameFromNode(node, var);

      const VardefNode* vardef = dynamic_cast<const VardefNode*>(var);
      if (vardef != NULL) {
        bool isShared = updateAllocType(node, vardef);
        node->setRefersTo(vardef->isLocal() ? kLocalVar : kGlobalVar,
                          isShared);
      }
      else if (dynamic_cast<const FuncDefNode*>(var) != NULL) {
        node->setRefersTo(kFunction, false);
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
        assert(0 && "unhandled registered symbol def");
      }

      return;
    }

    Type type = node->scope()->lookupType(node->name(), true);
    if (type.isDef()) {
      node->setName(type.typeName());
      return;
    }

    if (Properties::test_passLevel() > 2)
      errorf(node->srcpos(), E_UndefinedVar,
             "Unknown variable '%s'", (const char*)StrHelper(node->name()));
  }
}


void
Annotator::annotate(ArraySymbolNode* node)
{
  if (fPhase == kLookup) {
    Type type = node->scope()->lookupType(node->name(), true);
    if (!type.isDef()) {
      if (Properties::test_passLevel() > 2)
        errorf(node->srcpos(), E_UndefinedVar,
               "Unknown variable '%s'", (const char*)StrHelper(node->name()));
    }
    else
      node->setName(type.typeName());
  }
}


//------------------------------------------------------------------------------

void
Annotator::annotate(DefNode* node)
{
  VardefNode* vardefNode = dynamic_cast<VardefNode*>(node->defNode());
  if (vardefNode != NULL) {
    if (fPhase == kRegister)
      vardefNode->setScope(fScope);
    annotate(vardefNode, false);
    return;
  }

  FuncDefNode* funcNode = dynamic_cast<FuncDefNode*>(node->defNode());
  if (funcNode != NULL) {
    if (fPhase == kRegister)
      funcNode->setScope(fScope);
    annotate(funcNode, false);
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
    annotate(vardefNode, true);
    return;
  }

  FuncDefNode* funcNode = dynamic_cast<FuncDefNode*>(node->defNode());
  if (funcNode != NULL) {
    if (fPhase == kRegister)
      funcNode->setScope(fScope);
    annotate(funcNode, true);
    return;
  }

  annotateNode(node->defNode());
}


void
Annotator::annotate(VardefNode* node, bool isLocal)
{
  if (fPhase == kRegister) {
    if (!fScope->checkForRedefinition(node->srcpos(),
                                      Scope::kNormal, node->name()))
      fScope->registerVar(node->srcpos(), node->name(), node);
  }

  if (node->initExpr() != NULL)
    annotateNode(node->initExpr());
}


void
Annotator::annotate(FuncDefNode* node, bool isLocal)
{
  if (fPhase == kRegister)
    fScope->registerFunction(node->srcpos(), node->name(), node);

  ScopeHelper scopeHelper(fScope, false, true, kScopeL_Function);

  annotateNodeList(node->params());
  if (node->body() != NULL)
    annotateNode(node->body());
}


void
Annotator::annotate(FunctionNode* node)
{
  ScopeHelper scopeHelper(fScope, false, true, kScopeL_Function);

  annotateNodeList(node->params());
  if (node->body() != NULL)
    annotateNode(node->body());
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


void
Annotator::annotate(BlockNode* node)
{
  ScopeHelper scopeHelper(fScope, false, true, kScopeL_Local);
  annotateNodeList(node->children());
}


void
Annotator::annotate(ParamNode* node)
{
  if (fPhase == kRegister) {
    if (!fScope->checkForRedefinition(node->srcpos(),
                                      Scope::kNormal, node->name()))
      fScope->registerVar(node->srcpos(), node->name(), node);
  }

  if (node->initExpr() != NULL)
    annotateNode(node->initExpr());
}


void
Annotator::annotate(ApplyNode* node)
{
  annotateNode(node->base());
  annotateNodeList(node->children());
}


void
Annotator::annotate(ArrayNode* node)
{
  annotateNodeList(node->children());
}


void
Annotator::annotate(AssignNode* node)
{
  annotateNode(node->lvalue());
  annotateNode(node->rvalue());
}


void
Annotator::annotate(BinaryNode* node)
{
  annotateNode(node->left());
  annotateNode(node->right());
}


void
Annotator::annotate(NegateNode* node)
{
  annotateNode(node->base());
}


void
Annotator::annotate(IfNode* node)
{
  annotateNode(node->test());
  annotateNode(node->consequent());
  if (node->alternate())
    annotateNode(node->alternate());
}


void
Annotator::annotate(KeyargNode* node)
{
  annotateNode(node->value());
}


void
Annotator::annotate(MatchNode* node)
{
  annotateNode(node->expr());
  for (size_t i = 0; i < node->mappingCount(); i++) {
    annotateNode(node->mappingAt(i).fConsequent);
  }
}


void
Annotator::annotate(SelectNode* node)
{
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
  ScopeHelper scopeHelper(fScope, false, true, kScopeL_Local);

  annotateNodeList(node->params());
  annotateNode(node->body());
}


void
Annotator::annotate(RangeNode* node)
{
  annotateNode(node->from());
  annotateNode(node->to());
  if (node->by() != NULL)
    annotateNode(node->by());
}


void
Annotator::annotate(ThenWhileNode* node)
{
  annotateNode(node->first());
  annotateNode(node->step());
  annotateNode(node->test());
}


void
Annotator::annotate(TypeDefNode* node)
{
  if (fPhase == kRegister)
    fScope->registerType(node->srcpos(), node->name(), node->defType());
}


void
Annotator::annotate(WhileNode* node)
{
  annotateNode(node->test());
  annotateNode(node->body());
}


void
Annotator::annotate(VectorNode* node)
{
  annotateNodeList(node->children());
}


void
Annotator::annotate(DictNode* node)
{
  annotateNodeList(node->children());
}


void
Annotator::annotate(CastNode* node)
{
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
