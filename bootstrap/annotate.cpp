/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include "annotate.h"
#include "apt.h"
#include "errcodes.h"
#include "log.h"
#include "properties.h"
#include "scope.h"
#include "symbol.h"


#include <typeinfo>  //for 'typeid' to work
//----------------------------------------------------------------------------

using namespace heather;

Annotator::Annotator()
{
}


void
Annotator::annotateNode(AptNode* node)
{
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
  logf(kError, "Unexpected type here: %s\n", typeid(*otherNode).name());
  assert(0);
}


void
Annotator::annotate(SymbolNode* node)
{
  const AptNode* var = node->scope()->lookupVarOrFunc(node->name(), true);
  if (var == NULL) {
    if (Properties::test_passLevel() > 2)
      errorf(node->srcpos(), E_UndefinedVar,
             "Unknown variable '%s'", (const char*)StrHelper(node->name()));
  }
  else {
    takeFullNameFromNode(node, var);
  }
}


void
Annotator::annotate(ArraySymbolNode* node)
{
  // TODO
}


//------------------------------------------------------------------------------

void
Annotator::annotate(DefNode* node)
{
  VardefNode* vardefNode = dynamic_cast<VardefNode*>(node->defNode());
  if (vardefNode != NULL) {
    annotate(vardefNode, false);
    return;
  }

  FuncDefNode* funcNode = dynamic_cast<FuncDefNode*>(node->defNode());
  if (funcNode != NULL) {
    annotate(funcNode, false);
    return;
  }

  // TODO
  assert(0);
}


void
Annotator::annotate(LetNode* node)
{
  VardefNode* vardefNode = dynamic_cast<VardefNode*>(node->defNode());
  if (vardefNode != NULL) {
    annotate(vardefNode, true);
    return;
  }

  FuncDefNode* funcNode = dynamic_cast<FuncDefNode*>(node->defNode());
  if (funcNode != NULL) {
    annotate(funcNode, true);
    return;
  }

  // TODO
  assert(0);
}


void
Annotator::annotate(VardefNode* node, bool isLocal)
{
  if (node->initExpr() != NULL)
    annotateNode(node->initExpr());
}


void
Annotator::annotate(FuncDefNode* node, bool isLocal)
{
  annotateNodeList(node->params());
  annotateNode(node->body());
}


void
Annotator::annotate(FunctionNode* node)
{
  annotateNodeList(node->params());
  annotateNode(node->body());
}


void
Annotator::annotate(SlotdefNode* node)
{
  // TODO
}


void
Annotator::annotate(BlockNode* node)
{
  annotateNodeList(node->children());
}


void
Annotator::annotate(ParamNode* node)
{
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
  // TODO
}


void
Annotator::annotate(MatchNode* node)
{
  annotateNode(node->fExpr);
  for (size_t i = 0; i < node->fMappings.size(); i++) {
    annotateNode(node->fMappings[i].fConsequent);
  }
}


void
Annotator::annotate(SelectNode* node)
{
  annotateNode(node->fTest);
  if (node->fComparator != NULL)
    annotateNode(node->fComparator);

  for (size_t i = 0; i < node->fMappings.size(); i++) {
    if (node->fMappings[i].fTestValues.empty()) {
      annotateNode(node->fMappings[i].fConsequent);
    }
    else {
      for (size_t j = 0; j < node->fMappings[i].fTestValues.size(); j++)
        annotateNode(node->fMappings[i].fTestValues[j]);
    }
    annotateNode(node->fMappings[i].fConsequent);
  }
}


void
Annotator::annotate(OnNode* node)
{
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
}


void
Annotator::annotate(TypeDefNode* node)
{
}


void
Annotator::annotate(WhileNode* node)
{
}


void
Annotator::annotate(VectorNode* node)
{
}


void
Annotator::annotate(DictNode* node)
{
}


//------------------------------------------------------------------------------

void
Annotator::annotate(BoolNode* node)
{
}


void
Annotator::annotate(CharNode* node)
{
}


void
Annotator::annotate(StringNode* node)
{
}


void
Annotator::annotate(RationalNode* node)
{
}


void
Annotator::annotate(RealNode* node)
{
}


void
Annotator::annotate(IntNode* node)
{
}


void
Annotator::annotate(KeywordNode* node)
{
}


void
Annotator::annotate(UnitConstant* node)
{
}


void
Annotator::annotate(CastNode* node)
{
}


