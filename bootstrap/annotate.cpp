/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include "apt.h"
#include "annotate.h"
#include "scope.h"
#include "symbol.h"
#include "log.h"

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
  NodeList& nl = node->children();
  for (size_t i = 0; i < nl.size(); i++) {
    annotateNode(nl[i]);
  }
}


//------------------------------------------------------------------------------

void
Annotator::annotate(AptNode* node)
{
}


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
    logf(kError, "Unknown variable '%s'", (const char*)StrHelper(node->name()));
  }
  else {
    takeFullNameFromNode(node, var);
  }
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
  for (size_t pidx = 0; pidx < node->params().size(); pidx++)
    annotateNode(node->params()[pidx]);

  annotateNode(node->body());
}


void
Annotator::annotate(FunctionNode* node)
{
}


void
Annotator::annotate(SlotdefNode* node)
{
}


void
Annotator::annotate(BlockNode* node)
{
  NodeList& nl = node->children();
  for (size_t i = 0; i < nl.size(); i++) {
    annotateNode(nl[i]);
  }
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

  NodeList& nl = node->children();
  for (size_t i = 0; i < nl.size(); i++)
    annotateNode(nl[i]);
}


void
Annotator::annotate(ArrayNode* node)
{
}


void
Annotator::annotate(ArraySymbolNode* node)
{
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
}


void
Annotator::annotate(MatchNode* node)
{
}


void
Annotator::annotate(OnNode* node)
{
}


void
Annotator::annotate(RangeNode* node)
{
}


void
Annotator::annotate(SelectNode* node)
{
}


void
Annotator::annotate(ThenWhileNode* node)
{
}


void
Annotator::annotate(TypeNode* node)
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


