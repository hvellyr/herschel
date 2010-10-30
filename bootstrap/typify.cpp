/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.

   - look up all name references and complete their namespaces
 */

#include "typify.h"
#include "apt.h"
#include "errcodes.h"
#include "log.h"
#include "properties.h"
#include "scope.h"
#include "symbol.h"


using namespace heather;

//----------------------------------------------------------------------------

Typifier::Typifier()
  : fPhase(kTypify)
{
}


void
Typifier::typifyRecursively(AptNode* node)
{
  fPhase = kTypify;
  typifyNode(node);

  fPhase = kCheck;
  typifyNode(node);
}


void
Typifier::typifyNode(AptNode* node)
{
  node->typify(this);
}


void
Typifier::typify(CompileUnitNode* node)
{
  typifyNodeList(node->children());
}


void
Typifier::typifyNodeList(NodeList& nl)
{
  for (size_t i = 0; i < nl.size(); i++)
    typifyNode(nl[i]);
}


//------------------------------------------------------------------------------

void
Typifier::typify(SymbolNode* node)
{
  // TODO
}


void
Typifier::typify(ArraySymbolNode* node)
{
  //TODO
}


//------------------------------------------------------------------------------

void
Typifier::typify(DefNode* node)
{
  typifyNode(node->defNode());
}


void
Typifier::typify(LetNode* node)
{
  typifyNode(node->defNode());
}


void
Typifier::typify(VardefNode* node)
{
  if (node->initExpr() != NULL)
    typifyNode(node->initExpr());
}


void
Typifier::typify(FuncDefNode* node)
{
  typifyNodeList(node->params());
  if (node->body() != NULL)
    typifyNode(node->body());
}


void
Typifier::typify(FunctionNode* node)
{
  typifyNodeList(node->params());
  if (node->body() != NULL)
    typifyNode(node->body());
}


void
Typifier::typify(SlotdefNode* node)
{
  // TODO
}


void
Typifier::typify(BlockNode* node)
{
  typifyNodeList(node->children());
}


void
Typifier::typify(ParamNode* node)
{
  if (node->initExpr() != NULL)
    typifyNode(node->initExpr());
}


void
Typifier::typify(ApplyNode* node)
{
  typifyNode(node->base());
  typifyNodeList(node->children());
}


void
Typifier::typify(ArrayNode* node)
{
  typifyNodeList(node->children());
}


void
Typifier::typify(AssignNode* node)
{
  typifyNode(node->lvalue());
  typifyNode(node->rvalue());
}


void
Typifier::typify(BinaryNode* node)
{
  typifyNode(node->left());
  typifyNode(node->right());
}


void
Typifier::typify(NegateNode* node)
{
  typifyNode(node->base());
}


void
Typifier::typify(IfNode* node)
{
  typifyNode(node->test());
  typifyNode(node->consequent());
  if (node->alternate())
    typifyNode(node->alternate());
}


void
Typifier::typify(KeyargNode* node)
{
  typifyNode(node->value());
}


void
Typifier::typify(MatchNode* node)
{
  typifyNode(node->expr());
  for (size_t i = 0; i < node->mappings().size(); i++) {
    typifyNode(node->mappings()[i].fConsequent);
  }
}


void
Typifier::typify(SelectNode* node)
{
  typifyNode(node->test());
  if (node->comparator() != NULL)
    typifyNode(node->comparator());

  for (size_t i = 0; i < node->mappings().size(); i++) {
    if (node->mappings()[i].fTestValues.empty()) {
      typifyNode(node->mappings()[i].fConsequent);
    }
    else {
      for (size_t j = 0; j < node->mappings()[i].fTestValues.size(); j++)
        typifyNode(node->mappings()[i].fTestValues[j]);
    }
    typifyNode(node->mappings()[i].fConsequent);
  }
}


void
Typifier::typify(OnNode* node)
{
  typifyNodeList(node->params());
  typifyNode(node->body());
}


void
Typifier::typify(RangeNode* node)
{
  typifyNode(node->from());
  typifyNode(node->to());
  if (node->by() != NULL)
    typifyNode(node->by());
}


void
Typifier::typify(ThenWhileNode* node)
{
  typifyNode(node->first());
  typifyNode(node->step());
  typifyNode(node->test());
}


void
Typifier::typify(TypeDefNode* node)
{
  // TODO
}


void
Typifier::typify(WhileNode* node)
{
  typifyNode(node->test());
  typifyNode(node->body());
}


void
Typifier::typify(VectorNode* node)
{
  typifyNodeList(node->children());
}


void
Typifier::typify(DictNode* node)
{
  typifyNodeList(node->children());
}


void
Typifier::typify(CastNode* node)
{
  typifyNode(node->base());
}


//------------------------------------------------------------------------------

void
Typifier::typify(BoolNode* node)
{
  // TODO
}


void
Typifier::typify(CharNode* node)
{
  // TODO
}


void
Typifier::typify(StringNode* node)
{
  // TODO
}


void
Typifier::typify(RationalNode* node)
{
  // TODO
}


void
Typifier::typify(RealNode* node)
{
  // TODO
}


void
Typifier::typify(IntNode* node)
{
  // TODO
}


void
Typifier::typify(KeywordNode* node)
{
  // TODO
}


void
Typifier::typify(UnitConstNode* node)
{
  typifyNode(node->value());
}
