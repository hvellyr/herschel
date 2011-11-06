/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "traverse.h"
#include "apt.h"


//----------------------------------------------------------------------------

using namespace herschel;


Traversator::Traversator(TraverseDelegate& delegate)
  : fDelegate(delegate)
{
}


void
Traversator::traverseNode(AptNode* node)
{
  if (fDelegate.preApply(node)) {
    node->traverse(this);
    fDelegate.postApply(node);
  }
}


void
Traversator::traverse(CompileUnitNode* node)
{
  traverseNodeList(node->children());
}


void
Traversator::traverseNodeList(NodeList& nl)
{
  for (size_t i = 0; i < nl.size(); i++)
    traverseNode(nl[i]);
}


//------------------------------------------------------------------------------

void
Traversator::traverse(SymbolNode* node)
{
}


void
Traversator::traverse(ArrayTypeNode* node)
{
  traverseNode(node->typeNode());
}


void
Traversator::traverse(TypeNode* node)
{
}


//------------------------------------------------------------------------------

void
Traversator::traverse(DefNode* node)
{
  traverseNode(node->defNode());
}


void
Traversator::traverse(LetNode* node)
{
  traverseNode(node->defNode());
}


void
Traversator::traverse(VardefNode* node)
{
  if (node->initExpr() != NULL)
    traverseNode(node->initExpr());
}


void
Traversator::traverse(FuncDefNode* node)
{
  traverseNodeList(node->params());
  if (node->body() != NULL)
    traverseNode(node->body());
}


void
Traversator::traverse(FunctionNode* node)
{
  traverseNodeList(node->params());
  if (node->body() != NULL)
    traverseNode(node->body());
}


void
Traversator::traverse(SlotdefNode* node)
{
  if (node->initExpr() != NULL)
    traverseNode(node->initExpr());
}


void
Traversator::traverse(BlockNode* node)
{
  traverseNodeList(node->children());
}


void
Traversator::traverse(ParamNode* node)
{
  if (node->initExpr() != NULL)
    traverseNode(node->initExpr());
}


void
Traversator::traverse(ApplyNode* node)
{
  traverseNode(node->base());
  traverseNodeList(node->children());
}


void
Traversator::traverse(ArrayNode* node)
{
  traverseNodeList(node->children());
}


void
Traversator::traverse(AssignNode* node)
{
  traverseNode(node->lvalue());
  traverseNode(node->rvalue());
}


void
Traversator::traverse(BinaryNode* node)
{
  traverseNode(node->left());
  traverseNode(node->right());
}


void
Traversator::traverse(UnaryNode* node)
{
  traverseNode(node->base());
}


void
Traversator::traverse(IfNode* node)
{
  traverseNode(node->test());
  traverseNode(node->consequent());
  if (node->alternate())
    traverseNode(node->alternate());
}


void
Traversator::traverse(KeyargNode* node)
{
  traverseNode(node->value());
}


void
Traversator::traverse(MatchNode* node)
{
  traverseNode(node->expr());
  for (size_t i = 0; i < node->mappingCount(); i++) {
    traverseNode(node->mappingAt(i).fConsequent);
  }
}


void
Traversator::traverse(SelectNode* node)
{
  traverseNode(node->test());
  if (node->comparator() != NULL)
    traverseNode(node->comparator());

  for (size_t i = 0; i < node->mappingCount(); i++) {
    if (node->mappingAt(i).fTestValues.empty()) {
      traverseNode(node->mappingAt(i).fConsequent);
    }
    else {
      for (size_t j = 0; j < node->mappingAt(i).fTestValues.size(); j++)
        traverseNode(node->mappingAt(i).fTestValues[j]);
    }
    traverseNode(node->mappingAt(i).fConsequent);
  }
}


void
Traversator::traverse(OnNode* node)
{
  traverseNodeList(node->params());
  traverseNode(node->body());
}


void
Traversator::traverse(RangeNode* node)
{
  traverseNode(node->from());
  traverseNode(node->to());
  if (node->by() != NULL)
    traverseNode(node->by());
}


void
Traversator::traverse(TypeDefNode* node)
{
  // TODO
}


void
Traversator::traverse(WhileNode* node)
{
  traverseNode(node->test());
  traverseNode(node->body());
}


void
Traversator::traverse(VectorNode* node)
{
  traverseNodeList(node->children());
}


void
Traversator::traverse(DictNode* node)
{
  traverseNodeList(node->children());
}


void
Traversator::traverse(CastNode* node)
{
  traverseNode(node->base());
}


void
Traversator::traverse(BoolNode* node)
{
}


void
Traversator::traverse(CharNode* node)
{
}


void
Traversator::traverse(StringNode* node)
{
}


void
Traversator::traverse(RationalNode* node)
{
}


void
Traversator::traverse(RealNode* node)
{
}


void
Traversator::traverse(IntNode* node)
{
}


void
Traversator::traverse(KeywordNode* node)
{
}


void
Traversator::traverse(UndefNode* node)
{
}


void
Traversator::traverse(UnitConstNode* node)
{
  traverseNode(node->value());
}


void
Traversator::traverse(SlotRefNode* node)
{
  traverseNode(node->base());
}
