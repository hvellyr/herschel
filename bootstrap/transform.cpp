/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.

   - look up all name references and complete their namespaces
 */

#include "transform.h"
#include "apt.h"
#include "errcodes.h"
#include "log.h"
#include "properties.h"
#include "scope.h"
#include "symbol.h"


#include <typeinfo>  //for 'typeid' to work


using namespace heather;

//----------------------------------------------------------------------------

Transformator::Transformator()
{
}


void
Transformator::transformNode(AptNode* node)
{
  node->transform(this);
}


void
Transformator::transform(CompileUnitNode* node)
{
  transformNodeList(node->children());
}


void
Transformator::transformNodeList(NodeList& nl)
{
  for (size_t i = 0; i < nl.size(); i++)
    transformNode(nl[i]);
}


//------------------------------------------------------------------------------

void
Transformator::transform(SymbolNode* node)
{
  // nothing to transform
}


void
Transformator::transform(ArraySymbolNode* node)
{
  // nothing to transform
}


//------------------------------------------------------------------------------

void
Transformator::transform(DefNode* node)
{
  transformNode(node->defNode());
}


void
Transformator::transform(LetNode* node)
{
  transformNode(node->defNode());
}


void
Transformator::transform(VardefNode* node)
{
  if (node->initExpr() != NULL)
    transformNode(node->initExpr());
}


void
Transformator::transform(FuncDefNode* node)
{
  transformNodeList(node->params());
  if (node->body() != NULL) {
    transformNode(node->body());
  }
}


void
Transformator::transform(FunctionNode* node)
{
  transformNodeList(node->params());
  transformNode(node->body());
}


void
Transformator::transform(SlotdefNode* node)
{
  // TODO
}


int
Transformator::findBlockSplitIndex(const NodeList& nodes)
{
  enum NodeMode
  {
    kMode_begin,
    kMode_let,
    kMode_onExit,
    kMode_other
  };

  NodeMode mode = kMode_begin;

  for (size_t i = 0; i < nodes.size(); i++) {
    const LetNode* letnd = dynamic_cast<const LetNode*>(nodes[i].obj());
    if (letnd != NULL) {
      switch (mode) {
      case kMode_begin:
        mode = kMode_let;
        break;
      case kMode_let:
        mode = kMode_let;
        break;
      case kMode_other:
        return i;
      case kMode_onExit:
        return i;
      }
    }
    else {
      const OnNode* onnd = dynamic_cast<const OnNode*>(nodes[i].obj());
      if (onnd != NULL) {
        switch (mode) {
        case kMode_begin:
        case kMode_let:
          if (onnd->key() == String("signal"))
            mode = kMode_let;
          else if (onnd->key() == String("exit"))
            mode = kMode_onExit;
          else
            mode = kMode_other;
          break;
        case kMode_other:
          if (onnd->key() == String("signal"))
            return i;
          else if (onnd->key() == String("exit"))
            return i;
          else
            mode = kMode_other;
          break;
        case kMode_onExit:
          return i;
        }
      }
      else {
        switch (mode) {
        case kMode_begin:
          mode = kMode_other;
          break;
        case kMode_let:
          mode = kMode_other;
          break;
        case kMode_other:
          return i;
        case kMode_onExit:
          return i;
        }
      }
    }
  }

  return -1;
}


void
Transformator::transform(BlockNode* node)
{
  const NodeList& nodes = node->children();
  int idx = findBlockSplitIndex(nodes);

  if (idx > 0) {
    Ptr<BlockNode> newBlock = new BlockNode(nodes[idx]->srcpos());
    for (size_t i = idx; i < nodes.size(); i++)
      newBlock->appendNode(nodes[idx]);

    node->children().resize(idx);
    node->appendNode(newBlock);
  }

  transformNodeList(node->children());
}


void
Transformator::transform(ParamNode* node)
{
  if (node->initExpr() != NULL)
    transformNode(node->initExpr());
}


void
Transformator::transform(ApplyNode* node)
{
  transformNode(node->base());
  transformNodeList(node->children());
}


void
Transformator::transform(ArrayNode* node)
{
  transformNodeList(node->children());
}


void
Transformator::transform(AssignNode* node)
{
  transformNode(node->lvalue());
  transformNode(node->rvalue());
}


void
Transformator::transform(BinaryNode* node)
{
  transformNode(node->left());
  transformNode(node->right());
}


void
Transformator::transform(NegateNode* node)
{
  transformNode(node->base());
}


void
Transformator::transform(IfNode* node)
{
  transformNode(node->test());
  transformNode(node->consequent());
  if (node->alternate())
    transformNode(node->alternate());
}


void
Transformator::transform(KeyargNode* node)
{
  transformNode(node->value());
}


void
Transformator::transform(MatchNode* node)
{
  transformNode(node->fExpr);
  for (size_t i = 0; i < node->fMappings.size(); i++) {
    transformNode(node->fMappings[i].fConsequent);
  }
}


void
Transformator::transform(SelectNode* node)
{
  transformNode(node->fTest);
  if (node->fComparator != NULL)
    transformNode(node->fComparator);

  for (size_t i = 0; i < node->fMappings.size(); i++) {
    if (node->fMappings[i].fTestValues.empty()) {
      transformNode(node->fMappings[i].fConsequent);
    }
    else {
      for (size_t j = 0; j < node->fMappings[i].fTestValues.size(); j++)
        transformNode(node->fMappings[i].fTestValues[j]);
    }
    transformNode(node->fMappings[i].fConsequent);
  }
}


void
Transformator::transform(OnNode* node)
{
  transformNodeList(node->params());
  transformNode(node->body());
}


void
Transformator::transform(RangeNode* node)
{
  transformNode(node->from());
  transformNode(node->to());
  if (node->by() != NULL)
    transformNode(node->by());
}


void
Transformator::transform(ThenWhileNode* node)
{
  transformNode(node->first());
  transformNode(node->step());
  transformNode(node->test());
}


void
Transformator::transform(TypeDefNode* node)
{
}


void
Transformator::transform(WhileNode* node)
{
  transformNode(node->test());
  transformNode(node->body());
}


void
Transformator::transform(VectorNode* node)
{
  transformNodeList(node->children());
}


void
Transformator::transform(DictNode* node)
{
  transformNodeList(node->children());
}


void
Transformator::transform(CastNode* node)
{
  transformNode(node->base());
}


//------------------------------------------------------------------------------

void
Transformator::transform(BoolNode* node)
{
  // Nothing to transform here
}


void
Transformator::transform(CharNode* node)
{
  // Nothing to transform here
}


void
Transformator::transform(StringNode* node)
{
  // Nothing to transform here
}


void
Transformator::transform(RationalNode* node)
{
  // Nothing to transform here
}


void
Transformator::transform(RealNode* node)
{
  // Nothing to transform here
}


void
Transformator::transform(IntNode* node)
{
  // Nothing to transform here
}


void
Transformator::transform(KeywordNode* node)
{
  // Nothing to transform here
}


void
Transformator::transform(UnitConstNode* node)
{
  transformNode(node->value());
}




