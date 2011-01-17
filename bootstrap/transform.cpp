/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   - precompile transformation (simplification, etc.)
 */

#include "transform.h"
#include "apt.h"
#include "errcodes.h"
#include "log.h"
#include "predefined.h"
#include "properties.h"
#include "scope.h"
#include "symbol.h"


#include <typeinfo>  //for 'typeid' to work


using namespace herschel;


//----------------------------------------------------------------------------

TransformPass::TransformPass(int level)
  : AptNodeCompilePass(level)
{}


AptNode*
TransformPass::doApply(AptNode* src)
{
  Ptr<AptNode> node = src;
  Ptr<Transformator> tr = new Transformator;
  tr->transformNode(node);
  return node.release();
}


//----------------------------------------------------------------------------

Transformator::Transformator()
{
}


AptNode*
Transformator::transformNode(AptNode* node)
{
  return node->transform(this);
}


AptNode*
Transformator::transform(CompileUnitNode* node)
{
  transformNodeList(node->children());
  return node;
}


void
Transformator::transformNodeList(NodeList& nl)
{
  for (size_t i = 0; i < nl.size(); i++) {
    nl[i] = transformNode(nl[i]);
  }
}


//------------------------------------------------------------------------------

AptNode*
Transformator::transform(SymbolNode* node)
{
  // nothing to transform
  return node;
}


AptNode*
Transformator::transform(ArrayTypeNode* node)
{
  // nothing to transform
  return node;
}


AptNode*
Transformator::transform(TypeNode* node)
{
  // nothing to transform
  return node;
}


//------------------------------------------------------------------------------

AptNode*
Transformator::transform(DefNode* node)
{
  node->setDefNode(transformNode(node->defNode()));
  return node;
}


AptNode*
Transformator::transform(LetNode* node)
{
  node->setDefNode(transformNode(node->defNode()));
  return node;
}


AptNode*
Transformator::transform(VardefNode* node)
{
  if (node->initExpr() != NULL)
    node->setInitExpr(transformNode(node->initExpr()));
  return node;
}


AptNode*
Transformator::transform(FuncDefNode* node)
{
  transformNodeList(node->params());
  if (node->body() != NULL) {
    node->setBody(transformNode(node->body()));
  }
  return node;
}


AptNode*
Transformator::transform(FunctionNode* node)
{
  transformNodeList(node->params());
  node->setBody(transformNode(node->body()));
  return node;
}


AptNode*
Transformator::transform(SlotdefNode* node)
{
  // TODO
  return node;
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
          if (onnd->key() == Names::kSignalKeyword)
            mode = kMode_let;
          else if (onnd->key() == Names::kExitKeyword)
            mode = kMode_onExit;
          else
            mode = kMode_other;
          break;
        case kMode_other:
          if (onnd->key() == Names::kSignalKeyword)
            return i;
          else if (onnd->key() == Names::kExitKeyword)
            return i;
          else
            mode = kMode_other;
          break;
        case kMode_onExit:
          return i;
        }
      }
      else {
        mode = kMode_other;
      }
    }
  }

  return -1;
}


void
Transformator::transformSingleOnExitBlock(BlockNode* node, OnNode* onnd)
{
  // If there's no code other than the 'on exit' handler in the scope we can
  // inline the handler codes directly.  Make the handler's parameter (the
  // return value of the block) a local variable and initialize it to
  // 'lang|unspecified' (the value of an empty blocks) unless the parameter
  // has a default value already.  Issue a warning anyway, since this
  // situation is most likely a programming error.
  warningf(onnd->srcpos(), E_OrphanedOnExit,
           "orphaned 'on exit' handler parameter");
  assert(onnd->params().size() == 1);
  ParamNode* onPrmNode = dynamic_cast<ParamNode*>(onnd->params()[0].obj());
  assert(onPrmNode != NULL);

  Ptr<AptNode> initExpr = ( onPrmNode->initExpr() != NULL
                            ? onPrmNode->initExpr()
                            : new SymbolNode(onPrmNode->srcpos(),
                                             String("lang|unspecified")) );
  NodeList nl;
  nl.push_back(new LetNode(new VardefNode(onPrmNode->srcpos(),
                                          onPrmNode->name(), kNormalVar, true,
                                          onPrmNode->type(),
                                          initExpr)));
  nl.push_back(onnd->body());

  node->children().clear();
  node->appendNodes(nl);

  transformNodeList(node->children());
}


AptNode*
Transformator::transform(BlockNode* node)
{
  NodeList& nodes = node->children();
  if (nodes.size() == 1) {
    OnNode* onnd = dynamic_cast<OnNode*>(nodes[0].obj());
    if (onnd != NULL) {
      if (onnd->key() == Names::kSignalKeyword) {
        // if a block contains a single "on signal" node we can drop the
        // complete block, since there's no code in the scope which could
        // raise any signal.  So the signal code is effectively dead.  Print a
        // warning though.
        warningf(onnd->srcpos(), E_UnreachableCode,
                 "unreachable code in orphaned 'on signal' handler");
        return NULL;
      }
      else if (onnd->key() == Names::kExitKeyword) {
        transformSingleOnExitBlock(node, onnd);
        return node;
      }
    }
  }

  int idx = findBlockSplitIndex(nodes);
//  fprintf(stderr, "SPLIT AT: %d\n", idx);
  if (idx > 0) {
    Ptr<BlockNode> newBlock = new BlockNode(nodes[idx]->srcpos());
    for (size_t i = idx; i < nodes.size(); i++)
      newBlock->appendNode(nodes[i]);

    node->children().resize(idx);
    node->appendNode(newBlock);
  }

  transformNodeList(node->children());
  return node;
}


AptNode*
Transformator::transform(ParamNode* node)
{
  if (node->initExpr() != NULL)
    node->setInitExpr(transformNode(node->initExpr()));
  return node;
}


AptNode*
Transformator::transform(ApplyNode* node)
{
  node->setBase(transformNode(node->base()));
  transformNodeList(node->children());
  return node;
}


AptNode*
Transformator::transform(ArrayNode* node)
{
  transformNodeList(node->children());
  return node;
}


AptNode*
Transformator::transform(AssignNode* node)
{
  node->setLvalue(transformNode(node->lvalue()));
  node->setRvalue(transformNode(node->rvalue()));
  return node;
}


AptNode*
Transformator::transform(BinaryNode* node)
{
  node->setLeft(transformNode(node->left()));
  node->setRight(transformNode(node->right()));
  return node;
}


AptNode*
Transformator::transform(NegateNode* node)
{
  node->setBase(transformNode(node->base()));
  return node;
}


AptNode*
Transformator::transform(IfNode* node)
{
  node->setTest(transformNode(node->test()));
  node->setConsequent(transformNode(node->consequent()));
  if (node->alternate())
    node->setAlternate(transformNode(node->alternate()));
  return node;
}


AptNode*
Transformator::transform(KeyargNode* node)
{
  node->setValue(transformNode(node->value()));
  return node;
}


AptNode*
Transformator::transform(MatchNode* node)
{
  node->setExpr(transformNode(node->expr()));
  for (size_t i = 0; i < node->mappingCount(); i++) {
    node->setConsequentAt(i, transformNode(node->mappingAt(i).fConsequent));
  }
  return node;
}


AptNode*
Transformator::transform(SelectNode* node)
{
  node->setTest(transformNode(node->test()));
  if (node->comparator() != NULL)
    node->setComparator(transformNode(node->comparator()));

  for (size_t i = 0; i < node->mappingCount(); i++) {
    if (node->mappingAt(i).fTestValues.empty()) {
      node->setConsequentAt(i, transformNode(node->mappingAt(i).fConsequent));
    }
    else {
      for (size_t j = 0; j < node->mappingAt(i).fTestValues.size(); j++)
        node->setTestValueAt(i, j, transformNode(node->mappingAt(i).fTestValues[j]));
    }
    node->setConsequentAt(i, transformNode(node->mappingAt(i).fConsequent));
  }
  return node;
}


AptNode*
Transformator::transform(OnNode* node)
{
  transformNodeList(node->params());
  node->setBody(transformNode(node->body()));
  return node;
}


AptNode*
Transformator::transform(RangeNode* node)
{
  node->setFrom(transformNode(node->from()));
  node->setTo(transformNode(node->to()));
  if (node->by() != NULL)
    node->setBy(transformNode(node->by()));
  return node;
}


AptNode*
Transformator::transform(TypeDefNode* node)
{
  // TODO
  return node;
}


AptNode*
Transformator::transform(WhileNode* node)
{
  node->setTest(transformNode(node->test()));
  node->setBody(transformNode(node->body()));
  return node;
}


AptNode*
Transformator::transform(VectorNode* node)
{
  transformNodeList(node->children());
  return node;
}


AptNode*
Transformator::transform(DictNode* node)
{
  transformNodeList(node->children());
  return node;
}


AptNode*
Transformator::transform(CastNode* node)
{
  node->setBase(transformNode(node->base()));
  return node;
}


//------------------------------------------------------------------------------

AptNode*
Transformator::transform(BoolNode* node)
{
  // Nothing to transform here
  return node;
}


AptNode*
Transformator::transform(CharNode* node)
{
  // Nothing to transform here
  return node;
}


AptNode*
Transformator::transform(StringNode* node)
{
  // Nothing to transform here
  return node;
}


AptNode*
Transformator::transform(RationalNode* node)
{
  // Nothing to transform here
  return node;
}


AptNode*
Transformator::transform(RealNode* node)
{
  // Nothing to transform here
  return node;
}


AptNode*
Transformator::transform(IntNode* node)
{
  // Nothing to transform here
  return node;
}


AptNode*
Transformator::transform(KeywordNode* node)
{
  // Nothing to transform here
  return node;
}


AptNode*
Transformator::transform(UnitConstNode* node)
{
  node->setValue(transformNode(node->value()));
  return node;
}




