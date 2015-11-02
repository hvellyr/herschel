/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.

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
#include "utils.h"
#include "xmlout.h"


#include <typeinfo>  //for 'typeid' to work


using namespace herschel;


//----------------------------------------------------------------------------

TransformPass::TransformPass(int level)
  : AptNodeCompilePass(level)
{}


std::shared_ptr<AptNode>
TransformPass::doApply(std::shared_ptr<AptNode> src)
{
  Ptr<Transformator> tr = new Transformator;
  tr->transformNode(src);
  return src;
}


//----------------------------------------------------------------------------

Transformator::Transformator()
{
}


std::shared_ptr<AptNode>
Transformator::transformNode(std::shared_ptr<AptNode> node)
{
  return node->transform(this, node);
}


std::shared_ptr<AptNode>
Transformator::transform(std::shared_ptr<CompileUnitNode> node)
{
  transformNodeList(node->children());
  return node;
}


void
Transformator::transformNodeList(NodeList& nl)
{
  for (auto& nd : nl)
    nd = transformNode(nd);
}


//------------------------------------------------------------------------------

std::shared_ptr<AptNode>
Transformator::transform(std::shared_ptr<SymbolNode> node)
{
  // nothing to transform
  return node;
}


std::shared_ptr<AptNode>
Transformator::transform(std::shared_ptr<ArrayTypeNode> node)
{
  // nothing to transform
  return node;
}


std::shared_ptr<AptNode>
Transformator::transform(std::shared_ptr<TypeNode> node)
{
  // nothing to transform
  return node;
}


//------------------------------------------------------------------------------

std::shared_ptr<AptNode>
Transformator::transform(std::shared_ptr<DefNode> node)
{
  node->setDefNode(transformNode(node->defNode()));
  return node;
}


std::shared_ptr<AptNode>
Transformator::transform(std::shared_ptr<LetNode> node)
{
  node->setDefNode(transformNode(node->defNode()));
  return node;
}


std::shared_ptr<AptNode>
Transformator::transform(std::shared_ptr<VardefNode> node)
{
  if (node->initExpr())
    node->setInitExpr(transformNode(node->initExpr()));
  return node;
}


std::shared_ptr<AptNode>
Transformator::transform(std::shared_ptr<FuncDefNode> node)
{
  transformNodeList(node->params());
  if (node->body()) {
    node->setBody(transformNode(node->body()));
  }
  return node;
}


std::shared_ptr<AptNode>
Transformator::transform(std::shared_ptr<FunctionNode> node)
{
  transformNodeList(node->params());
  node->setBody(transformNode(node->body()));
  return node;
}


std::shared_ptr<AptNode>
Transformator::transform(std::shared_ptr<SlotdefNode> node)
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
    auto letnd = dynamic_cast<LetNode*>(nodes[i].get());
    if (letnd) {
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
      auto onnd = dynamic_cast<OnNode*>(nodes[i].get());
      if (onnd) {
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
  hr_assert(onnd->params().size() == 1);
  auto onPrmNode = dynamic_cast<ParamNode*>(onnd->params()[0].get());
  hr_assert(onPrmNode);

  auto initExpr = ( onPrmNode->initExpr()
                    ? onPrmNode->initExpr()
                    : std::make_shared<SymbolNode>(onPrmNode->srcpos(),
                                                   String("lang|unspecified")) );
  NodeList nl = vector_of<std::shared_ptr<AptNode> >
    (std::make_shared<LetNode>(
      std::make_shared<VardefNode>(onPrmNode->srcpos(),
                                   onPrmNode->name(), kNormalVar,
                                   K(isLocal),
                                   onPrmNode->type(),
                                   initExpr)))
    (onnd->body());

  node->children().clear();
  node->appendNodes(nl);

  transformNodeList(node->children());
}


std::shared_ptr<AptNode>
Transformator::transform(std::shared_ptr<BlockNode> node)
{
  const NodeList& nodes = node->children();
  if (nodes.size() == 1) {
    auto onnd = dynamic_cast<OnNode*>(nodes[0].get());
    if (onnd) {
      if (onnd->key() == Names::kSignalKeyword) {
        // if a block contains a single "on signal" node we can drop the
        // complete block, since there's no code in the scope which could
        // raise any signal.  So the signal code is effectively dead.  Print a
        // warning though.
        warningf(onnd->srcpos(), E_UnreachableCode,
                 "unreachable code in orphaned 'on signal' handler");
        return nullptr;
      }
      else if (onnd->key() == Names::kExitKeyword) {
        transformSingleOnExitBlock(node.get(), onnd);
        return node;
      }
    }
  }

  int idx = findBlockSplitIndex(nodes);
  if (idx > 0) {
    auto newBlock = std::make_shared<BlockNode>(nodes[idx]->srcpos());
    std::for_each(std::next(nodes.begin(), idx), nodes.end(),
                  [&](const NodeList::value_type& nd) {
                     newBlock->appendNode(nd);
                  });

    node->children().resize(idx);
    node->appendNode(newBlock);
  }

  transformNodeList(node->children());
  return node;
}


std::shared_ptr<AptNode>
Transformator::transform(std::shared_ptr<ParamNode> node)
{
  if (node->initExpr())
    node->setInitExpr(transformNode(node->initExpr()));
  return node;
}


std::shared_ptr<AptNode>
Transformator::transform(std::shared_ptr<ApplyNode> node)
{
  node->setBase(transformNode(node->base()));
  transformNodeList(node->children());
  return node;
}


std::shared_ptr<AptNode>
Transformator::transform(std::shared_ptr<ArrayNode> node)
{
  transformNodeList(node->children());
  return node;
}


std::shared_ptr<AptNode>
Transformator::transform(std::shared_ptr<AssignNode> node)
{
  node->setLvalue(transformNode(node->lvalue()));
  node->setRvalue(transformNode(node->rvalue()));
  return node;
}


std::shared_ptr<AptNode>
Transformator::transform(std::shared_ptr<BinaryNode> node)
{
  node->setLeft(transformNode(node->left()));
  node->setRight(transformNode(node->right()));
  return node;
}


std::shared_ptr<AptNode>
Transformator::transform(std::shared_ptr<UnaryNode> node)
{
  node->setBase(transformNode(node->base()));
  return node;
}


std::shared_ptr<AptNode>
Transformator::transform(std::shared_ptr<IfNode> node)
{
  node->setTest(transformNode(node->test()));
  node->setConsequent(transformNode(node->consequent()));
  if (node->alternate())
    node->setAlternate(transformNode(node->alternate()));
  return node;
}


std::shared_ptr<AptNode>
Transformator::transform(std::shared_ptr<KeyargNode> node)
{
  node->setValue(transformNode(node->value()));
  return node;
}


std::shared_ptr<AptNode>
Transformator::transform(std::shared_ptr<MatchNode> node)
{
  node->setExpr(transformNode(node->expr()));
  for (size_t i = 0; i < node->mappingCount(); i++) {
    node->setConsequentAt(i, transformNode(node->mappingAt(i).fConsequent));
  }

  std::shared_ptr<AptNode> rootIf;
  std::shared_ptr<IfNode> lastIf;
  std::shared_ptr<AptNode> elseAlternate;

  for (size_t i = 0; i < node->mappingCount(); i++) {
    if (node->mappingAt(i).fMatchType.isAny())
    {
      if (elseAlternate) {
        errorf(node->mappingAt(i).fSrcPos, E_MatchAmbiguousType,
               "redefinition of catch-all lang|Any branch in match");
        errorf(elseAlternate->srcpos(), E_MatchAmbiguousType,
               "previous Any branch was here");
      }
      else
        elseAlternate = node->mappingAt(i).fConsequent;
    }
    else {
      auto isaCall = std::make_shared<ApplyNode>(node->mappingAt(i).fSrcPos,
                                                 std::make_shared<SymbolNode>(node->mappingAt(i).fSrcPos,
                                                                Names::kLangIsaQ));
      isaCall->appendNode(node->expr()->clone());
      isaCall->appendNode(std::make_shared<TypeNode>(node->mappingAt(i).fSrcPos,
                                       node->mappingAt(i).fMatchType));

      auto newIf = std::make_shared<IfNode>(node->mappingAt(i).fSrcPos,
                                            isaCall,
                                            node->mappingAt(i).fConsequent, nullptr);
      if (lastIf) {
        lastIf->setAlternate(newIf);
        lastIf = newIf;
      }
      else
        rootIf = lastIf = newIf;
    }
  }

  if (!elseAlternate)
    elseAlternate = std::make_shared<SymbolNode>(node->srcpos(),
                                   Names::kLangUnspecified);

  if (lastIf)
    lastIf->setAlternate(elseAlternate);
  else
    rootIf = elseAlternate;

  lastIf = nullptr;
  elseAlternate = nullptr;

  return rootIf;
}


std::shared_ptr<AptNode>
Transformator::transform(std::shared_ptr<SelectNode> node)
{
  node->setTest(transformNode(node->test()));
  if (node->comparator())
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


std::shared_ptr<AptNode>
Transformator::transform(std::shared_ptr<OnNode> node)
{
  transformNodeList(node->params());
  node->setBody(transformNode(node->body()));
  return node;
}


std::shared_ptr<AptNode>
Transformator::transform(std::shared_ptr<RangeNode> node)
{
  node->setFrom(transformNode(node->from()));
  node->setTo(transformNode(node->to()));
  if (node->by())
    node->setBy(transformNode(node->by()));
  return node;
}


std::shared_ptr<AptNode>
Transformator::transform(std::shared_ptr<TypeDefNode> node)
{
  transformNodeList(node->params());
  transformNodeList(node->slots());
  transformNodeList(node->onExprs());

  return node;
}


std::shared_ptr<AptNode>
Transformator::transform(std::shared_ptr<WhileNode> node)
{
  node->setTest(transformNode(node->test()));
  node->setBody(transformNode(node->body()));
  return node;
}


std::shared_ptr<AptNode>
Transformator::transform(std::shared_ptr<VectorNode> node)
{
  transformNodeList(node->children());
  return node;
}


std::shared_ptr<AptNode>
Transformator::transform(std::shared_ptr<DictNode> node)
{
  transformNodeList(node->children());
  return node;
}


std::shared_ptr<AptNode>
Transformator::transform(std::shared_ptr<CastNode> node)
{
  node->setBase(transformNode(node->base()));
  return node;
}


//------------------------------------------------------------------------------

std::shared_ptr<AptNode>
Transformator::transform(std::shared_ptr<BoolNode> node)
{
  // Nothing to transform here
  return node;
}


std::shared_ptr<AptNode>
Transformator::transform(std::shared_ptr<CharNode> node)
{
  // Nothing to transform here
  return node;
}


std::shared_ptr<AptNode>
Transformator::transform(std::shared_ptr<StringNode> node)
{
  // Nothing to transform here
  return node;
}


std::shared_ptr<AptNode>
Transformator::transform(std::shared_ptr<RationalNode> node)
{
  // Nothing to transform here
  return node;
}


std::shared_ptr<AptNode>
Transformator::transform(std::shared_ptr<RealNode> node)
{
  // Nothing to transform here
  return node;
}


std::shared_ptr<AptNode>
Transformator::transform(std::shared_ptr<IntNode> node)
{
  // Nothing to transform here
  return node;
}


std::shared_ptr<AptNode>
Transformator::transform(std::shared_ptr<KeywordNode> node)
{
  // Nothing to transform here
  return node;
}


std::shared_ptr<AptNode>
Transformator::transform(std::shared_ptr<UnitConstNode> node)
{
  node->setValue(transformNode(node->value()));
  return node;
}


std::shared_ptr<AptNode>
Transformator::transform(std::shared_ptr<UndefNode> node)
{
  // Nothing to transform here
  return node;
}


std::shared_ptr<AptNode>
Transformator::transform(std::shared_ptr<SlotRefNode> node)
{
  node->setBase(transformNode(node->base()));
  return node;
}
