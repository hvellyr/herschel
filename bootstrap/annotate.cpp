/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.

   - look up all name references and complete their namespaces
 */

#include "annotate.hpp"

#include "ast.hpp"
#include "compiler.hpp"
#include "errcodes.hpp"
#include "log.hpp"
#include "predefined.hpp"
#include "properties.hpp"
#include "rootscope.hpp"
#include "scope.hpp"
#include "symbol.hpp"
#include "traverse.hpp"

#include <algorithm>
#include <iterator>
#include <typeinfo>  //for 'typeid' to work


namespace herschel {


template <typename T>
struct NodeAnnotator {
  static void annotate(Annotator* ann, T node)
  {
    // Nothing to annotate here
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<CompileUnitNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<CompileUnitNode> node)
  {
    ann->annotateNodeList(node->children(), !K(markTailPos), !K(markSingleType));
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<ArrayTypeNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<ArrayTypeNode> node)
  {
    node->setTypeNode(ann->annotateNode(node->typeNode()));
  }
};


static void annotateVardefNode(Annotator* ann, std::shared_ptr<VardefNode> node,
                               bool isLocal)
{
  if (isLocal) {
    if (!node->scope()->checkForRedefinition(node->srcpos(), Scope::kNormal,
                                             node->name()))
      node->scope()->registerVar(node->srcpos(), node->name(), node);
  }

  if (node->initExpr()) {
    node->setInitExpr(ann->annotateNode(node->initExpr()));
  }
}


static void annotateFuncdefNode(Annotator* ann, std::shared_ptr<FuncDefNode> node,
                                bool isLocal)
{
  if (isLocal) {
    node->scope()->registerFunction(node->srcpos(), node->name(), node);
  }
  else if (node->isMethod()) {
    auto var = node->scope()->lookupVarOrFunc(node->srcpos(), node->name(),
                                              K(showAmbiguousSymDef));
    if (!var) {
      HR_LOG(kError, node->srcpos(), E_NoGenericFunction)
          << "No generic function definition found for method";
    }
    else if (auto funcdef = dynamic_cast<const FuncDefNode*>(var)) {
      if (!funcdef->isGeneric()) {
        HR_LOG(kError, node->srcpos(), E_BadGenericReferrer)
            << "Bad method binding type (referred symbol is not a generic function).";
        HR_LOG(kError, var->srcpos(), E_BadGenericReferrer)
            << "Referred function definition was here";
      }
    }
    else {
      HR_LOG(kError, node->srcpos(), E_BadGenericReferrer)
          << "Bad method binding type (referred symbol is not a generic function).";
      HR_LOG(kError, var->srcpos(), E_BadGenericReferrer)
          << "Referred symbol definition was here";
    }
  }

  ann->annotateNodeList(node->params(), !K(markTailPos), K(markSingleType));
  if (node->body()) {
    node->body()->setIsInTailPos(true);
    node->setBody(ann->annotateNode(node->body()));
  }
}


template <>
struct NodeAnnotator<std::shared_ptr<DefNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<DefNode> node)
  {
    auto vardefNode = std::dynamic_pointer_cast<VardefNode>(node->defNode());
    if (vardefNode) {
      annotateVardefNode(ann, vardefNode, !K(isLocal));
      return;
    }

    auto funcNode = std::dynamic_pointer_cast<FuncDefNode>(node->defNode());
    if (funcNode) {
      annotateFuncdefNode(ann, funcNode, !K(isLocal));
      return;
    }

    node->setDefNode(ann->annotateNode(node->defNode()));
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<LetNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<LetNode> node)
  {
    auto vardefNode = std::dynamic_pointer_cast<VardefNode>(node->defNode());
    if (vardefNode) {
      annotateVardefNode(ann, vardefNode, K(isLocal));
      return;
    }

    auto funcNode = std::dynamic_pointer_cast<FuncDefNode>(node->defNode());
    if (funcNode) {
      annotateFuncdefNode(ann, funcNode, K(isLocal));
      return;
    }

    node->setDefNode(ann->annotateNode(node->defNode()));
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<FunctionNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<FunctionNode> node)
  {
    ann->annotateNodeList(node->params(), !K(markTailPos), K(markSingleType));
    if (node->body()) {
      node->body()->setIsInTailPos(true);
      node->setBody(ann->annotateNode(node->body()));
    }
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<BlockNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<BlockNode> node)
  {
    ann->annotateNodeList(node->children(), node->isInTailPos(), !K(markSingleType));
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<ParamNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<ParamNode> node)
  {
    if (!node->scope()->checkForRedefinition(node->srcpos(), Scope::kNormal,
                                             node->name()))
      node->scope()->registerVar(node->srcpos(), node->name(), node);

    if (node->initExpr()) {
      node->setInitExpr(ann->annotateNode(node->initExpr()));
    }
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<ApplyNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<ApplyNode> node)
  {
    ann->annotateNodeList(node->children(), !K(markTailPos), K(markSingleType));

    if (node->isSimpleCall()) {
      // don't call ann->annotateNode(node->base()) here to avoid
      // re-forming it into a slotref.

      auto funcName = node->simpleCallName();
      auto varNode = node->scope()->lookupVarOrFunc(node->srcpos(), funcName,
                                                    !K(showAmbiguousSymDef));
      // if not such binding is found, but the name looks like it has
      // a namespace, and the lastName of this name is known, then
      // let's rewrite this expression into a parameter passing
      // function call.  E.g.
      //
      // foo.bar()     => bar(foo)
      // foo.bar(gaz)  => bar(foo, gaz)
      if (!varNode && hasNamespace(funcName)) {
        auto lastName = baseName(funcName);
        if (auto varNode = node->scope()->lookupVarOrFunc(node->srcpos(), lastName,
                                                          !K(showAmbiguousSymDef))) {

          auto baseNode = makeSymbolNode(node->scope(), node->srcpos(), lastName);
          baseNode->setIsRemoveable(node->isRemoveable());
          node->setBase(baseNode);

          auto funcNameNode =
              makeSymbolNode(node->scope(), node->srcpos(), nsName(funcName));
          funcNameNode->setIsRemoveable(node->isRemoveable());
          node->children().insert(node->children().begin(), funcNameNode);

          // re-annotate the rewritten code
          node->setBase(ann->annotateNode(node->base()));
          ann->annotateNodeList(node->children(), !K(markTailPos), K(markSingleType));
        }
      }
      else {
        // handled in later phases
      }
    }
    else {
      node->setBase(ann->annotateNode(node->base()));
    }
  }
};


namespace {
  std::shared_ptr<AstNode> unrollSlotRefs(std::shared_ptr<AstNode> node,
                                          const String& name)
  {
    if (hasNamespace(name)) {
      auto slotName = baseName(name);
      auto rootName = nsName(name);

      if (!slotName.isEmpty()) {
        auto var = node->scope()->lookupVarOrFunc(node->srcpos(), rootName,
                                                  K(showAmbiguouseSymDef));
        auto slotrefNd = var ? makeSymbolNode(node->scope(), node->srcpos(), rootName)
                             : unrollSlotRefs(node, rootName);
        slotrefNd->setIsRemoveable(node->isRemoveable());
        return makeSlotRefNode(node->scope(), node->srcpos(), slotrefNd, slotName);
      }
    }

    auto symNd = makeSymbolNode(node->scope(), node->srcpos(), name);
    symNd->setIsRemoveable(node->isRemoveable());
    return symNd;
  }
}  // namespace


template <>
struct NodeAnnotator<std::shared_ptr<SymbolNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<SymbolNode> node)
  {
    auto var = node->scope()->lookupVarOrFunc(node->srcpos(), node->name(),
                                              K(showAmbiguousSymDef));
    if (!var && hasNamespace(node->name())) {
      ann->replaceNode(ann->annotateNode(unrollSlotRefs(node, node->name())));
    }
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<WeakNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<WeakNode> node)
  {
    if (node->refNode())
      node->setRefNode(ann->annotateNode(node->refNode()));
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<ArrayNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<ArrayNode> node)
  {
    ann->annotateNodeList(node->children(), !K(markTailPos), K(markSingleType));
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<AssignNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<AssignNode> node)
  {
    node->setLvalue(ann->annotateNode(node->lvalue()));
    node->setRvalue(ann->annotateNode(node->rvalue()));
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<BinaryNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<BinaryNode> node)
  {
    node->setLeft(ann->annotateNode(node->left()));
    node->setRight(ann->annotateNode(node->right()));
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<UnaryNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<UnaryNode> node)
  {
    node->setBase(ann->annotateNode(node->base()));
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<IfNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<IfNode> node)
  {
    node->setTest(ann->annotateNode(node->test()));

    node->consequent()->setIsInTailPos(node->isInTailPos());
    node->setConsequent(ann->annotateNode(node->consequent()));
    if (node->alternate()) {
      node->alternate()->setIsInTailPos(node->isInTailPos());
      node->setAlternate(ann->annotateNode(node->alternate()));
    }
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<KeyargNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<KeyargNode> node)
  {
    node->setValue(ann->annotateNode(node->value()));
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<SelectNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<SelectNode> node)
  {
    // TODO : set tail node position
    node->setTest(ann->annotateNode(node->test()));
    if (node->comparator())
      node->setComparator(ann->annotateNode(node->comparator()));

    for (size_t i = 0; i < node->mappingCount(); i++) {
      if (node->mappingAt(i).fTestValues.empty()) {
        node->mappingAt(i).fConsequent =
            ann->annotateNode(node->mappingAt(i).fConsequent);
      }
      else {
        for (auto& testValue : node->mappingAt(i).fTestValues)
          testValue = ann->annotateNode(testValue);
      }
      node->mappingAt(i).fConsequent = ann->annotateNode(node->mappingAt(i).fConsequent);
    }
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<RangeNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<RangeNode> node)
  {
    // TODO : set tail node position
    node->setFrom(ann->annotateNode(node->from()));
    node->setTo(ann->annotateNode(node->to()));
    if (node->by())
      node->setBy(ann->annotateNode(node->by()));
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<TypeDefNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<TypeDefNode> node)
  {
    // TODO : set tail node position

    // don't re-register the type if global; it is registered in pass2 already
    // node->scope()->registerType(node->srcpos(), node->name(), node->defType());
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<WhileNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<WhileNode> node)
  {
    node->setTest(ann->annotateNode(node->test()));

    node->body()->setIsInTailPos(node->isInTailPos());
    node->setBody(ann->annotateNode(node->body()));
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<VectorNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<VectorNode> node)
  {
    ann->annotateNodeList(node->children(), !K(markTailPos), K(markSingleType));
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<DictNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<DictNode> node)
  {
    ann->annotateNodeList(node->children(), !K(markTailPos), K(markSingleType));
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<CastNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<CastNode> node)
  {
    node->setBase(ann->annotateNode(node->base()));
  }
};


template <>
struct NodeAnnotator<std::shared_ptr<SlotRefNode>> {
  static void annotate(Annotator* ann, std::shared_ptr<SlotRefNode> node)
  {
    node->setBase(ann->annotateNode(node->base()));
  }
};


//----------------------------------------------------------------------------

AnnotatePass::AnnotatePass(int level, Compiler& compiler)
    : AstNodeCompilePass(level)
    , fCompiler(compiler)
{
}


std::shared_ptr<AstNode> AnnotatePass::doApply(std::shared_ptr<AstNode> src)
{
  Annotator an{fCompiler};
  src = an.annotateNode(src);
  return src;
}


//----------------------------------------------------------------------------

Annotator::Annotator(Compiler& compiler)
    : fCompiler(compiler)
{
}


std::shared_ptr<AstNode> Annotator::annotateNode(std::shared_ptr<AstNode> node)
{
  dispatchNode<void>(node,
                     [&](auto nd) { NodeAnnotator<decltype(nd)>::annotate(this, nd); });
  auto retv = fNewNode ? fNewNode : node;
  fNewNode = nullptr;
  return retv;
}


void Annotator::annotateNodeList(NodeList& nl, bool marktailpos, bool marksingletype)
{
  const size_t nlsize = nl.size();

  for (size_t i = 0; i < nlsize; i++) {
    if (marktailpos && i == nlsize - 1)
      nl[i]->setIsInTailPos(true);

    if (marksingletype)
      nl[i]->setIsSingleTypeRequired(true);

    nl[i] = annotateNode(nl[i]);
  }
}


void Annotator::replaceNode(std::shared_ptr<AstNode> newNode)
{
  hr_assert(!fNewNode);

  fNewNode = newNode;
}


}  // namespace herschel
