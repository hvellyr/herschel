/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.

   - look up all name references and complete their namespaces
*/

#include "typify.hpp"

#include "annotate.hpp"
#include "ast.hpp"
#include "codegen-utils.hpp"
#include "errcodes.hpp"
#include "log.hpp"
#include "port.hpp"
#include "predefined.hpp"
#include "properties.hpp"
#include "rootscope.hpp"
#include "scope.hpp"
#include "str.hpp"
#include "symbol.hpp"
#include "traverse.hpp"
#include "typectx.hpp"
#include "typeprops.hpp"
#include "utils.hpp"
#include "xmlrenderer.hpp"

#include <set>
#include <unordered_map>


namespace herschel {
namespace {
  void trackMoveablePositions(Typifier* typf, std::shared_ptr<AstNode> node)
  {
    if (auto binding = std::dynamic_pointer_cast<MoveableBinding>(node)) {
      auto iReferer = typf->fBindings.find(binding.get());
      if (iReferer != end(typf->fBindings)) {
        Typifier::BindingsUse& use = iReferer->second;
        use.fSymbol->setIsInMovePos(true);
        const_cast<MoveableBinding*>(iReferer->first)
            ->setWillBeMoved(use.fInOwnershipTransferContext);
      }
    }
  }

  std::shared_ptr<AstNode> generateFinalizer(Typifier* typf,
                                             std::shared_ptr<AstNode> node)
  {
    if (auto binding = std::dynamic_pointer_cast<MoveableBinding>(node)) {
      // things to be moved are not subject to destruction
      if (binding->willBeMoved()) {
        return nullptr;
      }

      auto finalizerMark = Names::kFinalizerMarker + binding->symbolName();

      std::shared_ptr<AstNode> freeExpr;

      auto ty = node->type();
      if (ty.isValueType()) {
        if (ty.isRecord()) {
          auto deinitExpr = makeApplyNode(
              node->scope(), node->srcpos(),
              makeSymbolNode(node->scope(), node->srcpos(), Names::kDeinitFuncName));
          auto bindingRefNd =
              makeSymbolNode(node->scope(), node->srcpos(), binding->symbolName());
          bindingRefNd->setIsInMovePos(true);
          deinitExpr->appendNode(bindingRefNd);

          freeExpr = deinitExpr;

          binding->setWillBeMoved(true);
        }
        else if (ty.isPlainType()) {
          return {};
        }
        else {
          // TODO handle ANY/union/etc.
          return {};
        }
      }
      else {
        // auto deallocExpr = makeApplyNode(
        //     node->scope(), node->srcpos(),
        //     makeSymbolNode(node->scope(), node->srcpos(), Names::kLangDeallocate));
        // auto deinitExpr = makeApplyNode(
        //     node->scope(), node->srcpos(),
        //     makeSymbolNode(node->scope(), node->srcpos(), Names::kLangDeinitialize));
        // auto bindingRefNd =
        //     makeSymbolNode(node->scope(), node->srcpos(), binding->symbolName());
        // bindingRefNd->setIsInMovePos(true);
        // deinitExpr->appendNode(bindingRefNd);
        // deallocExpr->appendNode(deinitExpr);
        // freeExpr = deallocExpr;

        // binding->setWillBeMoved(true);
        return {};
      }

      std::shared_ptr<AstNode> expr;
      {
        Annotator an{typf->fCompiler};
        expr = an.annotateNode(freeExpr);
      }

      auto result = typf->typifyNode(expr);
      result->addMarker(finalizerMark);
      return result;
    }

    return {};
  }

  Type getParamType(const std::shared_ptr<AstNode>& param)
  {
    if (auto p = std::dynamic_pointer_cast<ParamNode>(param))
      return p->type();
    return {};
  }

  Type getParamType(const FunctionParameter& param) { return param.type(); }


  template <typename FunParam>
  void typifyApplyArguments(Typifier* typf, std::shared_ptr<ApplyNode> node,
                            const std::vector<FunParam>& funcParams)
  {
    //const NodeList& funcParams = funcNode->params();
    NodeList& args = node->children();

    hr_assert(args.size() == funcParams.size());

    for (size_t i = 0; i < args.size(); i++) {
      auto paramType = getParamType(funcParams[i]);
      Typifier::OwnershipContext ctx{typf, paramType.isValueType()};
      args[i] = typf->typifyNode(args[i]);
      hr_assert(args[i] != nullptr);
    }
  }


  template <typename FunParam>
  std::shared_ptr<AstNode> liftApplyArgs(Typifier* typf, std::shared_ptr<ApplyNode> node,
                                         const std::vector<FunParam>& funcParams)
  {
    NodeList& args = node->children();

    hr_assert(args.size() == funcParams.size());

    std::vector<int> liftedArgs;
    for (size_t i = 0; i < args.size(); i++) {
      auto paramType = getParamType(funcParams[i]);

      if (paramType.isDef()) {
        if (!paramType.isValueType()) {
          if (!args[i]->type().isPlainType() && args[i]->isTempValue()) {
            liftedArgs.push_back(i);
          }
        }
      }
    }

    if (liftedArgs.empty())
      return node;

    auto scope = node->scope();
    auto srcpos = node->srcpos();

    ScopeHelper scopeHelper(scope, !K(doExport), K(isInnerScope), !K(doPropIntern),
                            kScopeL_Local);
    auto block = makeBlockNode(scope, srcpos);

    for (auto argi : liftedArgs) {
      auto argscope = args[argi]->scope();
      auto argsrcpos = args[argi]->srcpos();

      auto localVarSym = uniqueName("arginit");
      auto localVar = makeVardefNode(argscope, argsrcpos, localVarSym, kNormalVar,
                                     K(isLocal), args[argi]->type(), args[argi]);
      scope->registerVar(argsrcpos, localVarSym, localVar);
      auto localVarNd = makeLetNode(argscope, localVar);
      block->appendNode(localVarNd);

      args[argi] = makeSymbolNode(argscope, argsrcpos, localVarSym);
    }

    block->appendNode(node);
    block->markReturnNode(scope);

    std::shared_ptr<AstNode> resultNd;
    {
      Annotator an{typf->fCompiler};
      resultNd = an.annotateNode(block);
    }

    auto replacnd = typf->typifyNode(resultNd);
    typf->replaceNode(replacnd);

    return replacnd;
  }

}  // namespace


template <typename T>
struct NodeTypifier {
  static void typify(Typifier* typf, T node) { typf->typifyNodeList(node->children()); }
};


template <>
struct NodeTypifier<std::shared_ptr<TypeDefNode>> {
  static void typify(Typifier* typf, std::shared_ptr<TypeDefNode> node)
  {
    typf->typifyNodeList(node->slots());
  }
};


template <>
struct NodeTypifier<std::shared_ptr<UndefNode>> {
  static void typify(Typifier* typf, std::shared_ptr<UndefNode> node) {}
};


template <>
struct NodeTypifier<std::shared_ptr<SymbolNode>> {
  static void typify(Typifier* typf, std::shared_ptr<SymbolNode> node)
  {
    auto var1 = typf->fLastUsedScope->lookupVarOrFunc(node->srcpos(), node->name(),
                                                      K(showAmbiguousSymDef));
    if (var1) {
      if (auto bindnd = dynamic_cast<const MoveableBinding*>(var1)) {
        typf->fBindings[bindnd] =
            Typifier::BindingsUse{node.get(), typf->isInOwnershipTransferContext()};
      }
    }

    auto var = node->scope()->lookupVarOrFunc(node->srcpos(), node->name(),
                                              K(showAmbiguousSymDef));
    if (var) {
      if (auto fdn = dynamic_cast<const FuncDefNode*>(var))
        node->setLinkage(fdn->linkage());

      node->setType(var->type());
      return;
    }

    Type type0 = node->scope()->lookupType(node->name(), K(showAmbiguousSymDef));
    Type type1 = node->scope()->normalizeType(type0);

    Type type2 = degeneralizeType(node->srcpos(), type1, node->generics());
    if (type2.isDef())
      node->setType(Type::makeClassTypeOf(type2));
  }
};


template <>
struct NodeTypifier<std::shared_ptr<ArrayTypeNode>> {
  static void typify(Typifier* typf, std::shared_ptr<ArrayTypeNode> node)
  {
    node->setTypeNode(typf->typifyNode(node->typeNode()));

    auto symnd = std::dynamic_pointer_cast<SymbolNode>(node->typeNode());
    auto type = (symnd ? symnd->type() : node->typeNode()->type());
    auto type1 = node->scope()->normalizeType(type);
    node->setType(Type::makeArray(type1, 0, K(isValue)));
  }
};


template <>
struct NodeTypifier<std::shared_ptr<TypeNode>> {
  static void typify(Typifier* typf, std::shared_ptr<TypeNode> node)
  {
    hr_assert(node->type().isDef());
    Type type1 = node->scope()->normalizeType(node->type());
    node->setType(type1);
  }
};


template <>
struct NodeTypifier<std::shared_ptr<DefNode>> {
  static void typify(Typifier* typf, std::shared_ptr<DefNode> node)
  {
    node->setDefNode(typf->typifyNode(node->defNode()));
    node->setType(node->defNode()->type());
  }
};


template <>
struct NodeTypifier<std::shared_ptr<LetNode>> {
  static void typify(Typifier* typf, std::shared_ptr<LetNode> node)
  {
    node->setDefNode(typf->typifyNode(node->defNode()));
    if (auto nd = std::dynamic_pointer_cast<DelayTypeAnnotatable>(node->defNode())) {
      if (nd->isTypeSpecDelayed())
        return;
    }
    node->setType(node->defNode()->type());
  }
};


template <>
struct NodeTypifier<std::shared_ptr<VardefNode>> {
  static void typify(Typifier* typf, std::shared_ptr<VardefNode> node)
  {
    typf->fLastUsedScope->registerVar(node->srcpos(), node->name(), node);

    if (node->initExpr()) {
      Typifier::GenCode gen{typf, false};
      node->setInitExpr(typf->typifyNode(node->initExpr()));
    }

    if (!node->isTypeSpecDelayed())
      typf->setupBindingNodeType(node, "variable");

    // re-typify the initExpr, since only now we have all type
    // information for sure to decide on a possible ownership
    // transfer.
    {
      Typifier::OwnershipContext ctx{typf, node->type().isValueType()};
      if (node->initExpr())
        node->setInitExpr(typf->typifyNode(node->initExpr()));
    }
  }
};


namespace {
  std::shared_ptr<BlockNode> blockFromBody(std::shared_ptr<AstNode> body)
  {
    if (auto scnd = std::dynamic_pointer_cast<ScopeNode>(body)) {
      if (scnd->children().size() == 1) {
        body = scnd->children()[0];
      }
      else {
        hr_invalid("unexpected scope with multiple children");
        return {};
      }
    }

    if (auto block = std::dynamic_pointer_cast<BlockNode>(body)) {
      return block;
    }

    hr_invalid("unexpected node type as function body");
    return {};
  }


  void generateFinalizersForFuncParams(Typifier* typf, std::shared_ptr<AstNode> body,
                                       const NodeList& params)
  {
    if (body) {
      std::vector<std::shared_ptr<AstNode>> finalizers;
      for (auto prmnd : params) {
        if (auto finalizer = generateFinalizer(typf, prmnd)) {
          finalizers.push_back(finalizer);
        }
      }

      if (auto block = blockFromBody(body)) {
        block->insertFinalizers(finalizers);
      }
      else {
        hr_invalid("body is not a block");
      }
    }
  }

}  // namespace


template <>
struct NodeTypifier<std::shared_ptr<FuncDefNode>> {
  static void typify(Typifier* typf, std::shared_ptr<FuncDefNode> node)
  {
    ScopeGuard scopeGuard(typf->fLastUsedScope,
                          makeScope(kScopeL_Function, typf->fLastUsedScope));

    typf->typifyNodeList(node->params());
    if (node->body()) {
      node->setBody(typf->typifyNode(node->body()));

      if (node->body()) {
        if (!node->body()->type().isDef())
          node->body()->setType(Type::makeAny());
      }
    }

    typf->setupFunctionNodeType(node);

    // TODO: check the governing 'application' declaration
    if (node->name() == Names::kAppMain) {
      if (!node->retType().isAny()) {
        if (node->retType().typeId() != Names::kInt32TypeName) {
          HR_LOG(kError, node->srcpos(), E_TypeMismatch)
              << "return type of " MID_app_main "() must be " MID_Int32TypeName;
          HR_LOG(kError, node->srcpos(), E_TypeMismatch)
              << "found to be: " << node->retType();
        }
      }

      node->setRetType(Type::makeTypeRef(MID_Int32TypeName));
    }

    if (node->body()) {
      if (node->isMethod())
        typf->enforceAtomTypeConv(node->body(), node->retType());
      else
        typf->annotateTypeConv(node->body(), node->retType());
      typf->setBodyLastDstType(node->body(), node->retType());
    }

    if (typf->fGenerateCode) {
      for (auto c : node->params()) {
        trackMoveablePositions(typf, c);
      }

      generateFinalizersForFuncParams(typf, node->body(), node->params());
    }
  }
};


template <>
struct NodeTypifier<std::shared_ptr<FunctionNode>> {
  static void typify(Typifier* typf, std::shared_ptr<FunctionNode> node)
  {
    ScopeGuard scopeGuard(typf->fLastUsedScope,
                          makeScope(kScopeL_Function, typf->fLastUsedScope));

    typf->typifyNodeList(node->params());
    if (node->body()) {
      node->setBody(typf->typifyNode(node->body()));

      if (!node->body()->type().isDef())
        node->body()->setType(Type::makeAny());
    }

    typf->setupFunctionNodeType(node);
    if (node->body())
      typf->annotateTypeConv(node->body(), node->retType());

    if (typf->fGenerateCode) {
      for (auto c : node->params()) {
        trackMoveablePositions(typf, c);
      }

      generateFinalizersForFuncParams(typf, node->body(), node->params());
    }
  }
};


template <>
struct NodeTypifier<std::shared_ptr<SlotdefNode>> {
  static void typify(Typifier* typf, std::shared_ptr<SlotdefNode> node)
  {
    // TODO
  }
};


template <>
struct NodeTypifier<std::shared_ptr<BlockNode>> {
  static void typify(Typifier* typf, std::shared_ptr<BlockNode> node)
  {
    // there's at least a local (return) variable binding and a return
    // of that binding
    hr_assert(node->children().size() >= 2);

    {
      auto& exprs = node->children();

      bool stripRemoved = false;
      for (size_t i = 0; i < exprs.size() - 1; i++) {
        Typifier::OwnershipContext ctx{typf, false};
        exprs[i] = typf->typifyNode(exprs[i]);
        stripRemoved |= (exprs[i] == nullptr);
      }

      {
        auto lastExprIdx = exprs.size() - 1;
        Typifier::OwnershipContext ctx{typf, true};
        exprs[lastExprIdx] = typf->typifyNode(exprs[lastExprIdx]);

        // leaving out the last expression in a block is questionable
        hr_assert(exprs[lastExprIdx] != nullptr);
      }

      exprs.erase(std::remove_if(exprs.begin(), exprs.end(),
                                 [](const auto& nd) { return nd == nullptr; }),
                  exprs.end());
    }

    node->setType(node->children().back()->type());

    if (typf->fGenerateCode) {
      for (auto c : node->children()) {
        if (auto letnd = std::dynamic_pointer_cast<LetNode>(c)) {
          trackMoveablePositions(typf, letnd->defNode());
        }
      }

      auto& ndChildren = node->children();
      // the pre-last expression should be a local variable storing the
      // return expression
      hr_assert(dynamic_cast<const SymbolNode*>(prev(ndChildren.end(), 1)->get()));

      std::vector<std::shared_ptr<AstNode>> finalizers;
      for (auto c : ndChildren) {
        if (auto letnd = std::dynamic_pointer_cast<LetNode>(c)) {
          // we don't have to protect the letNode for the final return
          // value, since trackMoveablePositions() above will have set
          // the willBeMoved() flag on it, and thus it won't be
          // destructed.
          if (auto finalizer = generateFinalizer(typf, letnd->defNode()))
            finalizers.push_back(finalizer);
        }
      }

      node->insertFinalizers(finalizers);
    }
  }
};


template <>
struct NodeTypifier<std::shared_ptr<ParamNode>> {
  static void typify(Typifier* typf, std::shared_ptr<ParamNode> node)
  {
    typf->fLastUsedScope->registerVar(node->srcpos(), node->name(), node);

    if (node->initExpr()) {
      Typifier::GenCode gen{typf, false};
      node->setInitExpr(typf->typifyNode(node->initExpr()));
    }

    typf->setupBindingNodeType(node, "parameter");

    {
      Typifier::OwnershipContext ctx{typf, node->type().isValueType()};
      if (node->initExpr())
        node->setInitExpr(typf->typifyNode(node->initExpr()));
    }

    if (node->type().isDef()) {
      if (node->isSpecArg()) {
        if (node->type().isPlainType())
          node->setTypeConv(kAtom2PlainConv);
        else if (node->type().isAny())
          node->setTypeConv(kTypeCheckConv);
        // else
        //   ; //OK
      }
      node->setDstType(node->type());
    }
  }
};


namespace {
  std::vector<FunctionParameter> typesForArgs(const NodeList& args)
  {
    std::vector<FunctionParameter> result;
    for (auto& arg : args) {
      if (auto keyedParam = std::dynamic_pointer_cast<KeyargNode>(arg)) {
        result.push_back(FunctionParameter(FunctionParameter::kParamNamed, !K(isSpec),
                                           keyedParam->key(), arg->type()));
      }
      else {
        result.push_back(FunctionParameter(FunctionParameter::kParamPos, !K(isSpec),
                                           String(), arg->type()));
      }
    }
    return result;
  }


  bool replaceBuiltinDeinit(Typifier* typf, std::shared_ptr<ApplyNode> node)
  {
    if (node->children().size() != 1) {
      HR_LOG(kError, node->srcpos(), E_BadParameterList)
          << "Not matching count of params.  Expected 1 found "
          << node->children().size();
      return true;
    }

    auto arg = node->children()[0];
    auto ty = arg->type();

    auto realty = arg->scope()->lookupType(ty);

    if (realty.isPlainType()) {
      typf->replaceNode(nullptr);
      return true;
    }
    else if (realty.isRecord()) {
      node->setBase(typf->typifyNode(
          makeSymbolNode(arg->scope(), arg->srcpos(), Names::kDeinitFuncName)));
    }
    else {
      node->setBase(typf->typifyNode(
          makeSymbolNode(arg->scope(), arg->srcpos(), String("deinitialize"))));
    }

    return false;
  }


  bool replaceBuiltinNullValue(Typifier* typf, std::shared_ptr<ApplyNode> node)
  {
    if (node->children().size() != 1) {
      HR_LOG(kError, node->srcpos(), E_BadParameterList)
          << "Not matching count of params.  Expected 1 found "
          << node->children().size();
      return true;
    }

    auto arg = node->children()[0];
    auto ty = arg->type();

    if (!ty.isClassTypeOf()) {
      HR_LOG(kError, node->srcpos(), E_TypeMismatch) << "parameter type mismatch " << ty;
      return true;
    }

    auto realty = arg->scope()->lookupType(ty.classTypeOfType());

    if (realty.isPlainType()) {
      auto defnd = realty.typeProperty().makeNullValueNode();
      defnd->setScope(arg->scope());
      typf->replaceNode(typf->typifyNode(defnd));
      return true;
    }

    auto initnd = generateInstantiateCall(arg->srcpos(), arg->scope(), ty, {});

    {
      Annotator an{typf->fCompiler};
      initnd = an.annotateNode(initnd);
    }

    typf->replaceNode(typf->typifyNode(initnd));
    return true;
  }


  bool replaceBuiltinInitMoveCopy(Typifier* typf, std::shared_ptr<ApplyNode> node,
                                  const String& mvcpFuncName)
  {
    if (node->children().size() != 2) {
      HR_LOG(kError, node->srcpos(), E_BadParameterList)
          << "Not matching count of params.  Expected 2 found "
          << node->children().size();
      return true;
    }

    auto arg = node->children()[0];
    auto ty = arg->type();

    auto realty = arg->scope()->lookupType(ty);

    std::shared_ptr<AstNode> replacend;
    if (realty.isRecord()) {
      auto initmovend =
          makeApplyNode(node->scope(), node->srcpos(),
                        makeSymbolNode(node->scope(), node->srcpos(), mvcpFuncName));
      initmovend->appendNodes(node->children());
      replacend = initmovend;
    }
    else {
      replacend = makeAssignNode(node->scope(), node->srcpos(), node->children()[0],
                                 node->children()[1]);
    }

    {
      Annotator an{typf->fCompiler};
      replacend = an.annotateNode(replacend);
    }

    typf->replaceNode(typf->typifyNode(replacend));
    return true;
  }


  bool checkForAndReplaceBuiltinFunctions(Typifier* typf, std::shared_ptr<ApplyNode> node)
  {
    if (node->simpleCallName() == "__builtin-deinit") {
      return replaceBuiltinDeinit(typf, node);
    }
    else if (node->simpleCallName() == "__builtin-null-value") {
      return replaceBuiltinNullValue(typf, node);
    }
    else if (node->simpleCallName() == "__builtin-init-move") {
      return replaceBuiltinInitMoveCopy(typf, node, Names::kInitMoveFuncName);
    }
    else if (node->simpleCallName() == "__builtin-init-copy") {
      return replaceBuiltinInitMoveCopy(typf, node, Names::kInitCopyFuncName);
    }

    return false;
  }
}  // namespace


template <>
struct NodeTypifier<std::shared_ptr<ApplyNode>> {
  static void typify(Typifier* typf, std::shared_ptr<ApplyNode> node)
  {
    node->setBase(typf->typifyNode(node->base()));
    typf->typifyNodeList(node->children());

    if (node->isSimpleCall()) {
      if (checkForAndReplaceBuiltinFunctions(typf, node))
        return;

      auto funcNode =
          node->scope()->lookupFunction(node->simpleCallName(), K(showAmbiguousSymDef));

      if (funcNode) {
        if (funcNode->hasSpecializedParams()) {
          typf->reorderArguments(node, funcNode.get());
          typifyApplyArguments(typf, node, funcNode->params());

          Type type = typf->typifyMatchAndCheckParameters(
              node->srcpos(), node->children(), funcNode.get(), node->simpleCallName());
          if (type.isDef())
            node->setType(type);

          if (node->simpleCallName() == Names::kLangAllocateArray) {
            typf->checkAllocateArraySignature(node);
          }

          node->setFunSign(funcNode->type().functionSignature());

          auto replcdnd = liftApplyArgs(typf, node, funcNode->params());
          typf->annotateTypeConv(replcdnd, replcdnd->type());
          return;
        }
        else {
          if (auto bestFuncNode = node->scope()->lookupBestFunctionOverload(
                  node->simpleCallName(), typesForArgs(node->children()), node->srcpos(),
                  K(showAmbiguousSymDef))) {
            auto newBase =
                makeSymbolNode(node->scope(), node->srcpos(), bestFuncNode.fName);
            newBase->setRefersTo(kFunction, !K(isShared));
            node->setBase(typf->typifyNode(newBase));
            node->setRefFunction(bestFuncNode.fNode);

            // it could be that the function is only defined later,
            // and therefore its node hasn't been typify'ed yet.  Do
            // this lazily here.
            if (!bestFuncNode.fNode->type().isFunction()) {
              auto tmpNode = typf->typifyNode(bestFuncNode.fNode);
              hr_assert(bestFuncNode.fNode == tmpNode);
            }
            node->setFunSign(bestFuncNode.fNode->type().functionSignature());

            typf->reorderArguments(node, bestFuncNode.fNode.get());
            typifyApplyArguments(typf, node, bestFuncNode.fNode->params());

            Type type = typf->typifyMatchAndCheckParameters(
                node->srcpos(), node->children(), bestFuncNode.fNode.get(),
                node->simpleCallName());
            if (type.isDef())
              node->setType(type);

            if (node->simpleCallName() == Names::kLangAllocateArray) {
              typf->checkAllocateArraySignature(node);
            }

            auto replcdnd = liftApplyArgs(typf, node, bestFuncNode.fNode->params());
            typf->annotateTypeConv(replcdnd, replcdnd->type());
            return;
          }
          else if (node->isRemoveable()) {
            node->setIsObsolete(true);
          }
          else {
            HR_LOG(kError, node->srcpos(), E_NoMatchingFunction)
                << "no matching function: " << node->simpleCallName();
          }
        }
      }
      else {
        auto varNode = node->scope()->lookupVarOrFunc(
            node->srcpos(), node->simpleCallName(), K(showAmbiguousSymDef));
        if (varNode) {
          if (varNode->type().isFunction()) {
            node->base()->setType(varNode->type());
            node->setType(varNode->type().functionSignature().returnType());
            // TODO: check function signature of the function type
            /// HR_LOG(kInfo, node->srcpos()) << "varNode->type()? [1] " << varNode->type().functionSignature();
            node->setFunSign(varNode->type().functionSignature());

            typifyApplyArguments(typf, node,
                                 varNode->type().functionSignature().parameters());

            auto replcdnd = liftApplyArgs(
                typf, node, varNode->type().functionSignature().parameters());
            typf->annotateTypeConv(replcdnd, replcdnd->type());
            return;
          }
          else if (varNode->type().isClassTypeOf()) {
            auto newObjAllocExpr = makeApplyNode(
                node->scope(), node->srcpos(),
                makeSymbolNode(node->scope(), node->srcpos(), Names::kLangAllocate));
            newObjAllocExpr->appendNode(
                makeTypeNode(node->scope(), node->srcpos(), varNode->type()));

            //---
            std::shared_ptr<AstNode> funcNode;
            if (varNode->type().isOpen()) {
              // HR_LOG(kInfo, varNode->srcpos()) << "typify applynode " << varNode->type().typeName();
              auto apply = makeApplyNode(
                  node->scope(), node->srcpos(),
                  makeSymbolNode(node->scope(), node->srcpos(), Names::kLangInitFunctor));
              apply->appendNode(
                  makeTypeNode(node->scope(), node->srcpos(), varNode->type()));
              funcNode = apply;
            }
            else {
              String initName =
                  qualifyId(varNode->type().typeName(), Names::kInitFuncName);
              funcNode = makeSymbolNode(node->scope(), node->srcpos(), initName);
            }

            auto initExpr = makeApplyNode(node->scope(), node->srcpos(), funcNode);
            initExpr->appendNode(newObjAllocExpr);

            std::shared_ptr<AstNode> createNode;

            {
              Annotator an{typf->fCompiler};
              createNode = an.annotateNode(initExpr);
            }

            createNode = typf->typifyNode(createNode);

            // TODO: check function signature of the type constructor
            node->setBase(createNode);
            node->setType(createNode->type());
            // HR_LOG(kInfo, node->srcpos()) << "varNode->type()? " << varNode->type().functionSignature();
            // node->setFunSign(varNode->type().functionSignature());

            // TODO: do we have to liftApplyArgs here?
          }
          else {
            HR_LOG(kError, node->srcpos(), E_NoCallable)
                << "Non callable in function call context (lookup var node)";
          }
        }
        else if (node->isRemoveable()) {
          node->setIsObsolete(true);
        }
        else {
          HR_LOG(kError, node->srcpos(), E_UnknownSymbol)
              << "unknown symbol (apply): " << node->simpleCallName();
        }
      }
    }
    else {
      if (auto typeNode = std::dynamic_pointer_cast<ArrayTypeNode>(node->base())) {
        node->setType(typeNode->type());
      }
      else if (auto symNode = std::dynamic_pointer_cast<SymbolNode>(node->base())) {
        // TODO: the only way to come here seems to be a call like
        // 'T(...), where 'T might be a function?
        node->setType(symNode->type());
      }
      else if (auto typeNode = std::dynamic_pointer_cast<TypeNode>(node->base())) {
        node->setType(typeNode->type());
      }
      else if (auto funNode = std::dynamic_pointer_cast<FunctionNode>(node->base())) {
        // case: function(a : Int){ ... }(42)

        // TODO: check function signature
        node->setType(funNode->type());
        node->setFunSign(funNode->type().functionSignature());

        typifyApplyArguments(typf, node,
                             funNode->type().functionSignature().parameters());

        auto replcdnd =
            liftApplyArgs(typf, node, funNode->type().functionSignature().parameters());
        typf->annotateTypeConv(replcdnd, replcdnd->type());
        return;
      }
      else if (auto applyNode = std::dynamic_pointer_cast<ApplyNode>(node->base())) {
        if (applyNode->type().isFunction()) {
          // case: foo()()

          // TODO: check function signature
          node->setType(applyNode->type().functionSignature().returnType());
          node->setFunSign(applyNode->type().functionSignature());

          typifyApplyArguments(typf, node,
                               applyNode->type().functionSignature().parameters());

          auto replcdnd = liftApplyArgs(
              typf, node, applyNode->type().functionSignature().parameters());
          typf->annotateTypeConv(replcdnd, replcdnd->type());
          return;
        }
        else {
          HR_LOG(kError, applyNode->srcpos(), E_NoCallable)
              << "Non callable in function call context (apply-node) ["
              << applyNode->type() << "]";
          node->setType(applyNode->type());
        }
      }
      else if (auto srNode = std::dynamic_pointer_cast<SlotRefNode>(node->base())) {
        if (srNode->type().isFunction()) {
          // TODO: check function signature
          HR_LOG(kInfo) << "TODO: SlotRefNode in function call pos " << srNode->type();
          node->setType(srNode->type());
          node->setFunSign(srNode->type().functionSignature());
        }
        else {
          HR_LOG(kError, srNode->srcpos(), E_NoCallable)
              << "Non callable in function call context (slotref-node)";
          node->setType(srNode->type());
        }
      }
      else {
        HR_LOG(kError, node->srcpos(), E_NoCallable)
            << "Non callable in function call context";
        node->setType(node->type());
      }
    }

    typf->annotateTypeConv(node, node->type());
  }
};


template <>
struct NodeTypifier<std::shared_ptr<WeakNode>> {
  static void typify(Typifier* typf, std::shared_ptr<WeakNode> node)
  {
    if (node->refNode())
      node->setRefNode(typf->typifyNode(node->refNode()));

    if (node->isObsolete())
      node->reset();
  }
};


template <>
struct NodeTypifier<std::shared_ptr<AssignNode>> {
  static void typify(Typifier* typf, std::shared_ptr<AssignNode> node)
  {
    {
      Typifier::GenCode gen{typf, false};
      node->setRvalue(typf->typifyNode(node->rvalue()));
    }

    if (node->isTypeSpecDelayed()) {
      auto symNode = std::dynamic_pointer_cast<SymbolNode>(node->lvalue());
      hr_assert(symNode);

      auto var = node->scope()->lookupVarOrFunc(symNode->srcpos(), symNode->name(),
                                                K(showAmbiguousSymDef));
      auto vardefNode = const_cast<VardefNode*>(dynamic_cast<const VardefNode*>(var));
      hr_assert(vardefNode);

      if (vardefNode->type().isUnion()) {
        TypeVector newUnionTypes;
        for (auto ty : vardefNode->type().unionTypes()) {
          if (ty.isAny()) {
            newUnionTypes.push_back(node->rvalue()->type());
          }
          else {
            newUnionTypes.push_back(ty);
          }
        }

        auto newTy = Type::makeUnion(newUnionTypes, vardefNode->type().isValueType());

        symNode->setType(newTy);
        vardefNode->setType(newTy);
      }
      else {
        symNode->setType(node->rvalue()->type());
        vardefNode->setType(node->rvalue()->type());
      }

      node->setTypeSpecDelayed(false);
      vardefNode->setTypeSpecDelayed(false);
    }
    else {
      Typifier::OwnershipContext ctx{typf, false};
      node->setLvalue(typf->typifyNode(node->lvalue()));
    }

    Type ltype = node->lvalue()->type();
    Type rtype = node->rvalue()->type();

    if (!rtype.isDef()) {
      HR_LOG(kError, node->rvalue()->srcpos(), E_TypeMismatch)
          << "Undefined type in assignment right hand value";
    }
    else if (!ltype.isDef()) {
      // infer the vardef type from rvalue expression
      node->lvalue()->setType(rtype);
    }
    else if (!isCovariant(ltype, rtype, *node->scope(), node->srcpos()) &&
             !containsAny(rtype, node->srcpos())) {
      HR_LOG(kError, node->rvalue()->srcpos(), E_TypeMismatch)
          << "type mismatch in assignment: " << ltype.typeId() << " <- " << rtype;
    }

    {
      Typifier::OwnershipContext ctx{typf, node->lvalue()->type().isValueType()};
      node->setRvalue(typf->typifyNode(node->rvalue()));
    }

    node->setType(rtype);

    typf->annotateTypeConv(node->rvalue(), ltype);
    typf->annotateTypeConv(node->lvalue(), ltype);
    typf->annotateTypeConv(node, ltype);
  }
};


template <>
struct NodeTypifier<std::shared_ptr<BinaryNode>> {
  static void typify(Typifier* typf, std::shared_ptr<BinaryNode> node)
  {
    Type retty;

    {
      Typifier::OwnershipContext ctx{typf, false};
      node->setLeft(typf->typifyNode(node->left()));
      node->setRight(typf->typifyNode(node->right()));
    }

    auto leftty = node->left()->type();
    auto rightty = node->right()->type();

    switch (node->op()) {
    case kOpInvalid: hr_invalid("type not determined yet");

    case kOpPlus:
    case kOpMinus:
    case kOpMultiply:
    case kOpDivide:
    case kOpMod:
    case kOpRem:
    case kOpExponent:
      if (leftty.isAny() || rightty.isAny()) {
        node->setType(Type::makeAny());
        typf->annotateTypeConv(node->left(), node->type());
        typf->annotateTypeConv(node->right(), node->type());
        typf->annotateTypeConv(node, node->type());
        return;
      }
      if (leftty.isNumber() || rightty.isNumber()) {
        node->setType(Type::makeTypeRef(Names::kNumberTypeName, K(isValue)));
        typf->annotateTypeConv(node->left(), node->type());
        typf->annotateTypeConv(node->right(), node->type());
        typf->annotateTypeConv(node, node->type());
        return;
      }

      if (leftty.isComplex() || rightty.isComplex()) {
        node->setType(Type::makeTypeRef(Names::kComplexTypeName, K(isValue)));
        typf->annotateTypeConv(node->left(), node->type());
        typf->annotateTypeConv(node->right(), node->type());
        typf->annotateTypeConv(node, node->type());
        return;
      }

      if (leftty.isAnyFloat()) {
        if (rightty.isAnyFloat())
          node->setType(maxFloatType(leftty, rightty));
        else
          node->setType(leftty);
        typf->annotateTypeConv(node->left(), node->type());
        typf->annotateTypeConv(node->right(), node->type());
        typf->annotateTypeConv(node, node->type());
        return;
      }
      if (rightty.isAnyFloat()) {
        if (leftty.isAnyFloat())
          node->setType(maxFloatType(leftty, rightty));
        else
          node->setType(rightty);
        typf->annotateTypeConv(node->left(), node->type());
        typf->annotateTypeConv(node->right(), node->type());
        typf->annotateTypeConv(node, node->type());
        return;
      }

      if (leftty.isRational() || rightty.isRational()) {
        node->setType(Type::makeTypeRef(Names::kRationalTypeName, K(isValue)));
        typf->annotateTypeConv(node->left(), node->type());
        typf->annotateTypeConv(node->right(), node->type());
        typf->annotateTypeConv(node, node->type());
        return;
      }

      if (leftty.isAnyInt() && rightty.isAnyInt()) {
        node->setType(maxIntType(leftty, rightty));
        typf->annotateTypeConv(node->left(), node->type());
        typf->annotateTypeConv(node->right(), node->type());
        typf->annotateTypeConv(node, node->type());
        return;
      }


      if (typf->checkBinaryFunctionCall(node, typf->operatorNameByOp(node->op()),
                                        node->left(), node->right()))
        return;

      HR_LOG(kError, node->srcpos(), E_BinaryTypeMismatch)
          << "incompatible types in binary operation (" << leftty << " and " << rightty
          << ")";
      node->setType(Type::makeAny());
      typf->annotateTypeConv(node, node->type());
      break;

    case kOpEqual:
    case kOpGreater:
    case kOpGreaterEqual:
    case kOpIn:
    case kOpLess:
    case kOpLessEqual:
    case kOpUnequal:
      if (leftty.isAny() || rightty.isAny() ||
          (leftty.isAnySignedInt() && rightty.isAnySignedInt()) ||
          (leftty.isAnyUInt() && rightty.isAnyUInt()) ||
          (leftty.isAnyFloat() && rightty.isAnyFloat()) ||
          (leftty.isRational() && rightty.isRational()) ||
          (leftty.isComplex() && rightty.isComplex()) ||
          (leftty.isNumber() && rightty.isNumber()) ||
          isSameType(leftty, rightty, *node->scope(), node->srcpos())) {
        // any is always ok.
        node->setType(Type::makeBool());
        typf->annotateTypeConv(node->left(), leftty);
        typf->annotateTypeConv(node->right(), leftty);
        typf->annotateTypeConv(node, node->type());
        return;
      }

      // TODO: check that left and right type are comparable
      HR_LOG(kError) << "compare left: " << leftty;
      HR_LOG(kError) << "compare right" << rightty;
      HR_LOG(kError, node->srcpos(), E_BinaryTypeMismatch)
          << "incompatible types in binary comparison";
      node->setType(Type::makeAny());
      typf->annotateTypeConv(node, node->type());
      break;

    case kOpCompare:
      if (leftty.isAny() || rightty.isAny() ||
          (leftty.isAnySignedInt() && rightty.isAnySignedInt()) ||
          (leftty.isAnyUInt() && rightty.isAnyUInt()) ||
          (leftty.isAnyFloat() && rightty.isAnyFloat()) ||
          (leftty.isRational() && rightty.isRational()) ||
          (leftty.isComplex() && rightty.isComplex()) ||
          (leftty.isNumber() && rightty.isNumber()) ||
          isSameType(leftty, rightty, *node->scope(), node->srcpos())) {
        node->setType(Type::makeInt32());
        typf->annotateTypeConv(node->left(), leftty);
        typf->annotateTypeConv(node->right(), leftty);
        typf->annotateTypeConv(node, node->type());
        return;
      }

      HR_LOG(kError, node->srcpos(), E_BinaryTypeMismatch)
          << "incompatible types in binary comparison (compare)";
      node->setType(Type::makeAny());
      typf->annotateTypeConv(node, node->type());
      break;

    case kOpIsa:
      if (rightty.isAny() || rightty.isClassTypeOf()) {
        node->setType(Type::makeBool());
        typf->annotateTypeConv(node->left(), node->type());
        typf->annotateTypeConv(node->right(), node->type());
        typf->annotateTypeConv(node, node->type());
        return;
      }
      // TODO: try to lookup a method which enables isa(leftty, rightty) and
      // use it's returntype
      HR_LOG(kError, node->srcpos(), E_BinaryTypeMismatch)
          << "incompatible right side type in isa operation";
      node->setType(Type::makeAny());
      typf->annotateTypeConv(node, node->type());
      break;

    case kOpConcat:
      if (leftty.isString() || leftty.isAny()) {
        if (rightty.isString() || rightty.isChar() || rightty.isAny()) {
          node->setType(leftty);
          typf->annotateTypeConv(node->left(), leftty);
          typf->annotateTypeConv(node->right(), leftty);
          typf->annotateTypeConv(node, node->type());
          return;
        }
      }
      // TODO: try to lookup a method which enables append(leftty, rightty)
      // and use it's returntype
      HR_LOG(kError, node->srcpos(), E_BinaryTypeMismatch)
          << "incompatible types in append() operation";
      node->setType(Type::makeAny());
      typf->annotateTypeConv(node, node->type());
      break;

    case kOpFold:
      if (leftty.isString() || leftty.isAny()) {
        // accept everything on the right hand side
        node->setType(leftty);
        typf->annotateTypeConv(node->left(), leftty);
        typf->annotateTypeConv(node->right(), leftty);
        typf->annotateTypeConv(node, node->type());
        return;
      }
      // TODO: try to lookup a method which enables fold(leftty, rightty) and
      // use it's returntype
      HR_LOG(kError, node->srcpos(), E_BinaryTypeMismatch)
          << "incompatible types in fold() operation";
      node->setType(Type::makeAny());
      typf->annotateTypeConv(node, node->type());
      break;

    case kOpLogicalAnd:
    case kOpLogicalOr:
      if (leftty.isBool() && rightty.isBool()) {
        node->setType(Type::makeBool());
        typf->annotateTypeConv(node->left(), leftty);
        typf->annotateTypeConv(node->right(), leftty);
        typf->annotateTypeConv(node, node->type());
        return;
      }
      HR_LOG(kError, node->srcpos(), E_BinaryTypeMismatch)
          << "bool types required in logical 'and'/'or' operations";
      node->setType(Type::makeAny());
      typf->annotateTypeConv(node, node->type());
      break;

    case kOpBitAnd:
    case kOpBitOr:
    case kOpBitXor:
      if ((leftty.isAnyUInt() || leftty.isAny()) &&
          (rightty.isAnyUInt() || rightty.isAny())) {
        node->setType(leftty);
        typf->annotateTypeConv(node->left(), leftty);
        typf->annotateTypeConv(node->right(), leftty);
      }
      else {
        HR_LOG(kError, node->srcpos(), E_BinaryTypeMismatch)
            << "AND, OR, XOR operations require unsigned integer types on both sides";
        node->setType(Type::makeAny());
      }
      typf->annotateTypeConv(node, node->type());
      break;

    case kOpShiftLeft:
    case kOpShiftRight:
      if (leftty.isAnyUInt() || leftty.isAny()) {
        if (rightty.isAnyInt() || rightty.isAny()) {
          node->setType(leftty);
          typf->annotateTypeConv(node->left(), leftty);
          typf->annotateTypeConv(node->right(), leftty);
        }
        else {
          HR_LOG(kError, node->srcpos(), E_BinaryTypeMismatch)
              << "bit operations require integer types on right side";
          node->setType(Type::makeAny());
        }
      }
      else {
        HR_LOG(kError, node->srcpos(), E_BinaryTypeMismatch)
            << "bit operations require unsigned integer types on left side";
        node->setType(Type::makeAny());
      }
      typf->annotateTypeConv(node, node->type());
      break;

    case kOpMapTo:
    case kOpRange:
    case kOpBy:
    case kOpAs:
    case kOpAssign: hr_invalid("not handled operators?");
    }
  }
};


template <>
struct NodeTypifier<std::shared_ptr<SlotRefNode>> {
  static void typify(Typifier* typf, std::shared_ptr<SlotRefNode> node)
  {
    node->setBase(typf->typifyNode(node->base()));

    Type basety =
        (node->base()->type().isDef()
             ? node->scope()->lookupType(node->base()->type())
             : node->scope()->lookupType(Names::kAnyTypeName, K(showAmbiguousSymDef)));

    if (!basety.isDef()) {
      String typenm = (node->base()->type().isDef() ? node->base()->type().typeId()
                                                    : Names::kAnyTypeName);
      HR_LOG(kError, node->srcpos(), E_UndefinedType)
          << "undefined type '" << typenm << "'";

      node->setType(Type::makeAny());
      node->setDstType(Type::makeAny());
      typf->annotateTypeConv(node, node->type());
    }
    else {
      if (basety.isRecord()) {
        Type slotType = basety.slotType(node->slotName(), *node->scope());
        if (slotType.isDef()) {
          node->setType(slotType);
          node->setDstType(slotType);
          typf->annotateTypeConv(node, node->type());
        }
        else {
          HR_LOG(kError, node->srcpos(), E_UnknownSlot)
              << "reference to unknown slot '" << node->slotName()
              << "' in type: " << basety;
          node->setType(Type::makeAny());
          node->setDstType(Type::makeAny());
          typf->annotateTypeConv(node, node->type());
        }
      }
      else if (!node->base()->isRemoveable()) {
        HR_LOG(kError, node->srcpos(), E_SlotRefToNonClass)
            << "reference to slot '" << node->slotName() << "' is non-record type";
        node->setType(Type::makeAny());
        node->setDstType(Type::makeAny());
        typf->annotateTypeConv(node, node->type());
      }
    }
  }
};


template <>
struct NodeTypifier<std::shared_ptr<UnaryNode>> {
  static void typify(Typifier* typf, std::shared_ptr<UnaryNode> node)
  {
    {
      Typifier::OwnershipContext ctx{typf, false};
      node->setBase(typf->typifyNode(node->base()));
    }

    switch (node->op()) {
    case kUnaryOpNegate: node->setType(node->base()->type()); break;

    case kUnaryOpNot:
      if (!node->base()->type().isDef()) {
        HR_LOG(kError, node->srcpos(), E_BoolTypeExpected)
            << "bool expected for not operator";
        node->setType(Type::makeAny());
      }
      else if (node->base()->type().isBool()) {
        node->setType(node->base()->type());
      }
      else if (node->base()->type().isAny()) {
        node->setType(Type::makeBool());
      }
      else {
        HR_LOG(kError, node->srcpos(), E_BoolTypeExpected)
            << "bool expected for not operator";
        node->setType(Type::makeAny());
      }
      typf->annotateTypeConv(node->base(), Type::makeBool());
      break;

    case kUnaryOpInvalid: hr_invalid("unhandled unary operator"); break;
    }
  }
};


template <>
struct NodeTypifier<std::shared_ptr<IfNode>> {
  static void typify(Typifier* typf, std::shared_ptr<IfNode> node)
  {
    Typifier::OwnershipContext ctx{typf, false};

    node->setTest(typf->typifyNode(node->test()));
    typf->annotateTypeConv(node->test(), Type::makeBool());

    node->setConsequent(typf->typifyNode(node->consequent()));
    if (node->alternate())
      node->setAlternate(typf->typifyNode(node->alternate()));

    if (node->alternate()) {
      Type cotype = node->consequent()->type();
      Type alttype = node->alternate()->type();

      if (isCovariant(cotype, alttype, *node->scope(), node->srcpos())) {
        node->setType(alttype);
        typf->annotateTypeConv(node->consequent(), alttype);
        typf->annotateTypeConv(node->alternate(), alttype);
        typf->annotateTypeConv(node, alttype);
      }
      else if (isCovariant(alttype, cotype, *node->scope(), node->srcpos())) {
        node->setType(cotype);
        typf->annotateTypeConv(node->consequent(), cotype);
        typf->annotateTypeConv(node->alternate(), cotype);
        typf->annotateTypeConv(node, cotype);
      }
      else {
        // if the if expression is not in tail position, the branch type
        // mismatch doesn't matter.
        if (node->isInTailPos() || node->isSingleTypeRequired()) {
          HR_LOG(kError, node->srcpos(), E_IfConsqTypeMismatch)
              << "types for if consequent and alternate branch do not match";
        }
        node->setType(Type::makeAny(K(isValue)));
        typf->annotateTypeConv(node->consequent(), Type::makeAny());
        typf->annotateTypeConv(node->alternate(), Type::makeAny());
        typf->annotateTypeConv(node, Type::makeAny());
      }
    }
    else {
      if (node->isInTailPos() || node->isSingleTypeRequired()) {
        // if the if expression is in tail position we should definitely have
        // have an alternate branch.
        HR_LOG(kError, node->srcpos(), E_IfAltTypeMismatch)
            << "unspecified alternate branch do not match type with consequent";
        node->setType(Type::makeAny(K(isValue)));
      }
      else {
        node->setType(node->consequent()->type());
      }
      typf->annotateTypeConv(node->consequent(), node->type());
      typf->annotateTypeConv(node, node->type());
    }
  }
};


template <>
struct NodeTypifier<std::shared_ptr<KeyargNode>> {
  static void typify(Typifier* typf, std::shared_ptr<KeyargNode> node)
  {
    {
      Typifier::GenCode gen{typf, false};
      node->setValue(typf->typifyNode(node->value()));
    }
    {
      Typifier::OwnershipContext ctx{typf, node->value()->type().isValueType()};
      node->setValue(typf->typifyNode(node->value()));
    }
    node->setType(node->value()->type());
  }
};


template <>
struct NodeTypifier<std::shared_ptr<SelectNode>> {
  static void typify(Typifier* typf, std::shared_ptr<SelectNode> node)
  {
    Typifier::OwnershipContext ctx{typf, false};

    node->setTest(typf->typifyNode(node->test()));
    if (node->comparator())
      node->setComparator(typf->typifyNode(node->comparator()));

    for (size_t i = 0; i < node->mappings().size(); i++) {
      if (node->mappings()[i].fTestValues.empty()) {
        node->mappings()[i].fConsequent =
            typf->typifyNode(node->mappings()[i].fConsequent);
      }
      else {
        for (size_t j = 0; j < node->mappings()[i].fTestValues.size(); j++)
          node->mappings()[i].fTestValues[j] =
              typf->typifyNode(node->mappings()[i].fTestValues[j]);
      }
      node->mappings()[i].fConsequent = typf->typifyNode(node->mappings()[i].fConsequent);
    }
  }
};


template <>
struct NodeTypifier<std::shared_ptr<MatchNode>> {
  static void typify(Typifier* typf, std::shared_ptr<MatchNode> node)
  {
    hr_assert("there must be no matchNode anymore in this phase.  Forgot to call "
              "transformMatchNode in nodifypass?");
  }
};


template <>
struct NodeTypifier<std::shared_ptr<RangeNode>> {
  static void typify(Typifier* typf, std::shared_ptr<RangeNode> node)
  {
    Typifier::OwnershipContext ctx{typf, false};

    node->setFrom(typf->typifyNode(node->from()));
    node->setTo(typf->typifyNode(node->to()));
    if (node->by()) {
      node->setBy(typf->typifyNode(node->by()));
    }

    Type fromType = node->from()->type();
    Type toType = node->to()->type();
    bool fromIsOpen = fromType.isOpen();
    bool toIsOpen = toType.isOpen();

    if ((fromIsOpen || toIsOpen) && (fromIsOpen != toIsOpen)) {
      HR_LOG(kError, node->srcpos(), E_RangeTypeMismatch)
          << "partial open types in range declaration defeats generics usage";
      node->setType(makeRangeType(Type::makeAny(K(isValue))));
      return;
    }

    if (!isSameType(fromType, toType, *node->scope(), node->srcpos())) {
      HR_LOG(kError, node->srcpos(), E_RangeTypeMismatch) << "type of range is ambiguous";
      node->setType(makeRangeType(Type::makeAny(K(isValue))));
      return;
    }

    if (node->by()) {
      Type byType = node->by()->type();
      bool byIsOpen = byType.isOpen();

      if (byIsOpen && byIsOpen != fromIsOpen) {
        HR_LOG(kError, node->srcpos(), E_RangeTypeMismatch)
            << "partial open types in range declaration defeats generics usage";
        node->setType(makeRangeType(Type::makeAny(K(isValue))));
        return;
      }

      if (!isSameType(fromType, byType, *node->scope(), node->by()->srcpos())) {
        HR_LOG(kError, node->srcpos(), E_RangeTypeMismatch)
            << "step type does not match range type";
        node->setType(makeRangeType(Type::makeAny(K(isValue))));
        return;
      }
    }

    node->setType(makeRangeType(node->from()->type()));
  }
};


template <>
struct NodeTypifier<std::shared_ptr<WhileNode>> {
  static void typify(Typifier* typf, std::shared_ptr<WhileNode> node)
  {
    Typifier::OwnershipContext ctx{typf, false};

    node->setTest(typf->typifyNode(node->test()));
    node->setBody(typf->typifyNode(node->body()));

    typf->annotateTypeConv(node->test(), Type::makeBool());

    if (node->isInTailPos() || node->isSingleTypeRequired()) {
      // the while expression should never be in tail position
      HR_LOG(kWarn, node->srcpos(), E_WhileTypeMismatch)
          << "while in tail position enforces Any type";
      node->setType(Type::makeAny(K(isValue)));
    }
    else
      node->setType(node->body()->type());

    typf->annotateTypeConv(node->body(), node->type());
  }
};


static bool mapCommonType(Type& resultType, AstNode& node)
{
  Type ty0 = node.type();
  if (!resultType.isDef()) {
    resultType = ty0;
  }
  else if (!isSameType(ty0, resultType, *node.scope(), node.srcpos())) {
    resultType = Type::makeAny(K(isValue));
    return false;
  }

  return true;
}


template <>
struct NodeTypifier<std::shared_ptr<VectorNode>> {
  static void typify(Typifier* typf, std::shared_ptr<VectorNode> node)
  {
    Typifier::OwnershipContext ctx{typf, true};

    NodeList& nl = node->children();
    typf->typifyNodeList(nl);

    Type valueType;
    TypeVector generics;
    for (size_t i = 0; i < nl.size(); i++) {
      if (!mapCommonType(valueType, *nl[i]))
        break;
    }

    if (!valueType.isDef())
      valueType = Type::makeAny(K(isValue));

    for (auto& nd : nl)
      typf->annotateTypeConv(nd, valueType);

    node->setType(Type::makeTypeRef(Names::kVectorTypeName, makeVector(valueType),
                                    TypeConstVector(), K(isValue)));
  }
};


template <>
struct NodeTypifier<std::shared_ptr<DictNode>> {
  static void typify(Typifier* typf, std::shared_ptr<DictNode> node)
  {
    Typifier::OwnershipContext ctx{typf, true};

    Type keyType;
    Type valueType;

    NodeList& nl = node->children();
    for (size_t i = 0; i < nl.size(); i++) {
      auto pair = dynamic_cast<BinaryNode*>(nl[i].get());
      hr_assert(pair);
      hr_assert(pair->op() == kOpMapTo);

      pair->setLeft(typf->typifyNode(pair->left()));
      pair->setRight(typf->typifyNode(pair->right()));

      mapCommonType(keyType, *pair->left());
      mapCommonType(valueType, *pair->right());
    }

    if (!keyType.isDef())
      keyType = Type::makeAny(K(isValue));
    if (!valueType.isDef())
      valueType = Type::makeAny(K(isValue));

    for (size_t i = 0; i < nl.size(); i++) {
      auto pair = dynamic_cast<BinaryNode*>(nl[i].get());
      hr_assert(pair);
      hr_assert(pair->op() == kOpMapTo);
      typf->annotateTypeConv(pair->right(), valueType);
      typf->annotateTypeConv(pair->left(), keyType);
    }

    node->setType(Type::makeTypeRef(Names::kMapTypeName, makeVector(keyType, valueType),
                                    TypeConstVector(), K(isValue)));
  }
};


template <>
struct NodeTypifier<std::shared_ptr<ArrayNode>> {
  static void typify(Typifier* typf, std::shared_ptr<ArrayNode> node)
  {
    Typifier::OwnershipContext ctx{typf, true};

    NodeList& nl = node->children();
    typf->typifyNodeList(nl);

    Type valueType;
    TypeVector generics;
    for (auto& nd : nl) {
      if (!mapCommonType(valueType, *nd))
        break;
    }

    if (!valueType.isDef())
      valueType = Type::makeAny(K(isValue));

    for (auto& nd : nl)
      typf->annotateTypeConv(nd, valueType);

    node->setType(Type::makeArray(valueType, nl.size(), K(isValue)));
  }
};


template <>
struct NodeTypifier<std::shared_ptr<CastNode>> {
  static void typify(Typifier* typf, std::shared_ptr<CastNode> node)
  {
    {
      Typifier::OwnershipContext ctx{typf, false};
      node->setBase(typf->typifyNode(node->base()));
    }

    if (!node->type().isOpen()) {
      Type type = node->scope()->lookupType(node->type());
      if (!type.isDef()) {
        HR_LOG(kError, node->srcpos(), E_UndefinedType)
            << "undefined type '" << node->type() << "'";
        node->setType(Type::makeAny(K(isValue)));
      }
      else {
        if (isInvariant(node->base()->type(), type, *node->scope(), node->srcpos())) {
          HR_LOG(kError, node->srcpos(), E_InvariantType) << "Cast to invariant type";
          node->setType(Type::makeAny(K(isValue)));
        }
        else
          node->setType(type);
      }
    }

    typf->annotateTypeConv(node->base(), node->type());
  }
};


static void typifyNodeType(std::shared_ptr<AstNode> node, const Type& type,
                           const String& defaultTypeName, bool maybeImaginary)
{
  Type ty =
      (type.isDef() ? node->scope()->lookupType(type)
                    : node->scope()->lookupType(defaultTypeName, K(showAmbiguousSymDef)));

  if (!ty.isDef()) {
    HR_LOG(kError, node->srcpos(), E_UndefinedType)
        << "undefined type '" << defaultTypeName << "'";
    node->setType(Type::makeAny(K(isValue)));
  }
  else {
    if (maybeImaginary && ty.isAnyNumber()) {
      auto nnd = std::dynamic_pointer_cast<BaseNumberNode>(node);
      if (nnd)
        ty.setIsImaginary(nnd->isImaginary());
    }
    node->setType(ty);
  }
}


template <>
struct NodeTypifier<std::shared_ptr<BoolNode>> {
  static void typify(Typifier* typf, std::shared_ptr<BoolNode> node)
  {
    typifyNodeType(node, Type(), Names::kBoolTypeName, !K(maybeImaginary));
    typf->annotateTypeConv(node, node->type());
  }
};


template <>
struct NodeTypifier<std::shared_ptr<CharNode>> {
  static void typify(Typifier* typf, std::shared_ptr<CharNode> node)
  {
    typifyNodeType(node, Type(), Names::kCharTypeName, !K(maybeImaginary));
  }
};


template <>
struct NodeTypifier<std::shared_ptr<RationalNode>> {
  static void typify(Typifier* typf, std::shared_ptr<RationalNode> node)
  {
    typifyNodeType(node, node->type(), Names::kRationalTypeName, K(maybeImaginary));
  }
};


template <>
struct NodeTypifier<std::shared_ptr<RealNode>> {
  static void typify(Typifier* typf, std::shared_ptr<RealNode> node)
  {
    typifyNodeType(node, node->type(), Names::kFloat32TypeName, K(maybeImaginary));
  }
};


template <>
struct NodeTypifier<std::shared_ptr<IntNode>> {
  static void typify(Typifier* typf, std::shared_ptr<IntNode> node)
  {
    typifyNodeType(node, node->type(), Names::kInt32TypeName, K(maybeImaginary));
  }
};


template <>
struct NodeTypifier<std::shared_ptr<StringNode>> {
  static void typify(Typifier* typf, std::shared_ptr<StringNode> node)
  {
    typifyNodeType(node, Type(), Names::kStringTypeName, !K(maybeImaginary));
  }
};


template <>
struct NodeTypifier<std::shared_ptr<KeywordNode>> {
  static void typify(Typifier* typf, std::shared_ptr<KeywordNode> node)
  {
    typifyNodeType(node, Type(), Names::kKeywordTypeName, !K(maybeImaginary));
  }
};


template <>
struct NodeTypifier<std::shared_ptr<ScopeNode>> {
  static void typify(Typifier* typf, std::shared_ptr<ScopeNode> node)
  {
    ScopeGuard scopeGuard(typf->fLastUsedScope,
                          makeScope(node->fLevel, typf->fLastUsedScope));

    typf->typifyNodeList(node->children());

    if (!node->children().empty()) {
      node->setType(node->children().back()->type());
    }
  }
};


template <>
struct NodeTypifier<std::shared_ptr<ApplicationNode>> {
  static void typify(Typifier* typf, std::shared_ptr<ApplicationNode> node)
  {
    typf->typifyNodeList(node->children());

    if (!node->children().empty()) {
      node->setType(node->children().back()->type());
    }
  }
};


//------------------------------------------------------------------------------

Typifier::Typifier(Compiler& compiler)
    : fCompiler(compiler)
    , fLastUsedScope(type::newRootScope())
{
}


std::shared_ptr<AstNode> Typifier::typifyNode(std::shared_ptr<AstNode> node)
{
  dispatchNode<void>(node,
                     [&](auto nd) { NodeTypifier<decltype(nd)>::typify(this, nd); });
  auto retv = (fNewNode || fRemoveNode) ? fNewNode : node;
  fNewNode = nullptr;
  fRemoveNode = false;
  return retv;
}


void Typifier::typifyNodeList(NodeList& nl)
{
  const size_t nlsize = nl.size();

  bool stripRemoved = false;
  for (size_t i = 0; i < nlsize; i++) {
    nl[i] = typifyNode(nl[i]);
    stripRemoved |= (nl[i] == nullptr);
  }

  nl.erase(
      std::remove_if(nl.begin(), nl.end(), [](const auto& nd) { return nd == nullptr; }),
      nl.end());
}


void Typifier::replaceNode(std::shared_ptr<AstNode> newNode)
{
  hr_assert(!fNewNode);

  fNewNode = newNode;
  fRemoveNode = (fNewNode == nullptr);
}


//------------------------------------------------------------------------------

bool Typifier::isNodeCallToGenericFunction(const AstNode* node) const
{
  if (auto applyNode = dynamic_cast<const ApplyNode*>(node)) {
    if (applyNode->isSimpleCall()) {
      // special case lang|slice(array, idx)
      if (applyNode->simpleCallName() == Names::kLangSlice) {
        const NodeList& args = applyNode->children();
        if (args.size() == 2 && args[0]->type().isArray())
          return false;
      }
      // special case lang|slice!(array, idx, value)
      else if (applyNode->simpleCallName() == Names::kLangSliceX) {
        const NodeList& args = applyNode->children();
        if (args.size() == 3 && args[0]->type().isArray())
          return false;
      }
      else if (applyNode->simpleCallName() == Names::kLangIsaQ) {
        const NodeList& args = applyNode->children();
        if (args.size() == 2)
          return false;
      }

      auto funcNode = applyNode->scope()->lookupFunction(applyNode->simpleCallName(),
                                                         K(showAmbiguousSymDef));
      if (funcNode)
        return funcNode->hasSpecializedParams();
    }
  }
  return false;
}


void Typifier::annotateTypeConv(std::shared_ptr<AstNode> node, const Type& dstType)
{
  /*
    plain <- plain     (bitcast)
    plain <- atom      atom_2_x
    atom  <- atom      ok
    atom  <- any       type_check
    atom  <- plain     wrap_atom

    as a special check: if node is a function call to a generic function, it
    always has ATOM return representation, even if the type is plain.
   */
  bool isCallToGF = isNodeCallToGenericFunction(node.get());

  // this is a last resort stop hack, if either the node has not a valid type
  // or the dstType is not valid.  This may happen if we had parsing or typify
  // errors before.
  if (!node->type().isDef() || !dstType.isDef()) {
    node->setTypeConv(kTypeCheckConv);
    return;
  }

  if (dstType.isPlainType()) {
    if (isCallToGF) {
      // req. atom_2_x
      node->setTypeConv(kAtom2PlainConv);
    }
    else if (node->type().isPlainType()) {
      // ok.  TODO: maybe some bitcast between int/short/long, etc.
    }
    else {
      // req. atom_2_x
      node->setTypeConv(kAtom2PlainConv);
    }
  }
  else {
    if (isCallToGF) {
      // requires type_check
      node->setTypeConv(kTypeCheckConv);
    }
    else if (node->type().isPlainType()) {
      // req. wrap_atom
      node->setTypeConv(kPlain2AtomConv);
    }
    else if (node->type().isAny() || node->type().isClangAtom()) {
      // requires type_check
      node->setTypeConv(kTypeCheckConv);
    }
    else {
      // OK
    }
  }

  node->setDstType(dstType);
}


void Typifier::enforceAtomTypeConv(std::shared_ptr<AstNode> node, const Type& dstType)
{
  /*
    atom  <- atom      ok
    atom  <- any       type_check
    atom  <- plain     wrap_atom
   */
  bool isCallToGF = isNodeCallToGenericFunction(node.get());

  if (isCallToGF) {
    // requires type_check
    node->setTypeConv(kTypeCheckConv);
  }
  else if (node->type().isPlainType()) {
    // req. wrap_atom
    node->setTypeConv(kPlain2AtomConv);
  }
  else if (node->type().isAny() || node->type().isClangAtom()) {
    // requires type_check
    node->setTypeConv(kTypeCheckConv);
  }
  else {
    // OK
  }

  node->setDstType(dstType);
}


void Typifier::setBodyLastDstType(std::shared_ptr<AstNode> body, const Type& dstType)
{
  body->setDstType(dstType);
  if (auto block = std::dynamic_pointer_cast<BlockNode>(body)) {
    hr_assert(!block->children().empty());

    setBodyLastDstType(block->children().back(), dstType);
  }
}


void Typifier::setupBindingNodeType(std::shared_ptr<BindingNode> node, zstring errdesc)
{
  hr_assert(node->scope());

  if (node->type().isDef() && node->type().isOpen())
    return;

  String typenm = (node->type().isDef() ? node->type().typeId() : Names::kAnyTypeName);
  Type bindty =
      (node->type().isDef()
           ? node->scope()->lookupType(node->type())
           : node->scope()->lookupType(Names::kAnyTypeName, K(showAmbiguousSymDef)));
  if (!bindty.isDef()) {
    HR_LOG(kError, node->srcpos(), E_UndefinedType)
        << "undefined type '" << typenm << "' in " << errdesc;
    node->setType(Type::makeAny());
    if (node->initExpr()) {
      node->initExpr()->setDstType(Type::makeAny());
      annotateTypeConv(node->initExpr(), node->type());
    }
  }
  else {
    hr_assert(bindty.isDef());
    // if (bindty.isOpen()) {
    //   if (node->type().isDef())
    //     bindty = node->scope()->normalizeType(bindty, node->type());
    // }
    node->setType(bindty);

    if (node->initExpr()) {
      if (bindty.isAny()) {
        // infer the vardef type from the init expression
        node->setType(node->initExpr()->type());
        node->initExpr()->setDstType(node->initExpr()->type());
        annotateTypeConv(node->initExpr(), node->type());
      }
      else if (!node->initExpr()->type().isDef()) {
        if (std::dynamic_pointer_cast<UndefNode>(node->initExpr())) {
          node->initExpr()->setDstType(bindty);
        }
        else {
          HR_LOG(kError, node->initExpr()->srcpos(), E_TypeMismatch)
              << "Undefined type in " << errdesc << " initialization";
          node->initExpr()->setDstType(Type::makeAny());
        }
      }
      else if (!isContravariant(bindty, node->initExpr()->type(), *node->scope(),
                                node->srcpos())) {
        HR_LOG(kError, node->initExpr()->srcpos(), E_TypeMismatch)
            << "type mismatch in " << errdesc << " initialization: ";
        HR_LOG(kError, node->initExpr()->srcpos(), E_TypeMismatch)
            << "expected " << bindty << " found " << node->initExpr()->type();
        node->initExpr()->setDstType(Type::makeAny());
      }
      else {
        annotateTypeConv(node->initExpr(), node->type());
      }
    }
  }
}


void Typifier::setupFunctionNodeType(std::shared_ptr<FunctionNode> node)
{
  FunctionParamVector params;
  for (size_t i = 0; i < node->params().size(); i++) {
    auto prmnd = std::dynamic_pointer_cast<ParamNode>(node->params()[i]);
    hr_assert(prmnd);

    if (prmnd->flags() == kPosArg) {
      params.push_back(FunctionParameter(FunctionParameter::kParamPos, !K(isSpec),
                                         String(), prmnd->type()));
    }
    else if (prmnd->flags() == kSpecArg) {
      params.push_back(FunctionParameter(FunctionParameter::kParamPos, K(isSpec),
                                         String(), prmnd->type()));
    }
    else if (prmnd->flags() == kNamedArg) {
      params.push_back(FunctionParameter(FunctionParameter::kParamNamed, !K(isSpec),
                                         prmnd->key(), prmnd->type()));
    }
    else if (prmnd->flags() == kRestArg) {
      params.push_back(FunctionParameter(FunctionParameter::kParamRest, !K(isSpec),
                                         String(), prmnd->type()));
    }
    else {
      hr_invalid("undefined parameter type?");
    }
  }

  if (!node->retType().isDef() || node->retType().isAny()) {
    // try to infer the return type from the body's type
    if (node->body()) {
      node->setRetType(node->body()->type());
    }
    else {
      // TODO: make warnings like this optional
      // HR_LOG(kWarning, node->srcpos(), E_UndefinedType)
      //          << "undefined return type on function defaults to lang|Any";
      node->setRetType(Type::makeAny());
    }
  }
  else if (!node->retType().isOpen()) {
    String typenm =
        (node->retType().isDef() ? node->retType().typeId() : Names::kAnyTypeName);
    Type retty =
        (node->retType().isDef()
             ? node->scope()->lookupType(node->retType())
             : node->scope()->lookupType(Names::kAnyTypeName, K(showAmbiguousSymDef)));
    if (!retty.isDef()) {
      HR_LOG(kError, node->srcpos(), E_UndefinedType)
          << "undefined return type '" << typenm << "'";
      node->setRetType(Type::makeAny());
    }
    else {
      node->setRetType(retty);
    }
  }

  FunctionSignature sign(!K(isGeneric), String(), node->retType(), params);
  node->setType(Type::makeFunction(sign));
}


class FindReturnTraverseDelegate {
public:
  bool apply(std::shared_ptr<AstNode> node, TraversePhase phase)
  {
    if (phase == TraversePhase::before) {
      // don't dive into nested functions
      if (std::dynamic_pointer_cast<FunctionNode>(node))
        return false;

      auto apply = std::dynamic_pointer_cast<ApplyNode>(node);
      if (apply && apply->isSimpleCall() && apply->simpleCallName() == Names::kLangReturn)
        fReturns.push_back(apply);
    }
    return true;
  }

  std::vector<std::shared_ptr<AstNode>> fReturns;
};


void Typifier::checkFunctionReturnType(std::shared_ptr<FunctionNode> node)
{
  if (node->body()) {
    if (!isContravariant(node->retType(), node->body()->type(), *node->scope(),
                         node->srcpos()) &&
        !containsAny(node->body()->type(), node->srcpos())) {
      HR_LOG(kError, node->srcpos(), E_TypeMismatch)
          << "function's body type does not match its return type";
    }


    FindReturnTraverseDelegate delegate;
    Traversator<FindReturnTraverseDelegate>(delegate).traverseNode(node);

    for (auto ret : delegate.fReturns) {
      if (!isContravariant(node->retType(), ret->type(), *ret->scope(), ret->srcpos())) {
        HR_LOG(kError, ret->srcpos(), E_TypeMismatch)
            << "return's type does not match outer block type";
      }
    }
  }
}


void Typifier::checkArgParamType(TypeCtx& localCtx,
                                 const std::shared_ptr<ParamNode>& param,
                                 std::shared_ptr<AstNode> arg, int idx,
                                 const String& funcName)
{
  if (param->type().isOpen()) {
    if (!param->type().matchGenerics(localCtx, arg->type(), *arg->scope(),
                                     arg->srcpos())) {
      HR_LOG(kError, arg->srcpos(), E_TypeMismatch)
          << "type mismatch for argument " << idx << " in call to function '" << funcName
          << "' (" << param->type() << " for " << arg->type() << ")";
      return;
    }
  }
  else {
    if (!isContravariant(param->type(), arg->type(), *arg->scope(), arg->srcpos()) &&
        !containsAny(arg->type(), arg->srcpos())) {
      HR_LOG(kError, arg->srcpos(), E_TypeMismatch)
          << "type mismatch for argument " << idx << " in call to function '" << funcName
          << "'";
      return;
    }
  }

  // if the parameter is specialized enforce passing to AtomType
  if (param->flags() == kSpecArg)
    enforceAtomTypeConv(arg, param->type());
  else
    annotateTypeConv(arg, param->type());
}


Typifier::KeyargReturn Typifier::findKeyedArg(const NodeList& args, size_t argidx,
                                              const String& key)
{
  auto retval = KeyargReturn{nullptr, 0};

  for (size_t i = argidx; i < args.size(); i++) {
    auto keyarg = dynamic_cast<KeyargNode*>(args[i].get());
    if (!keyarg)
      return retval;

    if (keyarg->key() == key) {
      return KeyargReturn{keyarg, i};
    }
  }
  return retval;
}


void Typifier::reorderArguments(std::shared_ptr<ApplyNode> node,
                                const FunctionNode* funcNode)
{
  const NodeList& funcParams = funcNode->params();

  NodeList newArgs;
  newArgs.reserve(funcParams.size());

  NodeList posArgs;
  posArgs.reserve(node->children().size());
  std::map<String, std::shared_ptr<KeyargNode>> namedArgs;

  for (auto arg : node->children()) {
    if (auto keyArg = std::dynamic_pointer_cast<KeyargNode>(arg)) {
      namedArgs.insert(std::make_pair(keyArg->key(), keyArg));
    }
    else {
      posArgs.push_back(arg);
    }
  }

  // synchronize the arguments of the apply node with the function declaration
  size_t posArgIdx = 0;
  for (auto i = 0; i < funcParams.size(); ++i) {
    auto param = std::dynamic_pointer_cast<ParamNode>(funcParams[i]);
    hr_assert(param);

    if (param) {
      if (param->flags() == kPosArg || param->flags() == kSpecArg) {
        if (posArgIdx >= posArgs.size()) {
          // there was an error before.  Not enough positional arguments
          return;
        }
        newArgs.push_back(posArgs[posArgIdx]);
        ++posArgIdx;
      }
      else if (param->flags() == kNamedArg) {
        auto iArg = namedArgs.find(param->key());
        if (iArg != namedArgs.end()) {
          newArgs.push_back(iArg->second);
        }
        else {
          hr_assert(param->initExpr());
          // add keyarg parameters with the default values
          newArgs.push_back(makeKeyargNode(param->scope(), param->srcpos(), param->key(),
                                           param->initExpr()->clone()));
        }
      }
      else if (param->flags() == kRestArg) {
        // TODO(gck): create code to generate a array on stack, and assign
        // the remaining arguments to it; then pass the array as
        // single parameter.
        // TODO(gck): better: pass arguments as such, but access the
        // arguments inside the function as arg.
        for (auto i = posArgIdx; i < posArgs.size(); ++i) {
          newArgs.push_back(posArgs[i]);
        }

        break;
      }
    }
  }

  node->replaceChildren(newArgs);
}


Type Typifier::typifyMatchAndCheckParameters(const SrcPos& srcpos, const NodeList& args,
                                             const FunctionNode* funcNode,
                                             const String& funcName)
{
  const NodeList& funcParams = funcNode->params();

  TypeCtx localCtx;
  size_t argidx = 0;
  std::set<int> argIndicesUsed;
  for (size_t i = 0; i < funcParams.size(); ++i) {
    auto param = std::dynamic_pointer_cast<ParamNode>(funcParams[i]);

    if (param) {
      if (param->flags() == kPosArg || param->flags() == kSpecArg) {
        auto arg = args[argidx];

        if (i >= args.size()) {
          HR_LOG(kError, srcpos, E_BadArgNumber) << "not enough arguments";
          return Type();
        }
        checkArgParamType(localCtx, param, arg, i, funcName);
        argidx++;
      }
      else if (param->flags() == kNamedArg) {
        auto keyval = findKeyedArg(args, argidx, param->key());
        if (!keyval.fKeyarg) {
          // I think this can't happen anymore.  Default parameter
          // values are rendered in place in the nodify (?) phase.
          hr_invalid("");
          if (param->initExpr()) {
            if (param->initExpr()->type().isOpen()) {
              Type iety = param->initExpr()->type().replaceGenerics(localCtx);

              if (iety.isDef())
                param->initExpr()->setType(iety);
              else
                HR_LOG(kError, srcpos, E_TypeMismatch)
                    << "init value to argument " << (int)i << " of function " << funcName
                    << " has unmatched generic type";
            }

            checkArgParamType(localCtx, param, param->initExpr(), i, funcName);
          }
        }
        else {
          if (keyval.fKeyarg->value()->type().isOpen()) {
            Type keyvalty = keyval.fKeyarg->value()->type().replaceGenerics(localCtx);
            if (auto applyNd =
                    std::dynamic_pointer_cast<ApplyNode>(keyval.fKeyarg->value())) {
              for (auto cNd : applyNd->children()) {
                Type cNdTy = cNd->type().replaceGenerics(localCtx);
                if (cNdTy.isDef())
                  cNd->setType(cNdTy);
              }
            }

            if (keyvalty.isDef())
              keyval.fKeyarg->value()->setType(keyvalty);
            else
              HR_LOG(kError, srcpos, E_TypeMismatch)
                  << "init value to argument " << (int)i << " of function " << funcName
                  << " has unmatched generic type";
          }

          checkArgParamType(localCtx, param, keyval.fKeyarg->value(), keyval.fIdx,
                            funcName);
          argIndicesUsed.insert(keyval.fIdx);
        }
      }
      else if (param->flags() == kRestArg) {
        // TODO: type of rest arguments
        // TypeVector types;
        // for (size_t j = 0; j < args.size(); j++) {
        //   if (argIndicesUser.find(j) != argIndicesUser.end()) {
        //     // argument not yet used
        //     types.push_back(args[j]->type());
        //   }
        // }
        // Type seq = Type::makeIntersection(types, true);
        argidx = args.size();
      }
      else {
        hr_invalid("");
      }
    }
  }

  // if (argidx < args.size()) {
  //   HR_LOG(kError, srcpos, E_BadArgNumber) << "Too much arguments";
  // }

  if (funcNode->retType().isOpen()) {
    Type retty = funcNode->retType().replaceGenerics(localCtx);

    if (retty.isDef())
      return retty.setIsValueType(funcNode->retType().isValueType());
    else
      HR_LOG(kError, srcpos, E_TypeMismatch)
          << "function has unmatched generic return type.";

    // Type retty = funcnode->retType();
    // if (localCtx.hasType(retty.typeName())) {
    //   node.setType(localCtx.lookupType(retty.typeName()));
    // }
    // else {
    //   HR_LOG(kError, node.srcpos(), E_TypeMismatch) <<
    //          "function has unmatched generic return type.";
    // }

    return Type();
  }
  else
    return funcNode->retType();
}


void Typifier::checkAllocateArraySignature(std::shared_ptr<ApplyNode> node)
{
  const NodeList& args = node->children();
  if (args.size() == 2) {
    hr_assert(node->type().isArray());

    Type arrayBaseType = node->type().arrayBaseType();
    if (arrayBaseType.isBaseType()) {
      // this is always ok.
    }
    else if (arrayBaseType.isType()) {
      // TODO: when we distinguish nullable types, this is only allowed if the
      // type is nullable
      HR_LOG(kError, node->srcpos(), E_ArrayReqDefaultCtor)
          << "Can't create array of Type base type without explicit initializer";
    }
  }
}


String Typifier::operatorNameByOp(OperatorType type) const
{
  switch (type) {
  case kOpBitAnd: return String(MID_bitand);
  case kOpBitOr: return String(MID_bitor);
  case kOpBitXor: return String(MID_bitxor);
  case kOpCompare: return String(MID_compare);
  case kOpConcat: return String(MID_concat);
  case kOpDivide: return String(MID_divide);
  case kOpEqual: return String(MID_equalq);
  case kOpExponent: return String(MID_exponent);
  case kOpFold: return String(MID_fold);
  case kOpGreater: return String(MID_greaterq);
  case kOpGreaterEqual: return String(MID_greaterequalq);
  case kOpIn: return String(MID_in);
  case kOpIsa: return String(MID_isaQ);
  case kOpLess: return String(MID_lessq);
  case kOpLessEqual: return String(MID_lessequalq);
  case kOpLogicalAnd: return String(MID_logand);
  case kOpLogicalOr: return String(MID_logor);
  case kOpMinus: return String(MID_subtract);
  case kOpMod: return String(MID_mod);
  case kOpRem: return String(MID_rem);
  case kOpMultiply: return String(MID_multiply);
  case kOpPlus: return String(MID_add);
  case kOpShiftLeft: return String(MID_shiftleft);
  case kOpShiftRight: return String(MID_shiftright);
  case kOpUnequal: return String(MID_unequalq);

  case kOpInvalid:
  case kOpAssign: return String("=");
  case kOpAs: return String(MID_castto);
  case kOpBy: return String(MID_byid);
  case kOpMapTo: return String(MID_mapto);
  case kOpRange: return String(".."); hr_invalid("");
  }

  return String();
}


bool Typifier::checkBinaryFunctionCall(std::shared_ptr<BinaryNode> node,
                                       const String& funcName,
                                       std::shared_ptr<AstNode> leftArg,
                                       std::shared_ptr<AstNode> rightArg)
{
  auto funcNode = node->scope()->lookupFunction(funcName, K(showAmbiguousSymDef));

  if (funcNode) {
    auto args = makeNodeList({leftArg, rightArg});

    if (auto bestFuncNode = node->scope()->lookupBestFunctionOverload(
            funcName, typesForArgs(args), node->srcpos(), K(showAmbiguousSymDef))) {
      if (bestFuncNode.fNode->params().size() > 2) {
        HR_LOG(kError, node->srcpos(), E_WrongOperatorFuncSign)
            << "operator implementation with wrong parameter count";
        return false;
      }

      // auto newBase = makeSymbolNode(node->scope(), node->srcpos(), bestFuncNode.fName);
      // node->setBase(newBase);
      node->setRefFunction(bestFuncNode.fNode);
      // typf->reorderArguments(node, bestFuncNode.get());

      Type type =
          typifyMatchAndCheckParameters(node->srcpos(), args, funcNode.get(), funcName);
      if (type.isDef()) {
        node->setType(type);
        return true;
      }
    }
    else {
      HR_LOG(kError, node->srcpos(), E_NoMatchingFunction)
          << "no matching implementation for operator: " << funcName;
    }
  }

  return false;
}


bool Typifier::isInOwnershipTransferContext() const
{
  return !fInOwnershipTransferContext.empty() ? fInOwnershipTransferContext.front()
                                              : false;
}


//------------------------------------------------------------------------------

TypifyPass::TypifyPass(int level, Compiler& compiler)
    : AstNodeCompilePass(level, K(showNodeType))
    , fCompiler(compiler)
{
}


std::shared_ptr<AstNode> TypifyPass::doApply(std::shared_ptr<AstNode> src)
{
  auto ty = Typifier{fCompiler};
  ty.typifyNode(src);
  return src;
}

}  // namespace herschel
