/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2020 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "codegen-utils.hpp"

#include "ast.hpp"
#include "log.hpp"
#include "predefined.hpp"
#include "require.hpp"
#include "scope.hpp"
#include "type.hpp"

#include <memory>
#include <vector>


namespace herschel {

std::shared_ptr<AstNode>
generateInitObjectCall(const SrcPos& srcpos, std::shared_ptr<Scope> scope,
                       std::shared_ptr<AstNode> newObjAllocExpr, const Type& type,
                       const std::vector<std::shared_ptr<AstNode>>& params)
{
  std::shared_ptr<AstNode> funcNode;
  if (type.isOpen()) {
    auto apply = makeApplyNode(scope, srcpos,
                               makeSymbolNode(scope, srcpos, Names::kLangInitFunctor));
    apply->appendNode(makeTypeNode(scope, srcpos, type));
    funcNode = apply;
  }
  else {
    funcNode = makeSymbolNode(scope, srcpos, Names::kInitFuncName);
  }

  auto initExpr = makeApplyNode(scope, srcpos, funcNode);
  initExpr->appendNode(newObjAllocExpr);
  initExpr->appendNodes(params);

  return initExpr;
}


std::shared_ptr<AstNode>
generateInstantiateCall(const SrcPos& srcpos, std::shared_ptr<Scope> scope,
                        const Type& type,
                        const std::vector<std::shared_ptr<AstNode>>& params)
{
  auto ty = scope->lookupType(type);
  if (ty.isRecord() && ty.isValueType()) {
    ScopeHelper scopeHelper(scope, !K(doExport), K(isInnerScope), !K(doPropIntern),
                            kScopeL_Local);

    auto block = makeBlockNode(scope, srcpos);
    auto localVarSym = uniqueName("init");
    auto localVar =
        makeVardefNode(scope, srcpos, localVarSym, kNormalVar, K(isLocal), ty, nullptr);
    scope->registerVar(srcpos, localVarSym, localVar);
    auto localVarNd = makeLetNode(scope, localVar);

    block->appendNode(localVarNd);
    block->appendNode(generateInitObjectCall(
        srcpos, scope, makeSymbolNode(scope, srcpos, localVarSym), type, {}));
    block->appendNode(makeSymbolNode(scope, srcpos, localVarSym));

    block->markReturnNode(scope);

    return makeScopeNode(scope, srcpos, block, !K(doExport), K(isInnerScope),
                         !K(doPropIntern), kScopeL_Local);
  }
  else {
    auto newObjAllocExpr =
        makeApplyNode(scope, srcpos, makeSymbolNode(scope, srcpos, Names::kLangAllocate));
    newObjAllocExpr->appendNode(makeTypeNode(scope, srcpos, type));

    return generateInitObjectCall(srcpos, scope, newObjAllocExpr, type, params);
  }
}


}  // namespace herschel
