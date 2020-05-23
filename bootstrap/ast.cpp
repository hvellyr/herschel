/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2019 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.

   - look up all name references and complete their namespaces
*/

#include "ast.hpp"

#include <set>


namespace herschel {

void BlockNode::markReturnNode(std::shared_ptr<Scope> scope)
{
  if (!fChildren.empty()) {
    auto lastExpr = fChildren.back();
    if (std::dynamic_pointer_cast<SymbolNode>(lastExpr))
      return;

    auto varName = uniqueName("retv");
    auto newVarNode = makeVardefNode(scope, lastExpr->srcpos(), varName, kNormalVar,
                                     K(isLocal), Type::makeAny(), lastExpr);
    scope->registerVar(lastExpr->srcpos(), varName, newVarNode);

    fChildren.back() = makeLetNode(fScope, newVarNode);
    fChildren.push_back(makeSymbolNode(scope, lastExpr->srcpos(), varName));
  }
}

}  // namespace herschel
