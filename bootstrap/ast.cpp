/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2019 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.

   - look up all name references and complete their namespaces
*/

#include "ast.hpp"

#include "predefined.hpp"

#include <set>


namespace herschel {

void BlockNode::markReturnNode(std::shared_ptr<Scope> scope)
{
  if (!fChildren.empty()) {
    auto lastExpr = fChildren.back();
    if (fChildren.size() > 1 && std::dynamic_pointer_cast<SymbolNode>(lastExpr))
      return;

    auto varName = uniqueName("retv");
    auto newVarNode = makeVardefNode(scope, lastExpr->srcpos(), varName, kNormalVar,
                                     K(isLocal), Type::makeAny(), lastExpr);
    scope->registerVar(lastExpr->srcpos(), varName, newVarNode);

    fChildren.back() = makeLetNode(fScope, newVarNode);
    fChildren.push_back(makeSymbolNode(scope, lastExpr->srcpos(), varName));
  }
}


void BlockNode::insertFinalizers(const NodeList& exprs)
{
  auto& ndChildren = children();
  hr_assert(!ndChildren.empty());

  std::set<String> existingFinalizers;
  for (auto& nd : ndChildren) {
    for (const auto& mark : nd->markers()) {
      if (mark.startsWith(Names::kFinalizerMarker)) {
        existingFinalizers.insert(mark);
      }
    }
  }

  NodeList newFinalizers;
  for (const auto& finalizer : exprs) {
    auto finalizeExists = false;
    for (const auto& mark : finalizer->markers()) {
      if (mark.startsWith(Names::kFinalizerMarker)) {
        if (existingFinalizers.count(mark) > 0) {
          finalizeExists = true;
        }
      }
    }

    if (!finalizeExists) {
      newFinalizers.push_back(finalizer);
    }
  }

  ndChildren.insert(prev(ndChildren.end()), begin(newFinalizers), end(newFinalizers));
}

}  // namespace herschel
