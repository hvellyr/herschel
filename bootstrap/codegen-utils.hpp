/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2020 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include "common.hpp"

#include <memory>
#include <vector>


namespace herschel {
class AstNode;
class Scope;
class SrcPos;
class Type;

std::shared_ptr<AstNode>
generateInitObjectCall(const SrcPos& srcpos, std::shared_ptr<Scope> scope,
                       std::shared_ptr<AstNode> newObjAllocExpr, const Type& type,
                       const std::vector<std::shared_ptr<AstNode>>& params);

/*! Creates the ast nodes which represent the instantiation call for
 * @p type
 *
 * @p params are the arguments pass to a instantiation call.
 */
std::shared_ptr<AstNode>
generateInstantiateCall(const SrcPos& srcpos, std::shared_ptr<Scope> scope,
                        const Type& type,
                        const std::vector<std::shared_ptr<AstNode>>& params);

}  // namespace herschel
