/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include "common.hpp"

#include "compilepass.hpp"
#include "token.hpp"

#include <vector>

namespace herschel {

class ApplyNode;
class AstNode;
class BinaryNode;
class BindingNode;
class FunctionNode;
class KeyargNode;
class ParamNode;
class SrcPos;
class String;
class Type;
class TypeCtx;


//! A vector of AstNodes.
using NodeList = std::vector<std::shared_ptr<AstNode>>;


/*! Implements a code annotation traversal which determines and checks
 * types for each node in the abstract syntax tree.  After being
 * applied all nodes should have proper types and typeConversions set
 * which can be accessed via the @c AstNode::type(), @c
 * AstNode::dstType(), and @c AstNode::typeConv() methods.
 */
class TypeChecker {
public:
  TypeChecker();

  /*! Typify @p node */
  void checkNode(std::shared_ptr<AstNode> node);

  /*! Typify each node in @p nl sequentially */
  void checkNodeList(const NodeList& nl);

  /*! Find all return branches in the body of FunctionNode @p node and check
   * that their types match the return type of @p node.  Requires that
   * setupFunctionNodeType() has been successfully run before. */
  void checkFunctionReturnType(std::shared_ptr<FunctionNode> node);
};


/*! Compiler pass for determining and checking the types of APT nodes. */
class TypeCheckPass : public AstNodeCompilePass {
public:
  TypeCheckPass(int level);
  std::shared_ptr<AstNode> doApply(std::shared_ptr<AstNode> src) override;
};

}  // namespace herschel
