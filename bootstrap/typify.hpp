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
 * AstNode::dstType(), and @c AstNode::typeConv() methods.  The main
 * entry point is @c typifyRecursively().
 */
class Typifier {
public:
  Typifier();

  /*! The main entry function, which starts a recursive processing on @p node. */
  void typifyRecursively(std::shared_ptr<AstNode> node);

  void annotateTypeConv(std::shared_ptr<AstNode> toNode, const Type& type);
  void enforceAtomTypeConv(std::shared_ptr<AstNode> node, const Type& dstType);
  void setBodyLastDstType(std::shared_ptr<AstNode> body, const Type& dstType);
  bool isNodeCallToGenericFunction(const AstNode* node) const;

  /*! Typify @p node */
  void typifyNode(std::shared_ptr<AstNode> node);

  /*! Typify each node in @p nl sequentially */
  void typifyNodeList(const NodeList& nl);

  /*! Compute the function signature type for a FunctionNode @p node
   * including return type and parameter types. */
  void setupFunctionNodeType(std::shared_ptr<FunctionNode> node);

  /*! Find all return branches in the body of FunctionNode @p node and check
   * that their types match the return type of @p node.  Requires that
   * setupFunctionNodeType() has been successfully run before. */
  void checkFunctionReturnType(std::shared_ptr<FunctionNode> node);

  /*! Check that an allocate function call without explicit @c "value:"
   * argument works on types without positional ctor parameters. */
  void checkAllocateArraySignature(std::shared_ptr<ApplyNode> node);

  /*! Determine the proper type for a binding node @p node and set it on @p
   * node.  @p errdesc is used as a context name to be shown in possible
   * error messages. */
  void setupBindingNodeType(std::shared_ptr<BindingNode> node, zstring errdesc);

  void checkArgParamType(TypeCtx& localCtx, const std::shared_ptr<ParamNode>& param,
                         std::shared_ptr<AstNode> arg, int idx);

  /*! Sequentially determine and check types for the arguments parameters of ApplyNode
   * @p node which is to be applied to @p funcNode.
   *
   * @param node       the ApplyNode to typify
   * @param funcNode   the FunctionNode as looked up from the scope. */
  void typifyMatchAndCheckParameters(std::shared_ptr<ApplyNode> node,
                                     const FunctionNode* funcNode);
  /*! Sequentially determine and check types for the arguments @p args
   * @p node which is to be applied to @p funcNode.
   *
   * @param srcpos     the source position where the function is applied
   * @param args       the arguments to apply to @p funcNode
   * @param funcNode   the FunctionNode as looked up from the scope.
   * @return the return type of the function applied */
  Type typifyMatchAndCheckParameters(const SrcPos& srcpos, const NodeList& args,
                                     const FunctionNode* funcNode);

  /*! Looks up a matching function implementing the operator of the
   * binary node @p node and determine and set the types if applying the
   * arguments @p leftArg and @p rightArg to it.
   *
   * @return @c true if such a function can be found and all types apply
   *         properly. */
  bool checkBinaryFunctionCall(std::shared_ptr<BinaryNode> node, const String& funcName,
                               std::shared_ptr<AstNode> leftArg,
                               std::shared_ptr<AstNode> rightArg);
  /*! Returns the builtin function name for the operator @p type. */
  String operatorNameByOp(OperatorType type) const;

  /*! Helper class for the findKeyedArg() function. */
  struct KeyargReturn {
    const KeyargNode* fKeyarg;
    size_t fIdx;
  };

  /*! When determining function call types, this finds the matching argument
   * for a keyed parameter named @p key from @p args starting at @p
   * argidx.  If no matching argument can be found the returned structure
   * has the value <tt>{ nullptr, 0 }</tt>; */
  KeyargReturn findKeyedArg(const NodeList& args, size_t argidx, const String& key);

  enum Phase { kTypify, kCheck };

  Phase fPhase;
};


/*! Compiler pass for determining and checking the types of APT nodes. */
class TypifyPass : public AstNodeCompilePass {
public:
  TypifyPass(int level);
  std::shared_ptr<AstNode> doApply(std::shared_ptr<AstNode> src) override;
};

}  // namespace herschel
