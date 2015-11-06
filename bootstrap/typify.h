/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include "compilepass.h"

#include <vector>

namespace herschel
{
  class ApplyNode;
  class AptNode;
  class ArrayNode;
  class ArrayTypeNode;
  class AssignNode;
  class BinaryNode;
  class BindingNode;
  class BlockNode;
  class BoolNode;
  class CastNode;
  class CharNode;
  class CompileUnitNode;
  class DefNode;
  class DictNode;
  class FuncDefNode;
  class FunctionNode;
  class IfNode;
  class IntNode;
  class KeyargNode;
  class KeywordNode;
  class LetNode;
  class MatchNode;
  class UnaryNode;
  class OnNode;
  class ParamNode;
  class RangeNode;
  class RationalNode;
  class RealNode;
  class SelectNode;
  class SlotdefNode;
  class SlotRefNode;
  class StringNode;
  class SymbolNode;
  class TypeDefNode;
  class UnitConstNode;
  class VardefNode;
  class VectorNode;
  class WhileNode;
  class Type;
  class UndefNode;


  //! A vector of AptNodes.
  using NodeList = std::vector<std::shared_ptr<AptNode>>;

  //------------------------------------------------------------------------------

  //! Implements a code annotation traversal which determines and checks types
  //! for each node in the abstract syntax tree.  After being applied all
  //! nodes should have proper types and typeConversions set which can be
  //! accessed via the \c AptNode::type(), \c AptNode::dstType(), and \c
  //! AptNode::typeConv() methods.  The main entry point is
  //! \c typifyRecursively().

  class Typifier
  {
  public:
    Typifier();

    //! The main entry function, which starts a recursive processing on \p node.
    void typifyRecursively(AptNode& node);

    //! Typify implementation for CompileUnitNodes (cf. \c TypifyPass)
    void typify(CompileUnitNode& node);

    //! Typify implementation for SymbolNodes (cf. \c TypifyPass)
    void typify(SymbolNode& node);

    //! Typify implementation for DefNodes (cf. \c TypifyPass)
    void typify(DefNode& node);
    //! Typify implementation for LetNodes (cf. \c TypifyPass)
    void typify(LetNode& node);
    //! Typify implementation for BlockNodes (cf. \c TypifyPass)
    void typify(BlockNode& node);

    //! Typify implementation for ParamNodes (cf. \c TypifyPass)
    void typify(ParamNode& node);

    //! Typify implementation for ApplyNodes (cf. \c TypifyPass)
    void typify(ApplyNode& node);
    //! Typify implementation for ArrayNodes (cf. \c TypifyPass)
    void typify(ArrayNode& node);
    //! Typify implementation for ArrayTypeNodes (cf. \c TypifyPass)
    void typify(ArrayTypeNode& node);
    //! Typify implementation for AssignNodes (cf. \c TypifyPass)
    void typify(AssignNode& node);
    //! Typify implementation for BinaryNodes (cf. \c TypifyPass)
    void typify(BinaryNode& node);
    //! Typify implementation for BoolNodes (cf. \c TypifyPass)
    void typify(BoolNode& node);
    //! Typify implementation for CastNodes (cf. \c TypifyPass)
    void typify(CastNode& node);
    //! Typify implementation for CharNodes (cf. \c TypifyPass)
    void typify(CharNode& node);
    //! Typify implementation for DictNodes (cf. \c TypifyPass)
    void typify(DictNode& node);
    //! Typify implementation for FunctionNodes (cf. \c TypifyPass)
    void typify(FunctionNode& node);
    //! Typify implementation for IfNodes (cf. \c TypifyPass)
    void typify(IfNode& node);
    //! Typify implementation for IntNodes (cf. \c TypifyPass)
    void typify(IntNode& node);
    //! Typify implementation for KeyargNodes (cf. \c TypifyPass)
    void typify(KeyargNode& node);
    //! Typify implementation for KeywordNodes (cf. \c TypifyPass)
    void typify(KeywordNode& node);
    //! Typify implementation for MatchNodes (cf. \c TypifyPass)
    void typify(MatchNode& node);
    //! Typify implementation for UnaryNodes (cf. \c TypifyPass)
    void typify(UnaryNode& node);
    //! Typify implementation for OnNodes (cf. \c TypifyPass)
    void typify(OnNode& node);
    //! Typify implementation for RangeNodes (cf. \c TypifyPass)
    void typify(RangeNode& node);
    //! Typify implementation for RationalNodes (cf. \c TypifyPass)
    void typify(RationalNode& node);
    //! Typify implementation for RealNodes (cf. \c TypifyPass)
    void typify(RealNode& node);
    //! Typify implementation for SelectNodes (cf. \c TypifyPass)
    void typify(SelectNode& node);
    //! Typify implementation for SlotdefNodes (cf. \c TypifyPass)
    void typify(SlotdefNode& node);
    //! Typify implementation for SlotRefNodes (cf. \c TypifyPass)
    void typify(SlotRefNode& node);
    //! Typify implementation for StringNodes (cf. \c TypifyPass)
    void typify(StringNode& node);
    //! Typify implementation for TypeDefNodes (cf. \c TypifyPass)
    void typify(TypeDefNode& node);
    //! Typify implementation for TypeNodes (cf. \c TypifyPass)
    void typify(TypeNode& node);
    //! Typify implementation for UnitConstNodes (cf. \c TypifyPass)
    void typify(UnitConstNode& node);
    //! Typify implementation for VectorNodes (cf. \c TypifyPass)
    void typify(VectorNode& node);
    //! Typify implementation for WhileNodes (cf. \c TypifyPass)
    void typify(WhileNode& node);
    //! Typify implementation for VardefNodes (cf. \c TypifyPass)
    void typify(VardefNode& node);
    //! Typify implementation for FuncDefNodes (cf. \c TypifyPass)
    void typify(FuncDefNode& node);
    //! Typify implementation for UndefNodes (cf. \c TypifyPass)
    void typify(UndefNode& node);

  private:
    void annotateTypeConv(AptNode& toNode, const Type& type);
    void enforceAtomTypeConv(AptNode& node, const Type& dstType);
    void setBodyLastDstType(AptNode& body, const Type& dstType);
    bool isNodeCallToGenericFunction(const AptNode& node) const;

    //! Typify \p node
    void typifyNode(AptNode& node);

    //! Typify each node in \p nl sequentially
    void typifyNodeList(NodeList& nl);

    //! Compute the function signature type for a FunctionNode \p node
    //! including return type and parameter types.
    void setupFunctionNodeType(FunctionNode& node);

    //! Find all return branches in the body of FunctionNode \p node and check
    //! that their types match the return type of \p node.  Requires that
    //! setupFunctionNodeType() has been successfully run before.
    void checkFunctionReturnType(FunctionNode& node);

    //! Check that an allocate function call without explicit \c "value:"
    //! argument works on types without positional ctor parameters.
    void checkAllocateArraySignature(ApplyNode& node);

    //! Determine the proper type for a binding node \p node and set it on \p
    //! node.  \p errdesc is used as a context name to be shown in possible
    //! error messages.
    void setupBindingNodeType(BindingNode& node, zstring errdesc);

    void checkArgParamType(TypeCtx& localCtx, const ParamNode& param,
                           AptNode& arg, int idx);

    //! Sequentially determine and check types for the arguments parameters of ApplyNode
    //! \p node which is to be applied to \p funcNode.
    //!
    //! @param node       the ApplyNode to typify
    //! @param funcNode   the FunctionNode as looked up from the scope.
    void typifyMatchAndCheckParameters(ApplyNode& node,
                                       const FunctionNode& funcNode);
    //! Sequentially determine and check types for the arguments \p args
    //! \p node which is to be applied to \p funcNode.
    //!
    //! @param srcpos     the source position where the function is applied
    //! @param args       the arguments to apply to \p funcNode
    //! @param funcNode   the FunctionNode as looked up from the scope.
    //! @return the return type of the function applied
    Type typifyMatchAndCheckParameters(const SrcPos& srcpos,
                                       const NodeList& args,
                                       const FunctionNode& funcNode);

    //! Looks up a matching function implementing the operator of the
    //! binary node \p node and determine and set the types if applying the
    //! arguments \p leftArg and \p rightArg to it.
    //!
    //! @return \c true if such a function can be found and all types apply
    //!         properly.
    bool checkBinaryFunctionCall(BinaryNode& node,
                                 const String& funcName,
                                 std::shared_ptr<AptNode> leftArg,
                                 std::shared_ptr<AptNode> rightArg);
    //! Returns the builtin function name for the operator \p type.
    String operatorNameByOp(OperatorType type) const;

    //! Helper class for the findKeyedArg() function.
    struct KeyargReturn
    {
      const KeyargNode* fKeyarg;
      size_t fIdx;
    };

    //! When determining function call types, this finds the matching argument
    //! for a keyed parameter named \p key from \p args starting at \p
    //! argidx.  If no matching argument can be found the returned structure
    //! has the value <tt>{ nullptr, 0 }</tt>;
    KeyargReturn findKeyedArg(const NodeList& args, size_t argidx,
                              const String& key);

    //-------- data members

    enum Phase
    {
      kTypify,
      kCheck
    };

    Phase fPhase;
  };


  //--------------------------------------------------------------------------

  //! Compiler pass for determining and checking the types of APT nodes.

  class TypifyPass : public AptNodeCompilePass
  {
  public:
    TypifyPass(int level);
    std::shared_ptr<AptNode> doApply(std::shared_ptr<AptNode> src) override;
  };
};                              // namespace

