/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_typify_h
#define bootstrap_typify_h

#include "refcountable.h"
#include "ptr.h"
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
  class NegateNode;
  class OnNode;
  class ParamNode;
  class RangeNode;
  class RationalNode;
  class RealNode;
  class SelectNode;
  class SlotdefNode;
  class StringNode;
  class SymbolNode;
  class TypeDefNode;
  class UnitConstNode;
  class VardefNode;
  class VectorNode;
  class WhileNode;
  class Type;
  class UndefNode;


  typedef std::vector<Ptr<AptNode> > NodeList;

  //------------------------------------------------------------------------------

  //! Implements a code annotation traversal which determines and checks types
  //! for each node in the abstract syntax tree.  After an appliance of this
  //! all nodes should have a proper type set which can be access via the
  //! AptNode::type() method.
  //!
  //! The main entry point is typifyRecursively().

  class Typifier : public RefCountable
  {
  public:
    Typifier();

    //! The main entry function, which starts a recursive processing on \p node.
    void typifyRecursively(AptNode* node);

    //! Typify implementation for CompileUnitNodes (cf. TypifyPass)
    void typify(CompileUnitNode* node);

    //! Typify implementation for SymbolNodes (cf. TypifyPass)
    void typify(SymbolNode* node);

    //! Typify implementation for DefNodes (cf. TypifyPass)
    void typify(DefNode* node);
    //! Typify implementation for LetNodes (cf. TypifyPass)
    void typify(LetNode* node);
    //! Typify implementation for BlockNodes (cf. TypifyPass)
    void typify(BlockNode* node);

    //! Typify implementation for ParamNodes (cf. TypifyPass)
    void typify(ParamNode* node);

    //! Typify implementation for ApplyNodes (cf. TypifyPass)
    void typify(ApplyNode* node);
    //! Typify implementation for ArrayNodes (cf. TypifyPass)
    void typify(ArrayNode* node);
    //! Typify implementation for ArrayTypeNodes (cf. TypifyPass)
    void typify(ArrayTypeNode* node);
    //! Typify implementation for AssignNodes (cf. TypifyPass)
    void typify(AssignNode* node);
    //! Typify implementation for BinaryNodes (cf. TypifyPass)
    void typify(BinaryNode* node);
    //! Typify implementation for BoolNodes (cf. TypifyPass)
    void typify(BoolNode* node);
    //! Typify implementation for CastNodes (cf. TypifyPass)
    void typify(CastNode* node);
    //! Typify implementation for CharNodes (cf. TypifyPass)
    void typify(CharNode* node);
    //! Typify implementation for DictNodes (cf. TypifyPass)
    void typify(DictNode* node);
    //! Typify implementation for FunctionNodes (cf. TypifyPass)
    void typify(FunctionNode* node);
    //! Typify implementation for IfNodes (cf. TypifyPass)
    void typify(IfNode* node);
    //! Typify implementation for IntNodes (cf. TypifyPass)
    void typify(IntNode* node);
    //! Typify implementation for KeyargNodes (cf. TypifyPass)
    void typify(KeyargNode* node);
    //! Typify implementation for KeywordNodes (cf. TypifyPass)
    void typify(KeywordNode* node);
    //! Typify implementation for MatchNodes (cf. TypifyPass)
    void typify(MatchNode* node);
    //! Typify implementation for NegateNodes (cf. TypifyPass)
    void typify(NegateNode* node);
    //! Typify implementation for OnNodes (cf. TypifyPass)
    void typify(OnNode* node);
    //! Typify implementation for RangeNodes (cf. TypifyPass)
    void typify(RangeNode* node);
    //! Typify implementation for RationalNodes (cf. TypifyPass)
    void typify(RationalNode* node);
    //! Typify implementation for RealNodes (cf. TypifyPass)
    void typify(RealNode* node);
    //! Typify implementation for SelectNodes (cf. TypifyPass)
    void typify(SelectNode* node);
    //! Typify implementation for SlotdefNodes (cf. TypifyPass)
    void typify(SlotdefNode* node);
    //! Typify implementation for StringNodes (cf. TypifyPass)
    void typify(StringNode* node);
    //! Typify implementation for TypeDefNodes (cf. TypifyPass)
    void typify(TypeDefNode* node);
    //! Typify implementation for TypeNodes (cf. TypifyPass)
    void typify(TypeNode* node);
    //! Typify implementation for UnitConstNodes (cf. TypifyPass)
    void typify(UnitConstNode* node);
    //! Typify implementation for VectorNodes (cf. TypifyPass)
    void typify(VectorNode* node);
    //! Typify implementation for WhileNodes (cf. TypifyPass)
    void typify(WhileNode* node);
    //! Typify implementation for VardefNodes (cf. TypifyPass)
    void typify(VardefNode* node);
    //! Typify implementation for FuncDefNodes (cf. TypifyPass)
    void typify(FuncDefNode* node);
    //! Typify implementation for UndefNodes (cf. TypifyPass)
    void typify(UndefNode* node);

  private:
    void annotateTypeConv(AptNode* toNode, const Type& type);
    void enforceAtomTypeConv(AptNode* node, const Type& dstType);
    void setBodyLastDstType(AptNode* body, const Type& dstType);
    bool isNodeCallToGenericFunction(const AptNode* node) const;

    //! Typify \p node
    void typifyNode(AptNode* node);

    //! Typify each node in \p nl sequentially
    void typifyNodeList(NodeList& nl);

    //! Compute the function signature type for a FunctionNode \p node
    //! including return type and parameter types.
    void setupFunctionNodeType(FunctionNode* node);

    //! Find all return branches in the body of FunctionNode \p node and check
    //! that their types match the return type of \p node.  Requires that
    //! setupFunctionNodeType() has been successfully run before.
    void checkFunctionReturnType(FunctionNode* node);

    //! Determine the proper type for a binding node \p node and set it on \p
    //! node.  \p errdesc is used as a context name to be shown in possible
    //! error messages.
    void setupBindingNodeType(BindingNode* node, const char* errdesc);

    void checkArgParamType(TypeCtx& localCtx, const ParamNode* param,
                           AptNode* arg, int idx);

    //! Sequentially determine and check types for the arguments parameters of ApplyNode
    //! \p node which is to be applied to \p funcNode.
    //!
    //! @param node       the ApplyNode to typify
    //! @param funcNode   the FunctionNode as looked up from the scope.
    void typifyMatchAndCheckParameters(ApplyNode* node,
                                       const FunctionNode* funcNode);
    //! Sequentially determine and check types for the arguments \p args
    //! \p node which is to be applied to \p funcNode.
    //!
    //! @param srcpos     the source position where the function is applied
    //! @param args       the arguments to apply to \p funcNode
    //! @param funcNode   the FunctionNode as looked up from the scope.
    //! @return the return type of the function applied
    Type typifyMatchAndCheckParameters(const SrcPos& srcpos,
                                       const NodeList& args,
                                       const FunctionNode* funcNode);

    //! lookup up a matching function implementing the operator of the binary
    //! node \p node and determine and set the types if appling the arguments
    //! \p leftArg and \p rightArg to it.
    //!
    //! @return true if such a function can be found and all types apply
    //!         properly.
    bool checkBinaryFunctionCall(BinaryNode* node,
                                 const String& funcName,
                                 AptNode* leftArg, AptNode* rightArg);
    //! returns the builtin function name for the operator \p type.
    String operatorNameByOp(OperatorType type) const;

    //! Helper class for the findKeyedArg() function.
    struct KeyargReturn
    {
      const KeyargNode* fKeyarg;
      int fIdx;
    };

    //! When determining function call types, this finds the matching argument
    //! for a keyed parameter named \p key from \p args starting at \p
    //! argidx.  If no matching argument can be found the returned structure
    //! has the value ''{ NULL, 0 }'';
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
    virtual AptNode* doApply(AptNode* src);
  };
};                              // namespace

#endif                          // bootstrap_typify_h
