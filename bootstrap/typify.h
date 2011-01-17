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


  typedef std::vector<Ptr<AptNode> > NodeList;

  //------------------------------------------------------------------------------

  class Typifier : public RefCountable
  {
  public:
    enum Phase
    {
      kTypify,
      kCheck
    };

    Typifier();

    void typifyRecursively(AptNode* node);

    void typifyNode(AptNode* node);

    void typify(CompileUnitNode* node);

    void typify(SymbolNode* node);

    void typify(DefNode* node);
    void typify(LetNode* node);
    void typify(BlockNode* node);

    void typify(ParamNode* node);

    void typify(ApplyNode* node);
    void typify(ArrayNode* node);
    void typify(ArrayTypeNode* node);
    void typify(AssignNode* node);
    void typify(BinaryNode* node);
    void typify(BoolNode* node);
    void typify(CastNode* node);
    void typify(CharNode* node);
    void typify(DictNode* node);
    void typify(FunctionNode* node);
    void typify(IfNode* node);
    void typify(IntNode* node);
    void typify(KeyargNode* node);
    void typify(KeywordNode* node);
    void typify(MatchNode* node);
    void typify(NegateNode* node);
    void typify(OnNode* node);
    void typify(RangeNode* node);
    void typify(RationalNode* node);
    void typify(RealNode* node);
    void typify(SelectNode* node);
    void typify(SlotdefNode* node);
    void typify(StringNode* node);
    void typify(TypeDefNode* node);
    void typify(TypeNode* node);
    void typify(UnitConstNode* node);
    void typify(VectorNode* node);
    void typify(WhileNode* node);
    void typify(VardefNode* node);
    void typify(FuncDefNode* node);

  private:
    void typifyNodeList(NodeList& nl);

    void setupFunctionNodeType(FunctionNode* node);
    void checkFunctionReturnType(FunctionNode* node);

    void setupBindingNodeType(BindingNode* node, const char* errdesc);

    void checkArgParamType(TypeCtx& localCtx, const ParamNode* param,
                           AptNode* arg, int idx);
    void typifyMatchAndCheckParameters(ApplyNode* node,
                                       const FunctionNode* funcNode,
                                       const NodeList& funcParams);

    struct KeyargReturn
    {
      const KeyargNode* fKeyarg;
      int fIdx;
    };

    KeyargReturn findKeyedArg(const NodeList& args, size_t argidx,
                              const String& key);

    //-------- data members
    Phase fPhase;
  };


  //--------------------------------------------------------------------------

  class TypifyPass : public AptNodeCompilePass
  {
  public:
    TypifyPass(int level);
    virtual AptNode* doApply(AptNode* src);
  };
};                              // namespace

#endif                          // bootstrap_typify_h
