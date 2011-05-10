/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef bootstrap_transform_h
#define bootstrap_transform_h

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
  class TypeNode;
  class UndefNode;
  class UnitConstNode;
  class VardefNode;
  class VectorNode;
  class WhileNode;

  class Scope;

  typedef std::vector<Ptr<AptNode> > NodeList;

  //------------------------------------------------------------------------------

  class Transformator : public RefCountable
  {
  public:
    Transformator();

    AptNode* transformNode(AptNode* node);

    AptNode* transform(CompileUnitNode* node);

    AptNode* transform(SymbolNode* node);

    AptNode* transform(DefNode* node);
    AptNode* transform(LetNode* node);
    AptNode* transform(BlockNode* node);

    AptNode* transform(ParamNode* node);

    AptNode* transform(ApplyNode* node);
    AptNode* transform(ArrayNode* node);
    AptNode* transform(ArrayTypeNode* node);
    AptNode* transform(AssignNode* node);
    AptNode* transform(BinaryNode* node);
    AptNode* transform(BoolNode* node);
    AptNode* transform(CastNode* node);
    AptNode* transform(CharNode* node);
    AptNode* transform(DictNode* node);
    AptNode* transform(FunctionNode* node);
    AptNode* transform(IfNode* node);
    AptNode* transform(IntNode* node);
    AptNode* transform(KeyargNode* node);
    AptNode* transform(KeywordNode* node);
    AptNode* transform(MatchNode* node);
    AptNode* transform(NegateNode* node);
    AptNode* transform(OnNode* node);
    AptNode* transform(RangeNode* node);
    AptNode* transform(RationalNode* node);
    AptNode* transform(RealNode* node);
    AptNode* transform(SelectNode* node);
    AptNode* transform(SlotdefNode* node);
    AptNode* transform(StringNode* node);
    AptNode* transform(TypeDefNode* node);
    AptNode* transform(TypeNode* node);
    AptNode* transform(UnitConstNode* node);
    AptNode* transform(VectorNode* node);
    AptNode* transform(WhileNode* node);
    AptNode* transform(VardefNode* node);
    AptNode* transform(FuncDefNode* node);
    AptNode* transform(UndefNode* node);

  private:
    void transformNodeList(NodeList& nl);

    int findBlockSplitIndex(const NodeList& nodes);

    void transformSingleOnExitBlock(BlockNode* node, OnNode* onnd);
  };


  //--------------------------------------------------------------------------

  class TransformPass : public AptNodeCompilePass
  {
  public:
    TransformPass(int level);
    virtual AptNode* doApply(AptNode* src);
  };
};                              // namespace

#endif                          // bootstrap_transform_h
