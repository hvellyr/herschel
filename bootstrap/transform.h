/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_transform_h
#define bootstrap_transform_h

#include "refcountable.h"
#include "ptr.h"

#include <vector>

namespace heather
{
  class ApplyNode;
  class AptNode;
  class ArrayNode;
  class ArraySymbolNode;
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
  class ThenWhileNode;
  class TypeDefNode;
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

    void transformNode(AptNode* node);

    void transform(CompileUnitNode* node);

    void transform(SymbolNode* node);

    void transform(DefNode* node);
    void transform(LetNode* node);
    void transform(BlockNode* node);

    void transform(ParamNode* node);

    void transform(ApplyNode* node);
    void transform(ArrayNode* node);
    void transform(ArraySymbolNode* node);
    void transform(AssignNode* node);
    void transform(BinaryNode* node);
    void transform(BoolNode* node);
    void transform(CastNode* node);
    void transform(CharNode* node);
    void transform(DictNode* node);
    void transform(FunctionNode* node);
    void transform(IfNode* node);
    void transform(IntNode* node);
    void transform(KeyargNode* node);
    void transform(KeywordNode* node);
    void transform(MatchNode* node);
    void transform(NegateNode* node);
    void transform(OnNode* node);
    void transform(RangeNode* node);
    void transform(RationalNode* node);
    void transform(RealNode* node);
    void transform(SelectNode* node);
    void transform(SlotdefNode* node);
    void transform(StringNode* node);
    void transform(ThenWhileNode* node);
    void transform(TypeDefNode* node);
    void transform(UnitConstNode* node);
    void transform(VectorNode* node);
    void transform(WhileNode* node);
    void transform(VardefNode* node);
    void transform(FuncDefNode* node);

  private:
    void transformNodeList(NodeList& nl);

    int findBlockSplitIndex(const NodeList& nodes);
  };

};                              // namespace

#endif                          // bootstrap_transform_h
