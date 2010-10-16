/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_annotate_h
#define bootstrap_annotate_h

#include "refcountable.h"

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
  class TypeNode;
  class UnitConstant;
  class VardefNode;
  class VectorNode;
  class WhileNode;

  class Scope;

  //------------------------------------------------------------------------------

  class Annotator : public RefCountable
  {
  public:
    Annotator();

    void annotateNode(AptNode* node);

    void annotate(CompileUnitNode* node);

    void annotate(SymbolNode* node);

    void annotate(DefNode* node);
    void annotate(LetNode* node);
    void annotate(BlockNode* node);

    void annotate(ParamNode* node);

    void annotate(ApplyNode* node);
    void annotate(ArrayNode* node);
    void annotate(ArraySymbolNode* node);
    void annotate(AssignNode* node);
    void annotate(BinaryNode* node);
    void annotate(BoolNode* node);
    void annotate(CharNode* node);
    void annotate(DictNode* node);
    void annotate(FunctionNode* node);
    void annotate(IfNode* node);
    void annotate(IntNode* node);
    void annotate(KeyargNode* node);
    void annotate(KeywordNode* node);
    void annotate(MatchNode* node);
    void annotate(NegateNode* node);
    void annotate(OnNode* node);
    void annotate(RangeNode* node);
    void annotate(RationalNode* node);
    void annotate(RealNode* node);
    void annotate(SelectNode* node);
    void annotate(SlotdefNode* node);
    void annotate(StringNode* node);
    void annotate(ThenWhileNode* node);
    void annotate(TypeNode* node);
    void annotate(UnitConstant* node);
    void annotate(VectorNode* node);
    void annotate(WhileNode* node);

  private:
    void annotate(VardefNode* node, bool isLocal);
    void annotate(FuncDefNode* node, bool isLocal);

    void takeFullNameFromNode(SymbolNode* node, const AptNode* otherNode);
  };

};                              // namespace

#endif                          // bootstrap_annotate_h
