/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_typify_h
#define bootstrap_typify_h

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
    void typify(ArraySymbolNode* node);
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
    void typify(ThenWhileNode* node);
    void typify(TypeDefNode* node);
    void typify(UnitConstNode* node);
    void typify(VectorNode* node);
    void typify(WhileNode* node);
    void typify(VardefNode* node);
    void typify(FuncDefNode* node);

  private:
    void typifyNodeList(NodeList& nl);

    //-------- data members
    Phase fPhase;
  };
};                              // namespace

#endif                          // bootstrap_typify_h