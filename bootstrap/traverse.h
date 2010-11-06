/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_traverse_h
#define bootstrap_traverse_h

#include <vector>
#include "ptr.h"
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

  typedef std::vector<Ptr<AptNode> > NodeList;


  //--------------------------------------------------------------------------

  class TraverseDelegate
  {
  public:
    virtual ~TraverseDelegate() { }

    virtual bool preApply(AptNode* node) = 0;
    virtual void postApply(AptNode* node) = 0;
  };


  //--------------------------------------------------------------------------

  class Traversator : public RefCountable
  {
  public:
    Traversator(TraverseDelegate& delegate);

    void traverseNode(AptNode* node);

    void traverse(CompileUnitNode* node);

    void traverse(SymbolNode* node);

    void traverse(DefNode* node);
    void traverse(LetNode* node);
    void traverse(BlockNode* node);

    void traverse(ParamNode* node);

    void traverse(ApplyNode* node);
    void traverse(ArrayNode* node);
    void traverse(ArraySymbolNode* node);
    void traverse(AssignNode* node);
    void traverse(BinaryNode* node);
    void traverse(BoolNode* node);
    void traverse(CastNode* node);
    void traverse(CharNode* node);
    void traverse(DictNode* node);
    void traverse(FunctionNode* node);
    void traverse(IfNode* node);
    void traverse(IntNode* node);
    void traverse(KeyargNode* node);
    void traverse(KeywordNode* node);
    void traverse(MatchNode* node);
    void traverse(NegateNode* node);
    void traverse(OnNode* node);
    void traverse(RangeNode* node);
    void traverse(RationalNode* node);
    void traverse(RealNode* node);
    void traverse(SelectNode* node);
    void traverse(SlotdefNode* node);
    void traverse(StringNode* node);
    void traverse(ThenWhileNode* node);
    void traverse(TypeDefNode* node);
    void traverse(UnitConstNode* node);
    void traverse(VectorNode* node);
    void traverse(WhileNode* node);
    void traverse(VardefNode* node);
    void traverse(FuncDefNode* node);

  private:
    void traverseNodeList(NodeList& nl);

    //-------- data members

    TraverseDelegate& fDelegate;
  };
};                              // namespace

#endif                          // bootstrap_traverse_h
