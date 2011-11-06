/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef bootstrap_annotate_h
#define bootstrap_annotate_h

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
  class Compiler;
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
  class TypeNode;
  class UndefNode;
  class UnitConstNode;
  class VardefNode;
  class VectorNode;
  class WhileNode;

  class Scope;

  typedef std::vector<Ptr<AptNode> > NodeList;

  //------------------------------------------------------------------------------

  //! Defines the "annotate" pass over the abstract parse tree.
  //!
  //! Currently the annotate pass detects shared variable access (which is
  //! required for closure detection).
  class Annotator : public RefCountable
  {
  public:
    enum Phase
    {
      kRegister,
      kLookup
    };

    Annotator(Scope* scope, Compiler* compiler);

    void annotateRecursively(AptNode* node);

    void annotateNode(AptNode* node);

    void annotate(CompileUnitNode* node);

    void annotate(SymbolNode* node);

    void annotate(DefNode* node);
    void annotate(LetNode* node);
    void annotate(BlockNode* node);

    void annotate(ParamNode* node);

    void annotate(ApplyNode* node);
    void annotate(ArrayNode* node);
    void annotate(ArrayTypeNode* node);
    void annotate(AssignNode* node);
    void annotate(BinaryNode* node);
    void annotate(BoolNode* node);
    void annotate(CastNode* node);
    void annotate(CharNode* node);
    void annotate(DictNode* node);
    void annotate(FunctionNode* node);
    void annotate(IfNode* node);
    void annotate(IntNode* node);
    void annotate(KeyargNode* node);
    void annotate(KeywordNode* node);
    void annotate(MatchNode* node);
    void annotate(UnaryNode* node);
    void annotate(OnNode* node);
    void annotate(RangeNode* node);
    void annotate(RationalNode* node);
    void annotate(RealNode* node);
    void annotate(SelectNode* node);
    void annotate(SlotdefNode* node);
    void annotate(SlotRefNode* node);
    void annotate(StringNode* node);
    void annotate(TypeDefNode* node);
    void annotate(TypeNode* node);
    void annotate(UnitConstNode* node);
    void annotate(VectorNode* node);
    void annotate(WhileNode* node);
    void annotate(UndefNode* node);

  private:
    void annotateNodeList(NodeList& nl, bool marktailpos, bool marksingletype);

    void annotate(VardefNode* node, bool isLocal);
    void annotate(FuncDefNode* node, bool isLocal);

    void takeFullNameFromNode(SymbolNode* node, const AptNode* otherNode);
    bool updateAllocType(SymbolNode* usingNode, const AptNode* referedNode);


    //-------- data members

    Ptr<Scope> fScope;
    Phase      fPhase;
    Compiler*  fCompiler;       // backlink to owning compiler
  };


  //--------------------------------------------------------------------------

  //! \c TokenCompilePass wrapper for the \c Annotator pass to be used in the
  //! process pipeline as fourth pass.

  class AnnotatePass : public AptNodeCompilePass
  {
  public:
    AnnotatePass(int level, Scope* scope, Compiler* compiler);
    virtual AptNode* doApply(AptNode* src);

  private:
    Ptr<Scope> fScope;
    Compiler*  fCompiler;       // backlink to owning compiler
  };

};                              // namespace

#endif                          // bootstrap_annotate_h
