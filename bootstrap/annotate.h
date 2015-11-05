/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include "compilepass.h"

#include <vector>
#include <memory>


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

  using NodeList = std::vector<std::shared_ptr<AptNode>>;

  //------------------------------------------------------------------------------

  //! Defines the "annotate" pass over the abstract parse tree.
  //!
  //! Currently the annotate pass detects shared variable access (which is
  //! required for closure detection).
  class Annotator
  {
  public:
    enum Phase
    {
      kRegister,
      kLookup
    };

    Annotator(std::shared_ptr<Scope> scope, Compiler& compiler);

    void annotateRecursively(std::shared_ptr<AptNode> node);

    void annotateNode(std::shared_ptr<AptNode> node);

    void annotate(std::shared_ptr<CompileUnitNode> node);

    void annotate(std::shared_ptr<SymbolNode> node);

    void annotate(std::shared_ptr<DefNode> node);
    void annotate(std::shared_ptr<LetNode> node);
    void annotate(std::shared_ptr<BlockNode> node);

    void annotate(std::shared_ptr<ParamNode> node);

    void annotate(std::shared_ptr<ApplyNode> node);
    void annotate(std::shared_ptr<ArrayNode> node);
    void annotate(std::shared_ptr<ArrayTypeNode> node);
    void annotate(std::shared_ptr<AssignNode> node);
    void annotate(std::shared_ptr<BinaryNode> node);
    void annotate(std::shared_ptr<BoolNode> node);
    void annotate(std::shared_ptr<CastNode> node);
    void annotate(std::shared_ptr<CharNode> node);
    void annotate(std::shared_ptr<DictNode> node);
    void annotate(std::shared_ptr<FunctionNode> node);
    void annotate(std::shared_ptr<IfNode> node);
    void annotate(std::shared_ptr<IntNode> node);
    void annotate(std::shared_ptr<KeyargNode> node);
    void annotate(std::shared_ptr<KeywordNode> node);
    void annotate(std::shared_ptr<MatchNode> node);
    void annotate(std::shared_ptr<UnaryNode> node);
    void annotate(std::shared_ptr<OnNode> node);
    void annotate(std::shared_ptr<RangeNode> node);
    void annotate(std::shared_ptr<RationalNode> node);
    void annotate(std::shared_ptr<RealNode> node);
    void annotate(std::shared_ptr<SelectNode> node);
    void annotate(std::shared_ptr<SlotdefNode> node);
    void annotate(std::shared_ptr<SlotRefNode> node);
    void annotate(std::shared_ptr<StringNode> node);
    void annotate(std::shared_ptr<TypeDefNode> node);
    void annotate(std::shared_ptr<TypeNode> node);
    void annotate(std::shared_ptr<UnitConstNode> node);
    void annotate(std::shared_ptr<VectorNode> node);
    void annotate(std::shared_ptr<WhileNode> node);
    void annotate(std::shared_ptr<UndefNode> node);

  private:
    void annotateNodeList(NodeList& nl, bool marktailpos, bool marksingletype);

    void annotate(std::shared_ptr<VardefNode> node, bool isLocal);
    void annotate(std::shared_ptr<FuncDefNode> node, bool isLocal);

    void takeFullNameFromNode(SymbolNode* node, const AptNode* otherNode);
    bool updateAllocType(SymbolNode* usingNode, const AptNode* referedNode);


    //-------- data members

    std::shared_ptr<Scope> fScope;
    Phase      fPhase;
    Compiler&  fCompiler;       // backlink to owning compiler
  };


  //--------------------------------------------------------------------------

  //! \c TokenCompilePass wrapper for the \c Annotator pass to be used in the
  //! process pipeline as fourth pass.

  class AnnotatePass : public AptNodeCompilePass
  {
  public:
    AnnotatePass(int level, std::shared_ptr<Scope> scope, Compiler& compiler);
    virtual std::shared_ptr<AptNode> doApply(std::shared_ptr<AptNode> src);

  private:
    std::shared_ptr<Scope> fScope;
    Compiler&  fCompiler;       // backlink to owning compiler
  };

} // namespace
