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
  class TypeNode;
  class UndefNode;
  class UnitConstNode;
  class VardefNode;
  class VectorNode;
  class WhileNode;

  class Scope;

  using NodeList = std::vector<std::shared_ptr<AptNode>>;

  //------------------------------------------------------------------------------

  //! Compile step which applies various transformations to an \c AptNode
  //! tree.

  class Transformator
  {
  public:
    Transformator();

    std::shared_ptr<AptNode> transformNode(std::shared_ptr<AptNode> node);

    std::shared_ptr<AptNode> transform(std::shared_ptr<CompileUnitNode> node);

    std::shared_ptr<AptNode> transform(std::shared_ptr<SymbolNode> node);

    std::shared_ptr<AptNode> transform(std::shared_ptr<DefNode> node);
    std::shared_ptr<AptNode> transform(std::shared_ptr<LetNode> node);
    std::shared_ptr<AptNode> transform(std::shared_ptr<BlockNode> node);

    std::shared_ptr<AptNode> transform(std::shared_ptr<ParamNode> node);

    std::shared_ptr<AptNode> transform(std::shared_ptr<ApplyNode> node);
    std::shared_ptr<AptNode> transform(std::shared_ptr<ArrayNode> node);
    std::shared_ptr<AptNode> transform(std::shared_ptr<ArrayTypeNode> node);
    std::shared_ptr<AptNode> transform(std::shared_ptr<AssignNode> node);
    std::shared_ptr<AptNode> transform(std::shared_ptr<BinaryNode> node);
    std::shared_ptr<AptNode> transform(std::shared_ptr<BoolNode> node);
    std::shared_ptr<AptNode> transform(std::shared_ptr<CastNode> node);
    std::shared_ptr<AptNode> transform(std::shared_ptr<CharNode> node);
    std::shared_ptr<AptNode> transform(std::shared_ptr<DictNode> node);
    std::shared_ptr<AptNode> transform(std::shared_ptr<FunctionNode> node);
    std::shared_ptr<AptNode> transform(std::shared_ptr<IfNode> node);
    std::shared_ptr<AptNode> transform(std::shared_ptr<IntNode> node);
    std::shared_ptr<AptNode> transform(std::shared_ptr<KeyargNode> node);
    std::shared_ptr<AptNode> transform(std::shared_ptr<KeywordNode> node);
    std::shared_ptr<AptNode> transform(std::shared_ptr<MatchNode> node);
    std::shared_ptr<AptNode> transform(std::shared_ptr<UnaryNode> node);
    std::shared_ptr<AptNode> transform(std::shared_ptr<OnNode> node);
    std::shared_ptr<AptNode> transform(std::shared_ptr<RangeNode> node);
    std::shared_ptr<AptNode> transform(std::shared_ptr<RationalNode> node);
    std::shared_ptr<AptNode> transform(std::shared_ptr<RealNode> node);
    std::shared_ptr<AptNode> transform(std::shared_ptr<SelectNode> node);
    std::shared_ptr<AptNode> transform(std::shared_ptr<SlotdefNode> node);
    std::shared_ptr<AptNode> transform(std::shared_ptr<SlotRefNode> node);
    std::shared_ptr<AptNode> transform(std::shared_ptr<StringNode> node);
    std::shared_ptr<AptNode> transform(std::shared_ptr<TypeDefNode> node);
    std::shared_ptr<AptNode> transform(std::shared_ptr<TypeNode> node);
    std::shared_ptr<AptNode> transform(std::shared_ptr<UnitConstNode> node);
    std::shared_ptr<AptNode> transform(std::shared_ptr<VectorNode> node);
    std::shared_ptr<AptNode> transform(std::shared_ptr<WhileNode> node);
    std::shared_ptr<AptNode> transform(std::shared_ptr<VardefNode> node);
    std::shared_ptr<AptNode> transform(std::shared_ptr<FuncDefNode> node);
    std::shared_ptr<AptNode> transform(std::shared_ptr<UndefNode> node);

  private:
    void transformNodeList(NodeList& nl);

    int findBlockSplitIndex(const NodeList& nodes);

    void transformSingleOnExitBlock(BlockNode* node, OnNode* onnd);
  };


  //--------------------------------------------------------------------------

  //! \c TokenCompilePass wrapper for the \c Transformator pass to be used in the
  //! process pipeline as third pass.

  class TransformPass : public AptNodeCompilePass
  {
  public:
    TransformPass(int level);
    std::shared_ptr<AptNode> doApply(std::shared_ptr<AptNode> src) override;
  };
};                              // namespace

