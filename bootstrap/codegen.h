/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_codegen_h
#define bootstrap_codegen_h

#include "refcountable.h"

#include "llvm/Support/IRBuilder.h"

namespace llvm
{
  class Value;
  class Module;
};


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


  //----------------------------------------------------------------------------

  class CodeGenerator : public RefCountable
  {
  public:
    CodeGenerator();
    ~CodeGenerator();

    void generateCode(AptNode* node);

    llvm::Value* codegenNode(AptNode* node);

    llvm::Value* codegen(const ApplyNode* node);
    llvm::Value* codegen(const ArrayNode* node);
    llvm::Value* codegen(const ArraySymbolNode* node);
    llvm::Value* codegen(const AssignNode* node);
    llvm::Value* codegen(const BinaryNode* node);
    llvm::Value* codegen(const BlockNode* node);
    llvm::Value* codegen(const BoolNode* node);
    llvm::Value* codegen(const CharNode* node);
    llvm::Value* codegen(const CompileUnitNode* node);
    llvm::Value* codegen(const DefNode* node);
    llvm::Value* codegen(const DictNode* node);
    llvm::Value* codegen(const FuncDefNode* node);
    llvm::Value* codegen(const FunctionNode* node);
    llvm::Value* codegen(const IfNode* node);
    llvm::Value* codegen(const IntNode* node);
    llvm::Value* codegen(const KeyargNode* node);
    llvm::Value* codegen(const KeywordNode* node);
    llvm::Value* codegen(const LetNode* node);
    llvm::Value* codegen(const MatchNode* node);
    llvm::Value* codegen(const NegateNode* node);
    llvm::Value* codegen(const OnNode* node);
    llvm::Value* codegen(const ParamNode* node);
    llvm::Value* codegen(const RangeNode* node);
    llvm::Value* codegen(const RationalNode* node);
    llvm::Value* codegen(const RealNode* node);
    llvm::Value* codegen(const SelectNode* node);
    llvm::Value* codegen(const SlotdefNode* node);
    llvm::Value* codegen(const StringNode* node);
    llvm::Value* codegen(const SymbolNode* node);
    llvm::Value* codegen(const ThenWhileNode* node);
    llvm::Value* codegen(const TypeNode* node);
    llvm::Value* codegen(const UnitConstant* node);
    llvm::Value* codegen(const VardefNode* node);
    llvm::Value* codegen(const VectorNode* node);
    llvm::Value* codegen(const WhileNode* node);

  private:
    llvm::Module*     fModule;
    llvm::IRBuilder<> fBuilder;
  };
};                              // namespace

#endif                          // bootstrap_codegen_h
