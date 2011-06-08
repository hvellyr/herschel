/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef bootstrap_codegen_h
#define bootstrap_codegen_h

#include "llvm/Support/IRBuilder.h"

#include "refcountable.h"
#include "ptr.h"
#include "apt.h"

#include <map>
#include <vector>
#include <string>

namespace llvm
{
  class AllocaInst;
  class BasicBlock;
  class Function;
  class FunctionPassManager;
  class FunctionType;
  class LLVMContext;
  class Module;
  class TargetData;
  class Value;
};


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
  class StringNode;
  class SymbolNode;
  class Type;
  class TypeDefNode;
  class TypeNode;
  class UndefNode;
  class UnitConstNode;
  class VardefNode;
  class VectorNode;
  class WhileNode;

  class CodegenTypeUtils;
  class CodegenTools;
  class ModuleRuntimeInitializer;
  class CodegenBinaryNode;
  class CodegenFuncDef;
  class CodeGeneratorProxy;

  class String;

  typedef std::vector<Ptr<AptNode> > NodeList;


  //----------------------------------------------------------------------------

  //! The code generation pass.
  class CodeGenerator : public RefCountable
  {
  public:
    CodeGenerator(Compiler* compiler);
    ~CodeGenerator();

    bool compileToCode(const CompileUnitNode* node, const String& outputFile);

    llvm::Value* codegenNode(const AptNode* node, bool autoloadAllocInst = false);

    llvm::Value* codegen(const ApplyNode* node);
    llvm::Value* codegen(const ArrayNode* node);
    llvm::Value* codegen(const ArrayTypeNode* node);
    llvm::Value* codegen(const AssignNode* node);
    llvm::Value* codegen(const BinaryNode* node);
    llvm::Value* codegen(const BlockNode* node);
    llvm::Value* codegen(const BoolNode* node);
    llvm::Value* codegen(const CastNode* node);
    llvm::Value* codegen(const CharNode* node);
    llvm::Value* codegen(const CompileUnitNode* node);
    llvm::Value* codegen(const DefNode* node);
    llvm::Value* codegen(const DictNode* node);
    llvm::Value* codegen(const FunctionNode* node);
    llvm::Value* codegen(const IfNode* node);
    llvm::Value* codegen(const IntNode* node);
    llvm::Value* codegen(const KeyargNode* node);
    llvm::Value* codegen(const KeywordNode* node);
    llvm::Value* codegen(const LetNode* node);
    llvm::Value* codegen(const MatchNode* node);
    llvm::Value* codegen(const UnaryNode* node);
    llvm::Value* codegen(const OnNode* node);
    llvm::Value* codegen(const ParamNode* node);
    llvm::Value* codegen(const RangeNode* node);
    llvm::Value* codegen(const RationalNode* node);
    llvm::Value* codegen(const RealNode* node);
    llvm::Value* codegen(const SelectNode* node);
    llvm::Value* codegen(const SlotdefNode* node);
    llvm::Value* codegen(const StringNode* node);
    llvm::Value* codegen(const SymbolNode* node);
    llvm::Value* codegen(const TypeDefNode* node);
    llvm::Value* codegen(const TypeNode* node);
    llvm::Value* codegen(const UnitConstNode* node);
    llvm::Value* codegen(const VectorNode* node);
    llvm::Value* codegen(const WhileNode* node);
    llvm::Value* codegen(const UndefNode* node);

    llvm::LLVMContext& context() const;
    llvm::IRBuilder<>& builder() const;
    llvm::Module* module() const;
    llvm::FunctionPassManager* optPassManager() const;

    bool is64Bit() const;
    llvm::TargetData* targetData() const;

  private:
    friend class ModuleRuntimeInitializer;
    friend class CodegenTypeUtils;
    friend class CodegenTools;
    friend class CodegenBinaryNode;
    friend class CodegenFuncDef;
    friend class CodeGeneratorProxy;
    friend class CodegenApply;
    friend class CodegenVardef;

    void setupOptPassManager();

    llvm::Value* codegen(const FuncDefNode* node, bool isLocal);
    llvm::Value* codegen(const VardefNode* node, bool isLocal);

    llvm::Value* codegen(const NodeList& nl);

    void createDefaultCMainFunc();


    //------------------------------ emit operators

    llvm::Value* makeGetTypeLookupCall(const Type& ty) const;
    llvm::Value* makeGetGenericFuncLookupCall(const FuncDefNode* node) const;

    //-------- data members

    Compiler*                  fCompiler;
    llvm::LLVMContext&         fContext;
    llvm::Module*              fModule;
    // llvm::DIBuilder*        fDIBuilder;
    llvm::IRBuilder<>          fBuilder;
    llvm::FunctionPassManager* fOptPassManager;
    llvm::TargetData*          fTargetData;

    Ptr<ModuleRuntimeInitializer> fInitializer;
    Ptr<CodegenTypeUtils>      fTypes;
    Ptr<CodegenTools>          fTools;

    bool fHasMainFunc;

    // takes llvm::Value or llvm::GlobalVariable
    std::map<String, llvm::AllocaInst*> fNamedValues;
    std::map<String, llvm::GlobalVariable*> fGlobalVariables;
  };
};                              // namespace

#endif                          // bootstrap_codegen_h
