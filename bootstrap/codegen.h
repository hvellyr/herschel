/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef bootstrap_codegen_h
#define bootstrap_codegen_h

#include "llvm/IR/IRBuilder.h"

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
//  template <typename T> class PassManager;
//  typedef PassManager<Function> FunctionPassManager;
  class FunctionType;
  class LLVMContext;
  class Module;
  class DataLayout;
  class Value;

  namespace legacy
  {
    class FunctionPassManager;
  }
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
    llvm::Value* codegen(const SlotRefNode* node);
    llvm::Value* codegen(const StringNode* node);
    llvm::Value* codegen(const SymbolNode* node);
    llvm::Value* codegen(const TypeDefNode* node);
    llvm::Value* codegen(const TypeNode* node);
    llvm::Value* codegen(const UnitConstNode* node);
    llvm::Value* codegen(const VectorNode* node);
    llvm::Value* codegen(const WhileNode* node);
    llvm::Value* codegen(const UndefNode* node);

    //! Returns the current llvm context
    llvm::LLVMContext& context() const;
    //! Returns the current llvm IRBuilder
    llvm::IRBuilder<>& builder() const;
    //! Returns the current llvm module
    llvm::Module* module() const;
    //! Returns the current llvm pass manager for optimizing functions
    llvm::legacy::FunctionPassManager* optPassManager() const;

    //! Indicates whether we compile for a 64bit architecture.
    bool is64Bit() const;
    //! Returns the current llvm target data layout describing the target
    //! architecture of the current build
    const llvm::DataLayout* dataLayout() const;

  private:
    friend class CodeGeneratorProxy;
    friend class CodegenApply;
    friend class CodegenBinaryNode;
    friend class CodegenFuncDef;
    friend class CodegenSlot;
    friend class CodegenTools;
    friend class CodegenTypeUtils;
    friend class CodegenVardef;
    friend class ModuleRuntimeInitializer;

    //! Set up the llvm pass manager for optimizing code
    void setupOptPassManager();

    llvm::Value* codegen(const FuncDefNode* node, bool isLocal);
    llvm::Value* codegen(const VardefNode* node, bool isLocal);

    llvm::Value* codegen(const NodeList& nl);

    //! Create the C main entrance function
    void createDefaultCMainFunc();


    //------------------------------ emit operators

    llvm::Value* makeGetTypeLookupCall(const Type& ty) const;
    llvm::Value* makeGetGenericFuncLookupCall(const FuncDefNode* node) const;

    llvm::Value* makeGetBaseTypeLookupCall(const Type& ty) const;
    llvm::Value* makeGetArrayTypeLookupCall(const Type& ty) const;

    //-------- data members

    Compiler*                  fCompiler;
    llvm::LLVMContext&         fContext;
    llvm::Module*              fModule;
    // llvm::DIBuilder*        fDIBuilder;
    llvm::IRBuilder<>          fBuilder;
    llvm::legacy::FunctionPassManager* fOptPassManager;
    const llvm::DataLayout*    fDataLayout;

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
