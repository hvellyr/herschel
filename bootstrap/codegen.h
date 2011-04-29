/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_codegen_h
#define bootstrap_codegen_h

#include "refcountable.h"
#include "ptr.h"
#include "apt.h"

#include <map>
#include <vector>
#include <string>

#include "llvm/Support/IRBuilder.h"

namespace llvm
{
  class AllocaInst;
  class BasicBlock;
  class Function;
  class FunctionType;
  class FunctionPassManager;
  class LLVMContext;
  class Module;
  class Value;
  // class DIBuilder;
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
  class TypeDefNode;
  class TypeNode;
  class Type;
  class UnitConstNode;
  class VardefNode;
  class VectorNode;
  class UndefNode;
  class WhileNode;

  class CodegenTypeUtils;

  class String;

  typedef std::vector<Ptr<AptNode> > NodeList;


  //----------------------------------------------------------------------------

  class ModuleRuntimeInitializer
  {
  public:
    struct MethodImpl
    {
      const FuncDefNode* fNode;
      String fMethodImplName;

      MethodImpl(const FuncDefNode* node, const String& methImplNm)
        : fNode(node),
          fMethodImplName(methImplNm)
      {}
    };


    ModuleRuntimeInitializer(CodeGenerator* generator);

    void finish();

    void addGlobalCtor(llvm::Function* ctor, int priority);
    void addGlobalDtor(llvm::Function* dtor, int priority);

    void addGlobalVariable(const VardefNode* vardefNode);
    void addTypeDef(const TypeDefNode* typedefNode);
    void addGenericFunctionDef(const FuncDefNode* node);
    void addMethodDef(const FuncDefNode* node,
                      const String& methodImplName);

    CodeGenerator* generator() const;
    llvm::LLVMContext& context() const;
    llvm::IRBuilder<>& builder() const;
    llvm::Module* module() const;
    CodegenTypeUtils& types();
    const CodegenTypeUtils& types() const;

    llvm::Value* makeTypeOrCallRegistration(const Type& ty) const;

    llvm::Value* makeGenericFunctionRegistration(const FuncDefNode* node) const;
    llvm::Value* makeGenericFuncRegisterCall(llvm::Value* newType) const;
    llvm::Value* makeGenericFuncAllocCall(const FuncDefNode* node) const;
    llvm::Value* makeGetGenericFuncLookupCall(const FuncDefNode* node) const;

    void makeMethodRegisterCall(const MethodImpl& impl) const;

  private:
    friend class ClassInitStrategy;
    friend class MethodInitStrategy;

    typedef std::vector<std::pair<llvm::Constant*, int> > CtorList;

    void emitRuntimeInitFunc();
    void emitClassInitFunc();
    void emitGlobalVarInitFunc();
    void emitGenericsInitFunc();
    void emitMethodInitFunc();

    void emitCtorList(const CtorList &fns, const char *globalName);

    llvm::Function* createGlobalInitOrDtorFunction(const llvm::FunctionType *ft,
                                                   const String& name);

    llvm::Value* makeTypeRegisterCall(llvm::Value* newType) const;
    llvm::Value* makeClassAllocCall(const Type& ty) const;
    llvm::Value* makeTypeAllocCall(const Type& ty) const;
    llvm::Value* makeGetTypeLookupCall(const Type& ty) const;


    template<typename NodeT, typename StrategyT>
    void emitEntityInitFunc(const char* suggestedTmpName,
                            const std::vector<NodeT>& entities,
                            StrategyT strategy,
                            int priority);

    //-------- data members

    Ptr<CodeGenerator> fGenerator; 

    CtorList fGlobalCtors;
    CtorList fGlobalDtors;

    std::vector<const TypeDefNode*> fClassInitFuncs;
    std::vector<const VardefNode*>  fGlobalInitVars;
    std::vector<const FuncDefNode*> fGenericsInitFuncs;
    std::vector<MethodImpl>         fMethodInitFuncs;
  };


  //----------------------------------------------------------------------------

  class CodegenTypeUtils
  {
  public:
    CodegenTypeUtils(CodeGenerator* generator);

    const llvm::Type* getAtomType() const;
    const llvm::Type* getTypeType() const;
    const llvm::Type* getTypeSlotPairType() const;
    const llvm::Type* getGenericFuncType() const;
    const llvm::Type* getMethodType() const;
    const llvm::Type* getType(const Type& type) const;

  private:
    llvm::LLVMContext& context() const;
    llvm::IRBuilder<>& builder() const;
    llvm::Module* module() const;

    //-------- data members

    Ptr<CodeGenerator> fGenerator; 
  };


  //----------------------------------------------------------------------------

  //! The code generation pass.
  class CodeGenerator : public RefCountable
  {
  public:
    CodeGenerator();
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

  private:
    friend class ModuleRuntimeInitializer;
    friend class CodegenTypeUtils;

    llvm::Value* codegen(const FuncDefNode* node, bool isLocal);
    llvm::Value* codegen(const VardefNode* node, bool isLocal);

    llvm::FunctionType* createFunctionSignature(const FunctionNode* node,
                                                bool inlineRetv,
                                                const Type& retty,
                                                bool isGeneric);

    llvm::Value* codegen(const NodeList& nl);

    void createDefaultCMainFunc();

    llvm::Value* codegenForGlobalVars(const VardefNode* node);

    llvm::AllocaInst* createEntryBlockAlloca(llvm::Function *func,
                                             const String& name,
                                             const llvm::Type* type);

    llvm::Value* makeTypeCastAtomToPlain(llvm::Value* val, const Type& dstType);

    enum Typeid {
      kAtomInt  = 0,
      kAtomBool = 1,
      kAtomChar = 2,
    };

    void setAtom(llvm::AllocaInst* atom, Typeid typid, llvm::Value* value);

    void assignAtom(llvm::Value* src, llvm::Value* dst);

    llvm::Function* getIntrinsic(unsigned int iid,
                 const llvm::Type** tys, unsigned int numTys);
    llvm::Function* getMemCpyFn(const llvm::Type* dstType,
                                const llvm::Type* srcType,
                                const llvm::Type* sizeType);

    llvm::Value* wrapLoad(llvm::Value* val);

    llvm::Value* makeIntAtom(int val);
    llvm::Value* makeIntAtom(llvm::Value* val);
    llvm::Value* makeBoolAtom(llvm::Value* val);
    llvm::Value* makeBoolAtom(bool val);

    //------------------------------ functions

    llvm::Value* compileGenericFunctionDef(const FuncDefNode* node);
    llvm::Value* compileMethodDef(const FuncDefNode* node);
    llvm::Value* compileAbstractFuncDef(const FuncDefNode* node);
    llvm::Value* compileNormalFuncDef(const FuncDefNode* node, bool isLocal);
    String makeFunctionName(const FuncDefNode* node,
                            const String& methodNameSuffix) const;

    struct FuncPair
    {
      llvm::FunctionType* fType;
      llvm::Function* fFunc;
      Type fRetType;
    };
    FuncPair createFunction(const FuncDefNode* node,
                            const String& methodNameSuffix,
                            bool isGeneric);
    llvm::Value* compileNormalFuncDefImpl(const FuncPair& func,
                                          const FuncDefNode* node, bool isLocal);

    //------------------------------ emit operators

    bool isPlainInt(const Type& type) const;

    llvm::Value* wrapInt(llvm::Value* value, const Type& type);
    llvm::Value* wrapBool(llvm::Value* value, const Type& type);

    llvm::Value* coerceIntOperand(const Type& dstType, const Type& isType,
                                  llvm::Value* value);

    llvm::Value* codegenOpIntInt(const BinaryNode* node,
                                 llvm::Value* left,
                                 llvm::Value* right);

    llvm::Value* emitPackCode(const Type& dstType, TypeConvKind convKind,
                              llvm::Value* value,
                              const Type& valType);

    llvm::Value* makeGetTypeLookupCall(const Type& ty) const;
    llvm::Value* makeGetGenericFuncLookupCall(const FuncDefNode* node) const;

    //------------------------------ allocation

    llvm::Value* emitAllocateApply(const ApplyNode* node);
    llvm::Value* emitTypeNameForAllocate(const AptNode* node);

    //-------- data members

    llvm::LLVMContext& fContext;
    llvm::Module*     fModule;
    // llvm::DIBuilder*  fDIBuilder;
    llvm::IRBuilder<> fBuilder;
    llvm::FunctionPassManager* fOptPassManager;

    ModuleRuntimeInitializer fInitializer;
    CodegenTypeUtils fTypes;

    bool fHasMainFunc;

    // takes llvm::Value or llvm::GlobalVariable
    std::map<String, llvm::AllocaInst*> fNamedValues;
    std::map<String, llvm::GlobalVariable*> fGlobalVariables;
  };
};                              // namespace

#endif                          // bootstrap_codegen_h
