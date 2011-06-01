/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef bootstrap_codegen_init_h
#define bootstrap_codegen_init_h

#include "llvm/Support/IRBuilder.h"

#include "str.h"
#include "refcountable.h"
#include "codegen-proxy.h"


namespace llvm
{
  class Constant;
  class Function;
  class FunctionType;
  class LLVMContext;
  class Module;
  class Value;
};

namespace herschel
{
  class FuncDefNode;
  class CodeGenerator;
  class VardefNode;
  class TypeDefNode;
  class Type;
  class MethodImpl;

  class CodegenTypeUtils;
  class CodegenTools;

  //----------------------------------------------------------------------------

  class ModuleRuntimeInitializer : public RefCountable,
                                   public CodeGeneratorProxy
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

    llvm::Value* makeTypeOrCallRegistration(const Type& ty) const;

    llvm::Value* makeGenericFunctionRegistration(const FuncDefNode* node) const;
    llvm::Value* makeGenericFuncRegisterCall(llvm::Value* newType) const;
    llvm::Value* makeGenericFuncAllocCall(const FuncDefNode* node) const;
    llvm::Value* makeGetGenericFuncLookupCall(const FuncDefNode* node) const;

    void makeMethodRegisterCall(const MethodImpl& impl) const;

    llvm::Value* registerKeyword(const String& keyword);

  private:
    friend class ClassInitStrategy;
    friend class MethodInitStrategy;

    typedef std::vector<std::pair<llvm::Constant*, int> > CtorList;

    void emitModuleInitFunction();

    void emitRuntimeInitFunc();
    void emitKeywordInitFunc();
    void emitGlobalVarInitFunc();

    void emitCtorList(const CtorList &fns, const char *globalName);

    llvm::Function* createGlobalInitOrDtorFunction(const llvm::FunctionType *ft,
                                                   const String& name);

    llvm::Value* makeTypeRegisterCall(llvm::Value* newType) const;
    llvm::Value* makeClassAllocCall(const Type& ty) const;
    llvm::Value* makeTypeAllocCall(const Type& ty) const;
    llvm::Value* makeGetTypeLookupCall(const Type& ty) const;


    template<typename NodeT, typename StrategyT>
    void emitEntityInitFunc(const std::vector<NodeT>& entities,
                            StrategyT strategy);
    template<typename NodeT, typename StrategyT>
    void emitEntityGetterFunc(const std::vector<NodeT>& entities,
                              StrategyT strategy);

    struct SlotAndClassSpecs
    {
      llvm::Constant* fTypeSlotSpecs;
      size_t          fInstanceSize;
    };
    SlotAndClassSpecs computeTypeSlotAndClassSpecs(const Type& ty) const;

    struct OrderedTypeSlots
    {
      std::vector<String> fSlotNames;
      std::vector<size_t> fSlotOffsets;
      size_t              fTotalSize;
    };
    OrderedTypeSlots orderTypeSlots(const TypeSlotList& typeSlots) const;

    llvm::Constant* createTypeSlotSpec(const String& slotName,
                                       size_t slotOffset) const;

    //-------- data members

    CtorList fGlobalCtors;
    CtorList fGlobalDtors;

    std::vector<const TypeDefNode*> fClassInitFuncs;
    std::vector<const VardefNode*>  fGlobalInitVars;
    std::vector<const FuncDefNode*> fGenericsInitFuncs;
    std::vector<MethodImpl>         fMethodInitFuncs;

    typedef std::map<String, llvm::GlobalVariable*> KeywordMap;
    KeywordMap fKeywords;
  };


};                              // namespace

#endif                          // bootstrap_codegen_init_h
