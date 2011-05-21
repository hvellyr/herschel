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

  class ModuleRuntimeInitializer : public RefCountable
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
    CodegenTypeUtils* types();
    const CodegenTypeUtils* types() const;
    CodegenTools* tools() const;

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

    void emitModuleInitFunction();

    void emitRuntimeInitFunc();
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

    //-------- data members

    Ptr<CodeGenerator> fGenerator; 

    CtorList fGlobalCtors;
    CtorList fGlobalDtors;

    std::vector<const TypeDefNode*> fClassInitFuncs;
    std::vector<const VardefNode*>  fGlobalInitVars;
    std::vector<const FuncDefNode*> fGenericsInitFuncs;
    std::vector<MethodImpl>         fMethodInitFuncs;
  };


};                              // namespace

#endif                          // bootstrap_codegen_init_h
