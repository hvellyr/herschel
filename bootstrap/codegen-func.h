/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef bootstrap_codegen_func_h
#define bootstrap_codegen_func_h

#include "llvm/Support/IRBuilder.h"

#include "codegen-proxy.h"


namespace llvm
{
  class Module;
  class Value;
};


namespace herschel
{
  class CodeGenerator;
  class CodegenTypeUtils;
  class BinaryNode;
  class Type;


  class CodegenFuncDef : public CodeGeneratorProxy
  {
  public:
    CodegenFuncDef(CodeGenerator* generator);

    llvm::Value* emit(const FuncDefNode* node, bool isLocal) const;

  private:
    struct FuncPair
    {
      llvm::FunctionType* fType;
      llvm::Function* fFunc;
      Type fRetType;
    };

    llvm::FunctionType* createFunctionSignature(const FunctionNode* node,
                                                bool inlineRetv,
                                                const Type& retty,
                                                bool isGeneric) const;
    String makeFunctionName(const FuncDefNode* node,
                            const String& methodNameSuffix) const;
    FuncPair createFunction(const FuncDefNode* node,
                            const String& methodNameSuffix,
                            bool isGeneric) const;
    llvm::Value* compileGenericFunctionDef(const FuncDefNode* node) const;
    llvm::Value* compileMethodDef(const FuncDefNode* node) const;
    llvm::Value* compileAbstractFuncDef(const FuncDefNode* node) const;
    llvm::Value* compileNormalFuncDef(const FuncDefNode* node, bool isLocal) const;
    llvm::Value* compileNormalFuncDefImpl(const FuncPair& func,
                                          const FuncDefNode* node,
                                          bool isLocal,
                                          bool forceAtomReturnType) const;
  };
};                              // namespace

#endif                          // bootstrap_codegen_func_h
