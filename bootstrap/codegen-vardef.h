/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef bootstrap_codegen_vardef_h
#define bootstrap_codegen_vardef_h

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
  class VardefNode;


  class CodegenVardef : public CodeGeneratorProxy
  {
  public:
    CodegenVardef(CodeGenerator* generator);

    llvm::Value* emit(const VardefNode* node, bool isLocal) const;

  private:
    llvm::Value* codegenForLocalVars(const VardefNode* node) const;
    llvm::Value* codegenForGlobalVars(const VardefNode* node) const;
  };
};                              // namespace

#endif                          // bootstrap_codegen_vardef_h
