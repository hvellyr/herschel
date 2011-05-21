/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef bootstrap_codegen_apply_h
#define bootstrap_codegen_apply_h

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


  class CodegenApply : public CodeGeneratorProxy
  {
  public:
    CodegenApply(CodeGenerator* generator);

    llvm::Value* emit(const ApplyNode* node) const;

  private:
    llvm::Value* emitTypeNameForAllocate(const AptNode* node) const;
    llvm::Value* emitAllocateApply(const ApplyNode* node) const;
  };
};                              // namespace

#endif                          // bootstrap_codegen_apply_h
