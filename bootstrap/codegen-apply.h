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
  class Function;
};


namespace herschel
{
  class BinaryNode;
  class CodeGenerator;
  class CodegenTypeUtils;
  class SymbolNode;
  class Type;


  class CodegenApply : public CodeGeneratorProxy
  {
  public:
    CodegenApply(CodeGenerator* generator);

    llvm::Value* emit(const ApplyNode* node) const;

  private:
    llvm::Value* emitTypeNameForAllocate(const AptNode* node) const;
    llvm::Value* emitAllocateApply(const ApplyNode* node) const;
    llvm::Value* emitAllocateArrayApply(const ApplyNode* node) const;

    llvm::Value* emitGetSlotApply(const ApplyNode* node) const;
    llvm::Value* emitSetSlotApply(const ApplyNode* node) const;

    llvm::Value* emitPtrToSlot(const ApplyNode* node, bool isStore) const;

    llvm::Function* lazyDeclareExternFunction(const SymbolNode* symNode) const;

    llvm::Value* emitAllocateAtomArrayApply(const ApplyNode* node) const;
    llvm::Value* emitAllocateInt32ArrayApply(const ApplyNode* node) const;
  };
};                              // namespace

#endif                          // bootstrap_codegen_apply_h
