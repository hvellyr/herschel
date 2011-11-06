/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef bootstrap_codegen_slot_h
#define bootstrap_codegen_slot_h

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
  class SlotRefNode;
  class SymbolNode;
  class Type;


  class CodegenSlot : public CodeGeneratorProxy
  {
  public:
    CodegenSlot(CodeGenerator* generator);

    llvm::Value* emitSlotRefAccess(const SlotRefNode* node) const;
    llvm::Value* emitSlotRefAssignment(const SlotRefNode* node,
                                       const AptNode* rvalue) const;

  private:
    llvm::Value* emitPtrToSlot(const SlotRefNode* node, bool isStore) const;
  };
};                              // namespace

#endif                          // bootstrap_codegen_slot_h
