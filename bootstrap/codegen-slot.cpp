/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "codegen.h"
#include "apt.h"
#include "log.h"
#include "properties.h"
#include "symbol.h"
#include "xmlout.h"
#include "predefined.h"
#include "strbuf.h"
#include "codegen-init.h"
#include "codegen-slot.h"
#include "codegen-types.h"
#include "codegen-tools.h"
#include "codegen-func.h"
#include "compiler.h"
#include "utils.h"

#include <vector>

#include "llvm/Bitcode/BitstreamWriter.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Scalar.h"


//----------------------------------------------------------------------------

using namespace herschel;

CodegenSlot::CodegenSlot(CodeGenerator* generator)
  : CodeGeneratorProxy(generator)
{
}


llvm::Value*
CodegenSlot::emitSlotRefAccess(const SlotRefNode* node) const
{
  llvm::Value* addr = emitPtrToSlot(node, !K(isStore));

  llvm::Type* slotType = types()->getType(node->type())->getPointerTo();
  llvm::Value* slotAddr = builder().CreatePointerCast(addr, slotType);

  return builder().CreateLoad(slotAddr);
}


llvm::Value*
CodegenSlot::emitSlotRefAssignment(const SlotRefNode* node,
                                   const AptNode* rvalue) const
{
  llvm::Value* addr = emitPtrToSlot(node, K(isStore));

  llvm::Type* slotType = types()->getType(node->type())->getPointerTo();
  llvm::Value* slotAddr = builder().CreatePointerCast(addr, slotType);

  llvm::Value* slotRvalue = tools()->wrapLoad(generator()->codegenNode(rvalue));
  slotRvalue = tools()->emitPackCode(rvalue->dstType(),
                                     rvalue->typeConv(),
                                     slotRvalue,
                                     rvalue->type());

  return builder().CreateStore(slotRvalue, slotAddr);
}


llvm::Value*
CodegenSlot::emitPtrToSlot(const SlotRefNode* node, bool isStore) const
{
  const char* slotFuncName = "h7_instance_slot";

  llvm::Function *slotFunc = module()->getFunction(llvm::StringRef(slotFuncName));
  if (slotFunc == NULL) {
    // void* h7_instance_slot(ATOM* instance, const char* slot_name);
    llvm::FunctionType *ft = llvm::FunctionType::get(
      llvm::Type::getInt8PtrTy(context()),
      std::vector<llvm::Type*>{ types()->getAtomType(),
                                llvm::Type::getInt8PtrTy(context()) },
      !K(isVarArg));

    slotFunc = llvm::Function::Create(ft,
                                      llvm::Function::ExternalLinkage,
                                      llvm::Twine(slotFuncName),
                                      module());
  }

  // TODO: can we assert that the instance is actually ATOM typed
  llvm::Value* val = tools()->wrapLoad(generator()->codegenNode(node->base()));
  val = tools()->emitPackCode(node->base()->dstType(),
                              node->base()->typeConv(),
                              val,
                              node->base()->type());
  if (val == NULL)
    return NULL;
  llvm::Value* keywgv = initializer()->registerKeyword(node->slotName());
  llvm::Value* keyw = builder().CreateLoad(keywgv);

  hr_assert(slotFunc != NULL);
  return builder().CreateCall(slotFunc, std::vector<llvm::Value*>{ val, keyw});
}


