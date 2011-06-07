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

#include <vector>

#include "llvm/Analysis/Verifier.h"
#include "llvm/DerivedTypes.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/PassManager.h"
#include "llvm/Support/IRBuilder.h"
#include "llvm/Target/TargetData.h"
#include "llvm/Target/TargetSelect.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/Bitcode/BitstreamWriter.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/GlobalVariable.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Intrinsics.h"


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

  const llvm::Type* slotType = types()->getType(node->type())->getPointerTo();
  llvm::Value* slotAddr = builder().CreatePointerCast(addr, slotType);

  return builder().CreateLoad(slotAddr);
}


llvm::Value*
CodegenSlot::emitSlotRefAssignment(const SlotRefNode* node,
                                   const AptNode* rvalue) const
{
  llvm::Value* addr = emitPtrToSlot(node, K(isStore));

  const llvm::Type* slotType = types()->getType(node->type())->getPointerTo();
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
  const char* slotFuncName = "instance_slot";

  llvm::Function *slotFunc = module()->getFunction(llvm::StringRef(slotFuncName));
  if (slotFunc == NULL) {
    // void* instance_slot(ATOM* instance, const char* slot_name);
    std::vector<const llvm::Type*> sign;
    sign.push_back(types()->getAtomType()); //->getPointerTo());
    sign.push_back(llvm::Type::getInt8PtrTy(context()));

    llvm::FunctionType *ft = llvm::FunctionType::get(llvm::Type::getInt8PtrTy(context()),
                                                     sign,
                                                     !K(isVarArg));

    slotFunc = llvm::Function::Create(ft,
                                      llvm::Function::ExternalLinkage,
                                      llvm::Twine(slotFuncName),
                                      module());
  }

  std::vector<llvm::Value*> argv;

  // TODO: can we assert that the instance is actually ATOM typed
  llvm::Value* val = tools()->wrapLoad(generator()->codegenNode(node->base()));
  val = tools()->emitPackCode(node->base()->dstType(),
                              node->base()->typeConv(),
                              val,
                              node->base()->type());
  // val->dump();
  if (val == NULL)
    return NULL;
  argv.push_back(val);

  llvm::Value* keywgv = initializer()->registerKeyword(node->slotName());
  llvm::Value* keyw = builder().CreateLoad(keywgv);
  argv.push_back(keyw);

  hr_assert(slotFunc != NULL);
  return builder().CreateCall(slotFunc, argv.begin(), argv.end());
}


