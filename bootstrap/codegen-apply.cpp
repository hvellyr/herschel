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
#include "codegen-apply.h"
#include "codegen-types.h"
#include "codegen-tools.h"

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

CodegenApply::CodegenApply(CodeGenerator* generator)
  : CodeGeneratorProxy(generator)
{
}


llvm::Value*
CodegenApply::emitTypeNameForAllocate(const AptNode* node) const
{
  if (const TypeNode* typeNode = dynamic_cast<const TypeNode*>(node)) {
    Type ty = typeNode->type();
    hr_assert(ty.typeName() == String("lang|Class"));
    hr_assert(ty.hasGenerics());
    hr_assert(ty.generics().size() == 1);

    return generator()->makeGetTypeLookupCall(ty.generics()[0]);
  }
  else {
    // llvm::Value* val = wrapLoad(codegenNode(args[i]));
    hr_invalid("todo");
    return NULL;
  }
}


llvm::Value*
CodegenApply::emit(const ApplyNode* node) const
{
  llvm::Function *calleeFunc = NULL;
  bool inlineRetv = false;
  bool alwaysPassAtom = false;

  const SymbolNode* symNode = dynamic_cast<const SymbolNode*>(node->base());
  if (symNode != NULL) {
    hr_assert(symNode->refersTo() == kFunction || symNode->refersTo() == kGeneric);

    String funcnm;

    if (symNode->name() == Names::kLangAllocate)
      return emitAllocateApply(node);
    else if (symNode->name() == Names::kLangSlot)
      return emitGetSlotApply(node);
    else if (symNode->name() == Names::kLangSlotX)
      return emitSetSlotApply(node);

    if (symNode->hasCLinkage()) {
      // generic functions are not allowed to have C linkage
      hr_assert(symNode->refersTo() != kGeneric);
      funcnm = symNode->name();
      inlineRetv = false;
    }
    else {
      funcnm = herschel::mangleToC(symNode->name());
      inlineRetv = true;
      alwaysPassAtom = symNode->refersTo() == kGeneric;
    }

    calleeFunc = module()->getFunction(llvm::StringRef(funcnm));
    if (calleeFunc == NULL) {
      errorf(node->srcpos(), 0, "Unknown function referenced: %s",
             (const char*)StrHelper(funcnm));
      return NULL;
    }
  }
  else {
    // TODO
    hr_invalid("apply(!symbol) -> TODO");
  }

  const NodeList& args = node->children();
  if (calleeFunc->arg_size() != args.size() + (inlineRetv ? 1 : 0)) {
    errorf(node->srcpos(), 0, "Incorrect # arguments passed");
    return NULL;
  }

  llvm::AllocaInst* retv = NULL;
  llvm::Function* curFunction = builder().GetInsertBlock()->getParent();
  const llvm::Type* returnType = ( alwaysPassAtom
                                   ? types()->getAtomType()
                                   : types()->getType(node->type()) );
  retv = tools()->createEntryBlockAlloca(curFunction, String("local_retv"),
                                         returnType);

  std::vector<llvm::Value*> argv;
  if (inlineRetv)
    argv.push_back(retv);

  for (unsigned i = 0, e = args.size(); i != e; ++i) {
    llvm::Value* val = tools()->wrapLoad(generator()->codegenNode(args[i]));

    // TODO: can we assert that spec args are ATOM typed.
    // warningf(args[i]->srcpos(), 0, "emit pack code");
    val = tools()->emitPackCode(args[i]->dstType(), args[i]->typeConv(),
                                val, args[i]->type());

    // val->dump();
    if (val == NULL)
      return NULL;

    argv.push_back(val);
  }

  if (inlineRetv) {
    builder().CreateCall(calleeFunc, argv.begin(), argv.end());

    // TODO: if in tail position enforce ATOM return type?
    return retv;
  }
  else {
    llvm::Value* funcVal = builder().CreateCall(calleeFunc, argv.begin(), argv.end(),
                                               "xxx");
    if (node->isInTailPos()) {
      // TODO: return type id
      tools()->setAtom(retv, CodegenTools::kAtomInt32, funcVal);
      return retv;
    }
    else
      return funcVal;
  }
}


llvm::Value*
CodegenApply::emitAllocateApply(const ApplyNode* node) const
{
  const SymbolNode* symNode = dynamic_cast<const SymbolNode*>(node->base());
  hr_assert(symNode->name() == Names::kLangAllocate);
  hr_assert(symNode->refersTo() == kGeneric);

  String funcnm = String("allocate");

  llvm::Function *allocFunc = module()->getFunction(llvm::StringRef(funcnm));
  if (allocFunc == NULL) {
    std::vector<const llvm::Type*> sign;
    sign.push_back(types()->getAtomType()->getPointerTo());
    sign.push_back(types()->getTypeType()); // Type*

    llvm::FunctionType *ft = llvm::FunctionType::get(llvm::Type::getVoidTy(context()),
                                                     sign,
                                                     !K(isVarArg));

    allocFunc = llvm::Function::Create(ft,
                                     llvm::Function::ExternalLinkage,
                                     llvm::Twine("allocate"),
                                     module());
  }

  const NodeList& args = node->children();
  if (args.size() != 1) {
    errorf(node->srcpos(), 0, "Incorrect # arguments passed");
    return NULL;
  }

  std::vector<llvm::Value*> argv;
  llvm::Function* curFunction = builder().GetInsertBlock()->getParent();
  llvm::AllocaInst* retv = tools()->createEntryBlockAlloca(curFunction,
                                                           String("local_retv"),
                                                           types()->getAtomType());
  hr_assert(retv != NULL);
  argv.push_back(retv);

  for (size_t i = 0, e = args.size(); i != e; ++i) {
    llvm::Value* val = emitTypeNameForAllocate(args[i]);
    hr_assert(val != NULL);
    argv.push_back(val);
  }

  hr_assert(allocFunc != NULL);
  builder().CreateCall(allocFunc, argv.begin(), argv.end());

  // TODO: if in tail position enforce ATOM return type?
  return retv;
}


llvm::Value*
CodegenApply::emitPtrToSlot(const ApplyNode* node) const
{
  String slotFuncName = String("instance_slot");

  llvm::Function *slotFunc = module()->getFunction(llvm::StringRef(slotFuncName));
  if (slotFunc == NULL) {
    std::vector<const llvm::Type*> sign;
    sign.push_back(types()->getAtomType()->getPointerTo());
    sign.push_back(llvm::Type::getInt8PtrTy(context()));

    llvm::FunctionType *ft = llvm::FunctionType::get(llvm::Type::getInt8PtrTy(context()),
                                                     sign,
                                                     !K(isVarArg));

    slotFunc = llvm::Function::Create(ft,
                                      llvm::Function::ExternalLinkage,
                                      llvm::Twine("instance_slot"),
                                      module());
  }

  const NodeList& args = node->children();
  if (args.size() == 2) {
    errorf(node->srcpos(), 0, "Incorrect # arguments passed");
    return NULL;
  }

#if 0
  std::vector<llvm::Value*> argv;
  llvm::Function* curFunction = builder().GetInsertBlock()->getParent();
  llvm::AllocaInst* retv = tools()->createEntryBlockAlloca(curFunction,
                                                           String("local_retv"),
                                                           types()->getAtomType());
  hr_assert(retv != NULL);
  argv.push_back(retv);

  for (size_t i = 0, e = args.size(); i != e; ++i) {
    llvm::Value* val = emitTypeNameForAllocate(args[i]);
    hr_assert(val != NULL);
    argv.push_back(val);
  }

  hr_assert(allocFunc != NULL);
  builder().CreateCall(allocFunc, argv.begin(), argv.end());

  // TODO: if in tail position enforce ATOM return type?
  return retv;
#endif

  return NULL;
}


llvm::Value*
CodegenApply::emitGetSlotApply(const ApplyNode* node) const
{
  const SymbolNode* symNode = dynamic_cast<const SymbolNode*>(node->base());
  hr_assert(symNode->name() == Names::kLangSlot);
  hr_assert(symNode->refersTo() == kGeneric);

  llvm::Value* addr = emitPtrToSlot(node);
  // get value out of casted addr and store it into the retv
  return addr;
}


llvm::Value*
CodegenApply::emitSetSlotApply(const ApplyNode* node) const
{
  const SymbolNode* symNode = dynamic_cast<const SymbolNode*>(node->base());
  hr_assert(symNode->name() == Names::kLangSlotX);
  hr_assert(symNode->refersTo() == kGeneric);

  llvm::Value* addr = emitPtrToSlot(node);
  // store value into casted *addr
  return addr;
}
