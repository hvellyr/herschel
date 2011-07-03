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
  else if (const SymbolNode* symNode = dynamic_cast<const SymbolNode*>(node)) {
    Type ty = symNode->type();
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


llvm::Function*
CodegenApply::lazyDeclareExternFunction(const SymbolNode* symNode) const
{
  Scope* scope = generator()->fCompiler->referredFunctionCache();
  const AptNode* node = scope->lookupFunction(symNode->name(),
                                              !K(showAmbiguousSymDef));
  const FuncDefNode* funcdef = dynamic_cast<const FuncDefNode*>(node);
  if (funcdef != NULL)
    return CodegenFuncDef(generator()).emitExternFuncDef(funcdef);

  return NULL;
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
    else if (symNode->name() == Names::kLangAllocateArray)
      return emitAllocateArrayApply(node);
    else if (symNode->name() == Names::kLangSlice) {
      const NodeList& args = node->children();
      if (args.size() == 2 && args[0]->type().isArray())
        return emitArraySliceAccess(node);
    }
    else if (symNode->name() == Names::kLangSliceX) {
      const NodeList& args = node->children();
      if (args.size() == 3 && args[0]->type().isArray())
        return emitArraySliceSet(node);
    }
    else if (symNode->name() == Names::kLangNumItems) {
      const NodeList& args = node->children();
      if (args.size() == 1 && args[0]->type().isArray())
        return emitArrayNumItems(node);
    }

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
      calleeFunc = lazyDeclareExternFunction(symNode);
      if (calleeFunc == NULL) {
        errorf(node->srcpos(), 0, "Unknown function referenced: %s",
               (const char*)StrHelper(funcnm));
        return NULL;
      }
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

    if (val == NULL)
      return NULL;

    argv.push_back(val);
  }

  if (inlineRetv) {
    builder().CreateCall(calleeFunc, argv.begin(), argv.end());

    if (alwaysPassAtom &&
        node->dstType().isPlainType() && node->typeConv() == kNoConv)
    {
      llvm::Value* val = tools()->wrapLoad(retv);
      return tools()->emitPackCode(node->dstType(),
                                   kAtom2PlainConv,
                                   val, node->type());
    }

    // TODO: if in tail position enforce ATOM return type?
    return retv;
  }
  else {
    llvm::Value* funcVal = builder().CreateCall(calleeFunc, argv.begin(), argv.end());
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
#if defined(IS_DEBUG)
  const SymbolNode* symNode = dynamic_cast<const SymbolNode*>(node->base());
  hr_assert(symNode->name() == Names::kLangAllocate);
  hr_assert(symNode->refersTo() == kGeneric);
#endif

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


//----------------------------------------------------------------------------

namespace herschel
{
class ArrayAllocateStrategy : public RefCountable
{
public:
  ArrayAllocateStrategy(const CodegenApply* apply)
    : fApply(apply)
  {}

  virtual String allocateFuncName() const = 0;

  virtual std::vector<const llvm::Type*> allocateFuncSignature() const = 0;

  virtual llvm::Value* initValue(const ApplyNode* node) const = 0;

  virtual llvm::Value* typeTagArgument(const ApplyNode* node) const = 0;

protected:
  const CodegenApply* fApply;
};


class AtomArrayAllocateStrategy : public ArrayAllocateStrategy
{
public:
  AtomArrayAllocateStrategy(const CodegenApply* apply)
    : ArrayAllocateStrategy(apply)
  {}

  virtual String allocateFuncName() const
  {
    return String("allocate_array");
  }

  virtual std::vector<const llvm::Type*> allocateFuncSignature() const
  {
    std::vector<const llvm::Type*> sign;

    sign.push_back(fApply->types()->getAtomType()->getPointerTo());
    sign.push_back(fApply->types()->getTypeType());
    sign.push_back(fApply->types()->getAtomType());
    sign.push_back(fApply->types()->getSizeTTy());

    return sign;
  }

  virtual llvm::Value* initValue(const ApplyNode* node) const
  {
    return llvm::Constant::getNullValue(fApply->types()->getType(node->type()));
  }

  virtual llvm::Value* typeTagArgument(const ApplyNode* node) const
  {
    const NodeList& args = node->children();
    llvm::Value* val = fApply->emitTypeNameForAllocate(args[0]);
    hr_assert(val != NULL);
    return val;
  }
};


class Int32ArrayAllocateStrategy : public ArrayAllocateStrategy
{
public:
  Int32ArrayAllocateStrategy(const CodegenApply* apply)
    : ArrayAllocateStrategy(apply)
  {}

  virtual String allocateFuncName() const
  {
    return String("allocate_int32_array");
  }

  virtual std::vector<const llvm::Type*> allocateFuncSignature() const
  {
    std::vector<const llvm::Type*> sign;

    sign.push_back(fApply->types()->getAtomType()->getPointerTo());
    sign.push_back(fApply->types()->getTagIdType());
    sign.push_back(llvm::Type::getInt32Ty(fApply->context()));
    sign.push_back(fApply->types()->getSizeTTy());

    return sign;
  }

  virtual llvm::Value* initValue(const ApplyNode* node) const
  {
    return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(fApply->context()));
  }

  virtual llvm::Value* typeTagArgument(const ApplyNode* node) const
  {
    return fApply->tools()->emitTypeId(CodegenTools::kAtomInt32Array);
  }
};


class CharArrayAllocateStrategy : public ArrayAllocateStrategy
{
public:
  CharArrayAllocateStrategy(const CodegenApply* apply)
    : ArrayAllocateStrategy(apply)
  {}

  virtual String allocateFuncName() const
  {
    return String("allocate_char_array");
  }

  virtual std::vector<const llvm::Type*> allocateFuncSignature() const
  {
    std::vector<const llvm::Type*> sign;

    sign.push_back(fApply->types()->getAtomType()->getPointerTo());
    sign.push_back(fApply->types()->getTagIdType());
    sign.push_back(llvm::Type::getInt32Ty(fApply->context()));
    sign.push_back(fApply->types()->getSizeTTy());

    return sign;
  }

  virtual llvm::Value* initValue(const ApplyNode* node) const
  {
    return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(fApply->context()));
  }

  virtual llvm::Value* typeTagArgument(const ApplyNode* node) const
  {
    return fApply->tools()->emitTypeId(CodegenTools::kAtomCharArray);
  }
};
};


llvm::Value*
CodegenApply::emitAllocateArrayApply(const ApplyNode* node) const
{
#if defined(IS_DEBUG)
  const SymbolNode* symNode = dynamic_cast<const SymbolNode*>(node->base());
  hr_assert(symNode->name() == Names::kLangAllocateArray);
  hr_assert(symNode->refersTo() == kGeneric);
#endif

  Ptr<ArrayAllocateStrategy> strategy;

  // TODO: use type specialed array functions allocate_int_array, etc.
  if (node->type().typeId() == String("lang|Int32[]") ||
      node->type().typeId() == String("lang|UInt32[]"))
  {
    strategy = new Int32ArrayAllocateStrategy(this);
  }
  else if (node->type().typeId() == String("lang|Char[]")) {
    strategy = new CharArrayAllocateStrategy(this);
  }
  else {
    strategy = new AtomArrayAllocateStrategy(this);
  }

  String funcnm = strategy->allocateFuncName();

  llvm::Function *allocFunc = module()->getFunction(llvm::StringRef(funcnm));
  if (allocFunc == NULL) {
    // void allocate_array(ATOM* instance, Type* ty, ATOM init_value, size_t items);

    std::vector<const llvm::Type*> sign = strategy->allocateFuncSignature();
    llvm::FunctionType *ft = llvm::FunctionType::get(llvm::Type::getVoidTy(context()),
                                                     sign,
                                                     !K(isVarArg));

    allocFunc = llvm::Function::Create(ft,
                                       llvm::Function::ExternalLinkage,
                                       llvm::Twine(StrHelper(funcnm)),
                                       module());
  }

  std::vector<llvm::Value*> argv;
  llvm::Function* curFunction = builder().GetInsertBlock()->getParent();
  llvm::AllocaInst* retv = tools()->createEntryBlockAlloca(curFunction,
                                                           String("local_retv"),
                                                           types()->getAtomType());

  hr_assert(retv != NULL);
  argv.push_back(retv);

  const SymbolNode* typeNode = NULL;
  const AptNode* sizeNode = NULL;
  llvm::Value* initValue = NULL;

  const NodeList& args = node->children();
  if (args.size() == 2) {
    typeNode = dynamic_cast<const SymbolNode*>(args[0].obj());
    hr_assert(typeNode != NULL);

    sizeNode = args[1];
    initValue = strategy->initValue(node);
  }
  else if (args.size() == 3) {
    typeNode = dynamic_cast<const SymbolNode*>(args[0].obj());
    hr_assert(typeNode != NULL);

    const KeyargNode* valueNode = dynamic_cast<const KeyargNode*>(args[1].obj());
    hr_assert(valueNode != NULL);
    hr_assert(valueNode->key() == String("value"));

    initValue = tools()->wrapLoad(generator()->codegenNode(valueNode));
    initValue = tools()->emitPackCode(valueNode->dstType(), valueNode->typeConv(),
                                      initValue, valueNode->type());

    sizeNode = args[2];
    hr_assert(sizeNode != NULL);
  }
  else {
    errorf(node->srcpos(), 0, "Incorrect # arguments passed");
    return NULL;
  }

  // arg1: tag id / type
  argv.push_back(strategy->typeTagArgument(node));

  // arg 2: the init value
  argv.push_back(initValue);

  // arg 3: the element count
  llvm::Value* itemsVal = NULL;
  if (const IntNode* intNode = dynamic_cast<const IntNode*>(sizeNode)) {
    itemsVal = tools()->emitSizeTValue(intNode->value());
  }
  else {
    itemsVal = tools()->wrapLoad(generator()->codegenNode(sizeNode));
    itemsVal = ( (sizeNode->type().isPlainType())
                 ? itemsVal
                 : tools()->convertToPlainInt(itemsVal,
                                              Type::newInt32(), // why no convert to
                                                                // size_t directly?
                                              kAtom2PlainConv));
    itemsVal = builder().CreateIntCast(itemsVal, types()->getSizeTTy(),
                                       !K(isSigned));
  }

  hr_assert(itemsVal != NULL);
  argv.push_back(itemsVal);


  hr_assert(allocFunc != NULL);
  builder().CreateCall(allocFunc, argv.begin(), argv.end());

  // TODO: if in tail position enforce ATOM return type?
  return retv;
}


//----------------------------------------------------------------------------------------

llvm::Value*
CodegenApply::emitArraySize(const ApplyNode* node) const
{
  const NodeList& args = node->children();
  hr_assert(args.size() == 1);
  hr_assert(args[0]->type().isArray());

  Type arrayBaseType = args[0]->type().arrayBaseType();

  llvm::Value* arrayAtom = generator()->codegenNode(args[0]);

  llvm::Value* arrayAtomPayload = builder().CreateStructGEP(arrayAtom, 1);

  const llvm::Type* payloadType = types()->getArrayPayloadType()->getPointerTo()
                                         ->getPointerTo();
  llvm::Value* arrayPayloadTyped = builder().CreatePointerCast(arrayAtomPayload,
                                                               payloadType);

  // access the size slot in the array struct
  llvm::Value* loadedPayload = builder().CreateLoad(arrayPayloadTyped);
  llvm::Value* numItems = builder().CreateStructGEP(loadedPayload, 0);

  return builder().CreatePointerCast(numItems, llvm::Type::getInt32Ty(context())->getPointerTo());
}


CodegenApply::ArraySliceAccessData
CodegenApply::emitArraySliceAddress(const ApplyNode* node) const
{
  const NodeList& args = node->children();
  hr_assert(args.size() >= 2);
  hr_assert(args[0]->type().isArray());

  Type arrayBaseType = args[0]->type().arrayBaseType();

  llvm::Value* arrayAtom = generator()->codegenNode(args[0]);

  llvm::Value* arrayAtomPayload = builder().CreateStructGEP(arrayAtom, 1);

  const llvm::Type* payloadType = types()->getArrayPayloadType()->getPointerTo()
                                         ->getPointerTo();
  llvm::Value* arrayPayloadTyped = builder().CreatePointerCast(arrayAtomPayload,
                                                               payloadType);

  // access the data member in the array struct
  llvm::Value* loadedPayload = builder().CreateLoad(arrayPayloadTyped);
  llvm::Value* arrayData = builder().CreateStructGEP(loadedPayload, 1);
  const llvm::Type* arrayType = llvm::ArrayType::get(types()->getType(arrayBaseType),
                                                     0)->getPointerTo();
  llvm::Value* typedArray = builder().CreatePointerCast(arrayData, arrayType);

  llvm::Value* addr;
  if (const IntNode* idxNode = dynamic_cast<const IntNode*>(args[1].obj())) {
    addr = builder().CreateStructGEP(typedArray,
                                     idxNode->value());
  }
  else {
    llvm::Value* idxValue = tools()->wrapLoad(generator()->codegenNode(args[1]));
    llvm::Value* idxValue2 = ( (args[1]->type().isPlainType())
                               ? idxValue
                               : tools()->convertToPlainInt(idxValue,
                                                            Type::newInt32(),
                                                            kAtom2PlainConv) );

    std::vector<llvm::Value*> argv;
    argv.push_back(tools()->emitSizeTValue(0));
    argv.push_back(idxValue2);

    addr = builder().CreateGEP(typedArray,
                              argv.begin(), argv.end());
  }

  ArraySliceAccessData retv;
  retv.fArray = arrayAtom;
  retv.fAddr = addr;

  return retv;
}


llvm::Value*
CodegenApply::emitArraySliceAccess(const ApplyNode* node) const
{
  const NodeList& args = node->children();
  hr_assert(args.size() == 2);
  hr_assert(args[0]->type().isArray());

  ArraySliceAccessData arrayAccces = emitArraySliceAddress(node);
  llvm::Value* arraySliceVal = builder().CreateLoad(arrayAccces.fAddr);

  return tools()->emitPackCode(node->dstType(), node->typeConv(),
                               arraySliceVal, node->type());
}


llvm::Value*
CodegenApply::emitArraySliceSet(const ApplyNode* node) const
{
  const NodeList& args = node->children();
  hr_assert(args.size() == 3);
  hr_assert(args[0]->type().isArray());

  llvm::Value* newVal = tools()->wrapLoad(generator()->codegenNode(args[2]));

  ArraySliceAccessData arrayAccces = emitArraySliceAddress(node);

  const llvm::Type* ptrType = types()->getType(args[0]->type().arrayBaseType())->getPointerTo();
  llvm::Value* ptrAddr = builder().CreatePointerCast(arrayAccces.fAddr, ptrType);

  builder().CreateStore(newVal, ptrAddr);

  // the lang|slice! method returns the array itself, not the new set value.
  return arrayAccces.fArray;
}


llvm::Value*
CodegenApply::emitArrayNumItems(const ApplyNode* node) const
{
  const NodeList& args = node->children();
  hr_assert(args.size() == 1);
  hr_assert(args[0]->type().isArray());

  llvm::Value* numItems = emitArraySize(node);
  llvm::Value* numItemsVal = builder().CreateLoad(numItems);

  return tools()->wrapLoad(tools()->makeIntAtom(numItemsVal, CodegenTools::kAtomInt32));
}
