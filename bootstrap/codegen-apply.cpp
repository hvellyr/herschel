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

CodegenApply::CodegenApply(CodeGenerator& generator)
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

    return generator().makeGetTypeLookupCall(ty.generics()[0]);
  }
  else if (const SymbolNode* symNode = dynamic_cast<const SymbolNode*>(node)) {
    Type ty = symNode->type();
    hr_assert(ty.typeName() == String("lang|Class"));
    hr_assert(ty.hasGenerics());
    hr_assert(ty.generics().size() == 1);

    return generator().makeGetTypeLookupCall(ty.generics()[0]);
  }
  else {
    // llvm::Value* val = wrapLoad(*codegenNode(args[i]));
    hr_invalid("todo");
    return nullptr;
  }
}


llvm::Function*
CodegenApply::lazyDeclareExternFunction(const String& funcnm) const
{
  auto funcache = generator().fCompiler.referredFunctionCache();

  const AptNode* node = funcache->lookupFunction(funcnm,
                                                 !K(showAmbiguousSymDef));
  //fprintf(stderr, "op: %s %p\n", (zstring)StrHelper(funcnm), node);

  const FuncDefNode* funcdef = dynamic_cast<const FuncDefNode*>(node);
  if (funcdef)
    return CodegenFuncDef(generator()).emitExternFuncDef(funcdef);

  return nullptr;
}


llvm::Value*
CodegenApply::emitFunctionCall(const SrcPos& srcpos,
                               const String& clearFuncnm,
                               const String& mangledFuncnm,
                               const NodeList& args,
                               const Type& funcRetType,
                               const Type& dstType,
                               TypeConvKind dstConvKind,
                               bool isInTailPos,
                               bool inlineRetv,
                               bool alwaysPassAtom) const
{
  llvm::Function *calleeFunc = module()->getFunction(llvm::StringRef(mangledFuncnm));
  if (!calleeFunc) {
    calleeFunc = lazyDeclareExternFunction(clearFuncnm);
    if (!calleeFunc) {
      errorf(srcpos, 0, "Unknown function referenced: %s",
             (zstring)StrHelper(clearFuncnm));
      return nullptr;
    }
  }

  if (calleeFunc->arg_size() != args.size() + (inlineRetv ? 1 : 0)) {
    errorf(srcpos, 0, "Incorrect # arguments passed");
    return nullptr;
  }

  llvm::AllocaInst* retv = nullptr;
  llvm::Function* curFunction = builder().GetInsertBlock()->getParent();
  llvm::Type* returnType = ( alwaysPassAtom
                             ? types().getAtomType()
                             : types().getType(funcRetType) );
  retv = tools().createEntryBlockAlloca(curFunction, String("local_retv"),
                                         returnType);

  std::vector<llvm::Value*> argv;
  if (inlineRetv)
    argv.push_back(retv);

  for (auto& arg : args) {
    auto val = tools().wrapLoad(generator().codegenNode(*arg));

    // TODO: can we assert that spec args are ATOM typed.
    val = tools().emitPackCode(arg->dstType(), arg->typeConv(),
                               val, arg->type());

    if (!val)
      return nullptr;

    argv.push_back(tools().wrapLoad(val));
  }

  if (inlineRetv) {
    builder().CreateCall(calleeFunc, argv);

    if (alwaysPassAtom && dstType.isPlainType() && dstConvKind == kNoConv)
    {
      llvm::Value* val = tools().wrapLoad(retv);
      return tools().emitPackCode(dstType,
                                  kAtom2PlainConv,
                                  val, funcRetType);
    }

    // TODO: if in tail position enforce ATOM return type?
    return retv;
  }
  else {
    llvm::Value* funcVal = builder().CreateCall(calleeFunc, argv);
    if (isInTailPos) {
      // TODO: return type id
      tools().setAtom(retv, CodegenTools::kAtomInt32, funcVal);
      return retv;
    }
    else
      return funcVal;
  }
}


llvm::Value*
CodegenApply::emit(const ApplyNode* node) const
{
  bool inlineRetv = false;
  bool alwaysPassAtom = false;

  auto symNode = dynamic_cast<const SymbolNode*>(node->base().get());
  if (symNode) {
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
    else if (symNode->name() == Names::kLangIsaQ) {
      return emitIsaApply(node);
    }
    else if (symNode->name() == Names::kLangToChar) {
      return emitToCharApply(node);
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

    return emitFunctionCall(node->srcpos(),
                            symNode->name(),
                            funcnm,
                            node->children(),
                            node->type(),
                            node->dstType(),
                            node->typeConv(),
                            node->isInTailPos(),
                            inlineRetv,
                            alwaysPassAtom);
  }
  else {
    // TODO
    hr_invalid("apply(!symbol) -> TODO");
    return nullptr;
  }
}


llvm::Value*
CodegenApply::emitAllocateApply(const ApplyNode* node) const
{
#if defined(IS_DEBUG)
  auto symNode = dynamic_cast<const SymbolNode*>(node->base().get());
  hr_assert(symNode->name() == Names::kLangAllocate);
  hr_assert(symNode->refersTo() == kGeneric);
#endif

  const NodeList& args = node->children();
  if (args.size() != 1) {
    errorf(node->srcpos(), 0, "Incorrect # arguments passed");
    return nullptr;
  }

  return emitAllocateApplyImpl(args[0].get());
}


llvm::Value*
CodegenApply::emitAllocateApplyImpl(const AptNode* typeNode) const
{
  String funcnm = String("h7_allocate");

  llvm::Function *allocFunc = module()->getFunction(llvm::StringRef(funcnm));
  if (!allocFunc) {
    llvm::FunctionType *ft = llvm::FunctionType::get(
      llvm::Type::getVoidTy(context()),
      std::vector<llvm::Type*>{ types().getAtomType()->getPointerTo(),
                                types().getTypeType() }, // Type*
      !K(isVarArg));
    allocFunc = llvm::Function::Create(ft,
                                     llvm::Function::ExternalLinkage,
                                     llvm::Twine("h7_allocate"),
                                     module());
  }

  std::vector<llvm::Value*> argv;
  llvm::Function* curFunction = builder().GetInsertBlock()->getParent();
  llvm::AllocaInst* retv = tools().createEntryBlockAlloca(curFunction,
                                                          String("local_retv"),
                                                          types().getAtomType());
  hr_assert(retv);
  argv.push_back(retv);

  llvm::Value* val = emitTypeNameForAllocate(typeNode);
  hr_assert(val);
  argv.push_back(val);

  hr_assert(allocFunc);
  builder().CreateCall(allocFunc, argv);

  // TODO: if in tail position enforce ATOM return type?
  return retv;
}


//------------------------------------------------------------------------------

llvm::Value*
CodegenApply::emitIsaApply(const ApplyNode* applyNode) const
{
  String funcnm = String("h7_instance_isa");

  llvm::Function *isaFunc = module()->getFunction(llvm::StringRef(funcnm));
  if (!isaFunc) {
    // int h7_instance_isa(ATOM, Type*)
    llvm::FunctionType *ft = llvm::FunctionType::get(llvm::Type::getInt1Ty(context()),
                                                     std::vector<llvm::Type*>{
                                                       types().getAtomType(),
                                                       types().getTypeType() },
                                                     !K(isVarArg));
    isaFunc = llvm::Function::Create(ft,
                                     llvm::Function::ExternalLinkage,
                                     llvm::Twine("h7_instance_isa"),
                                     module());
  }

  std::vector<llvm::Value*> argv;

  const NodeList& args = applyNode->children();

  llvm::Value* baseExpr = tools().wrapLoad(generator().codegenNode(*args[0]));
  baseExpr = tools().emitPackCode(args[0]->dstType(), args[0]->typeConv(),
                                  baseExpr, args[0]->type());
  hr_assert(baseExpr);
  argv.push_back(baseExpr);

  llvm::Value* tyExpr = emitTypeNameForAllocate(args[1].get());
  hr_assert(tyExpr);
  argv.push_back(tyExpr);

  hr_assert(isaFunc);
  // TODO: if in tail position enforce ATOM return type?
  return builder().CreateCall(isaFunc, argv);
}


//----------------------------------------------------------------------------

llvm::Value*
CodegenApply::convertToPlainInt(const Type& dstType,
                               const AptNode* isNode,
                               llvm::Value* value) const
{
  switch (isNode->typeConv()) {
  case kNoConv:
    return builder().CreateIntCast(value, types().getType(dstType), isNode->type().isSigned());
  case kAtom2PlainConv:
    return tools().makeTypeCastAtomToPlain(value, dstType);
  case kPlain2AtomConv:
  case kTypeCheckConv:
    hr_invalid("");
  }

  return nullptr;
}


llvm::Value*
CodegenApply::emitToCharApply(const ApplyNode* applyNode) const
{
  const NodeList& args = applyNode->children();

  llvm::Value *val = tools().wrapLoad(generator().codegenNode(*args[0]));
  if (!val)
    return nullptr;

  return tools().emitPackCode(applyNode->dstType(),
                              kTypeCheckConv,
                              convertToPlainInt(Type::newUInt32(),
                                                args[0].get(), val),
                              applyNode->type());
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

  virtual bool passInitValueToAllocateCall() const = 0;

  virtual String allocateFuncName() const = 0;

  virtual std::vector<llvm::Type*> allocateFuncSignature() const = 0;

  virtual llvm::Value* initValue(const ApplyNode* node) const = 0;

  virtual llvm::Value* typeTagArgument(const ApplyNode* node) const = 0;

  virtual llvm::Value* postInit(const ApplyNode* node,
                                llvm::Value* retv,
                                const Type& arrayBaseType,
                                llvm::Value* sizeVal,
                                llvm::Value* eplicitInitValue) const = 0;

protected:
  const CodegenApply* fApply;
};


class AtomArrayAllocateStrategy : public ArrayAllocateStrategy
{
public:
  AtomArrayAllocateStrategy(const CodegenApply* apply)
    : ArrayAllocateStrategy(apply)
  {}

  virtual bool passInitValueToAllocateCall() const
  {
    return false;
  }

  virtual String allocateFuncName() const
  {
    return String("h7_allocate_array");
  }

  virtual std::vector<llvm::Type*> allocateFuncSignature() const
  {
    return std::vector<llvm::Type*>{
      fApply->types().getAtomType()->getPointerTo(),
      fApply->types().getTypeType(),
      fApply->types().getSizeTTy() };
  }

  virtual llvm::Value* initValue(const ApplyNode* node) const
  {
    const NodeList& args = node->children();
    return fApply->emitAllocateApplyImpl(args[0].get());
  }

  virtual llvm::Value* typeTagArgument(const ApplyNode* node) const
  {
    const NodeList& args = node->children();
    llvm::Value* val = fApply->emitTypeNameForAllocate(args[0].get());
    hr_assert(val);
    return val;
  }

  virtual llvm::Value* postInit(const ApplyNode* node,
                                llvm::Value* retv,
                                const Type& arrayBaseType,
                                llvm::Value* sizeVal,
                                llvm::Value* explicitInitValue) const
  {
    llvm::Function *curFunction = fApply->builder().GetInsertBlock()->getParent();
    llvm::AllocaInst* counter = fApply->tools().createEntryBlockAlloca(
      curFunction, String("i"),
      llvm::Type::getInt32Ty(fApply->context()));
    fApply->builder().CreateStore(
      llvm::ConstantInt::get(fApply->context(),
                             llvm::APInt(32, (int)0, !K(isSigned))),
      counter);

    llvm::BasicBlock *loopHeadBB = llvm::BasicBlock::Create(fApply->context(),
                                                            "loophead", curFunction);
    llvm::BasicBlock *loopBB = llvm::BasicBlock::Create(fApply->context(),
                                                        "loop", curFunction);
    // Create the "after loop" block and insert it.
    llvm::BasicBlock *afterBB = llvm::BasicBlock::Create(fApply->context(),
                                                         "afterloop",
                                                         curFunction);

    // Insert an explicit fall through from the current block to the loopBB.
    fApply->builder().CreateBr(loopHeadBB);

    // Start insertion in loopBB.
    fApply->builder().SetInsertPoint(loopHeadBB);

    // Convert condition to a bool by comparing equal to 1
    llvm::Value *testValue = fApply->builder().CreateICmpSLT(
      fApply->builder().CreateIntCast(fApply->builder().CreateLoad(counter),
                                      fApply->types().getSizeTTy(),
                                      !K(isSigned)),
      sizeVal, "loopcond");

    // Insert the conditional branch into the end of loopEndBB.
    fApply->builder().CreateCondBr(testValue, loopBB, afterBB);

    // Start insertion in loopBB.
    fApply->builder().SetInsertPoint(loopBB);

    CodegenApply::ArraySliceAccessData arrayAccces =
      fApply->emitArraySliceAddress(retv,
                                    arrayBaseType,
                                    counter);
    llvm::Type* ptrType = fApply->types().getType(arrayBaseType)->getPointerTo();
    llvm::Value* ptrAddr = fApply->builder().CreatePointerCast(arrayAccces.fAddr, ptrType);

    llvm::Value* initVal = ( explicitInitValue
                             ? explicitInitValue
                             : fApply->builder().CreateLoad(initValue(node)) );
    fApply->builder().CreateStore(initVal, ptrAddr);

    llvm::Value* newCounter = fApply->builder().CreateAdd(
      fApply->builder().CreateLoad(counter),
      llvm::ConstantInt::get(fApply->context(),
                             llvm::APInt(32, (int)1, !K(isSigned))),
      "newcounter");
    fApply->builder().CreateStore(newCounter, counter);

    // jump back to loop start
    fApply->builder().CreateBr(loopHeadBB);

    // Any new code will be inserted in AfterBB.
    fApply->builder().SetInsertPoint(afterBB);

    return retv;
  }
};


class Int32ArrayAllocateStrategy : public ArrayAllocateStrategy
{
public:
  Int32ArrayAllocateStrategy(const CodegenApply* apply)
    : ArrayAllocateStrategy(apply)
  {}

  virtual bool passInitValueToAllocateCall() const
  {
    return true;
  }

  virtual String allocateFuncName() const
  {
    return String("h7_allocate_int32_array");
  }

  virtual std::vector<llvm::Type*> allocateFuncSignature() const
  {
    return std::vector<llvm::Type*>{
      fApply->types().getAtomType()->getPointerTo(),
      fApply->types().getTagIdType(),
      llvm::Type::getInt32Ty(fApply->context()),
      fApply->types().getSizeTTy() };
  }

  virtual llvm::Value* initValue(const ApplyNode* node) const
  {
    return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(fApply->context()));
  }

  virtual llvm::Value* typeTagArgument(const ApplyNode* node) const
  {
    return fApply->tools().emitTypeId(CodegenTools::kAtomInt32Array);
  }

  virtual llvm::Value* postInit(const ApplyNode* node,
                                llvm::Value* retv,
                                const Type& arrayBaseType,
                                llvm::Value* sizeVal,
                                llvm::Value* explicitInitValue) const
  {
    return retv;
  }
};


class CharArrayAllocateStrategy : public ArrayAllocateStrategy
{
public:
  CharArrayAllocateStrategy(const CodegenApply* apply)
    : ArrayAllocateStrategy(apply)
  {}

  virtual bool passInitValueToAllocateCall() const
  {
    return true;
  }

  virtual String allocateFuncName() const
  {
    return String("allocate_char_array");
  }

  virtual std::vector<llvm::Type*> allocateFuncSignature() const
  {
    return std::vector<llvm::Type*>{ fApply->types().getAtomType()->getPointerTo(),
                                     fApply->types().getTagIdType(),
                                     llvm::Type::getInt32Ty(fApply->context()),
                                     fApply->types().getSizeTTy() };
  }

  virtual llvm::Value* initValue(const ApplyNode* node) const
  {
    return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(fApply->context()));
  }

  virtual llvm::Value* typeTagArgument(const ApplyNode* node) const
  {
    return fApply->tools().emitTypeId(CodegenTools::kAtomCharArray);
  }

  virtual llvm::Value* postInit(const ApplyNode* node,
                                llvm::Value* retv,
                                const Type& arrayBaseType,
                                llvm::Value* sizeVal,
                                llvm::Value* explicitInitValue) const
  {
    return retv;
  }
};
};


llvm::Value*
CodegenApply::emitAllocateArrayApply(const ApplyNode* node) const
{
#if defined(IS_DEBUG)
  auto symNode = dynamic_cast<const SymbolNode*>(node->base().get());
  hr_assert(symNode->name() == Names::kLangAllocateArray);
  hr_assert(symNode->refersTo() == kGeneric);
#endif

  Ptr<ArrayAllocateStrategy> strategy;

  // TODO: use type specialed array functions allocate_int_array, etc.
  if (node->type().typeId() == arrayTypeName(Names::kInt32TypeName) ||
      node->type().typeId() == arrayTypeName(Names::kUInt32TypeName))
  {
    strategy = new Int32ArrayAllocateStrategy(this);
  }
  else if (node->type().typeId() == arrayTypeName(Names::kCharTypeName)) {
    strategy = new CharArrayAllocateStrategy(this);
  }
  else {
    strategy = new AtomArrayAllocateStrategy(this);
  }

  String funcnm = strategy->allocateFuncName();

  llvm::Function *allocFunc = module()->getFunction(llvm::StringRef(funcnm));
  if (!allocFunc) {
    // void h7_allocate_array(ATOM* instance, Type* ty, size_t items);

    std::vector<llvm::Type*> sign = strategy->allocateFuncSignature();
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
  llvm::AllocaInst* retv = tools().createEntryBlockAlloca(curFunction,
                                                          String("local_retv"),
                                                          types().getAtomType());

  hr_assert(retv);
  argv.push_back(retv);

  const SymbolNode* typeNode = nullptr;
  const AptNode* sizeNode = nullptr;
  llvm::Value* initValue = nullptr;

  const NodeList& args = node->children();
  if (args.size() == 2) {
    typeNode = dynamic_cast<const SymbolNode*>(args[0].get());
    hr_assert(typeNode);

    sizeNode = args[1].get();
    if (strategy->passInitValueToAllocateCall())
      initValue = strategy->initValue(node);
  }
  else if (args.size() == 3) {
    typeNode = dynamic_cast<const SymbolNode*>(args[0].get());
    hr_assert(typeNode);

    auto valueParam = dynamic_cast<const KeyargNode*>(args[1].get());
    hr_assert(valueParam);
    hr_assert(valueParam->key() == String("value"));

    auto valueNode = valueParam->value().get();

    // TODO: strange.  the valueNode (so the real value of a keyarg node) does
    // not have its dstType() and typeConv() properly set.  These values are
    // only set on the KeyArg node itself.
    initValue = tools().wrapLoad(generator().codegenNode(*valueNode));
    initValue = tools().emitPackCode(valueParam->dstType(), valueParam->typeConv(),
                                     initValue, valueParam->type());

    sizeNode = args[2].get();
    hr_assert(sizeNode);
  }
  else {
    errorf(node->srcpos(), 0, "Incorrect # arguments passed");
    return nullptr;
  }

  // arg1: tag id / type
  argv.push_back(strategy->typeTagArgument(node));

  // arg 2: the init value
  if (strategy->passInitValueToAllocateCall())
    argv.push_back(initValue);

  // arg 3: the element count
  llvm::Value* elementCountValue = nullptr;
  if (const IntNode* intNode = dynamic_cast<const IntNode*>(sizeNode)) {
    elementCountValue = tools().emitSizeTValue(intNode->value());
  }
  else {
    elementCountValue = tools().wrapLoad(generator().codegenNode(*sizeNode));
    elementCountValue = ( (sizeNode->type().isPlainType())
                          ? elementCountValue
                          : tools().convertToPlainInt(elementCountValue,
                                                       // why no convert to size_t directly?
                                                       Type::newInt32(),
                                                       kAtom2PlainConv));
    elementCountValue = builder().CreateIntCast(elementCountValue,
                                                types().getSizeTTy(),
                                                !K(isSigned));
  }

  hr_assert(elementCountValue);
  argv.push_back(elementCountValue);

  hr_assert(allocFunc);
  builder().CreateCall(allocFunc, argv);

  llvm::Value* retv2 = strategy->postInit(node,
                                          retv,
                                          node->type().arrayBaseType(),
                                          elementCountValue,
                                          initValue);


  // TODO: if in tail position enforce ATOM return type?
  return retv2;
}


//----------------------------------------------------------------------------------------

llvm::Value*
CodegenApply::emitArraySize(const ApplyNode* node) const
{
  const NodeList& args = node->children();
  hr_assert(args.size() == 1);
  hr_assert(args[0]->type().isArray());

  Type arrayBaseType = args[0]->type().arrayBaseType();

  auto arrayAtom = generator().codegenNode(*args[0]);

  auto arrayAtomPayload = builder().CreateStructGEP(types().getAtomType(),
                                                    arrayAtom, 1);

  llvm::Type* payloadType = types().getArrayPayloadType()->getPointerTo()
                                   ->getPointerTo();
  llvm::Value* arrayPayloadTyped = builder().CreatePointerCast(arrayAtomPayload,
                                                               payloadType);

  // access the size slot in the array struct
  llvm::Value* loadedPayload = builder().CreateLoad(arrayPayloadTyped);
  llvm::Value* numItems = builder().CreateStructGEP(types().getArrayPayloadType(),
                                                    loadedPayload, 0);

  return builder().CreatePointerCast(numItems, llvm::Type::getInt32Ty(context())->getPointerTo());
}


CodegenApply::ArraySliceAccessData
CodegenApply::emitArraySliceAddress(llvm::Value* arrayAtom,
                                    const Type& arrayBaseType,
                                    llvm::Value* idxValue) const
{
  llvm::Value* arrayAtomPayload = builder().CreateStructGEP(types().getAtomType(),
                                                            arrayAtom, 1);

  llvm::Type* payloadType = types().getArrayPayloadType()->getPointerTo()
                                   ->getPointerTo();
  llvm::Value* arrayPayloadTyped = builder().CreatePointerCast(arrayAtomPayload,
                                                               payloadType);

  // access the data member in the array struct
  llvm::Value* loadedPayload = builder().CreateLoad(arrayPayloadTyped);
  llvm::Value* arrayData = builder().CreateStructGEP(types().getArrayPayloadType(),
                                                     loadedPayload, 1);
  llvm::Type* arrayType = llvm::ArrayType::get(types().getType(arrayBaseType),
                                               0)->getPointerTo();
  llvm::Value* typedArray = builder().CreatePointerCast(arrayData, arrayType);

  std::vector<llvm::Value*> argv = vector_of(tools().emitSizeTValue(0))
                                            (builder().CreateLoad(idxValue));

  llvm::Value* addr = builder().CreateGEP(typedArray, argv);

  ArraySliceAccessData retv;
  retv.fArray = arrayAtom;
  retv.fAddr = addr;

  return retv;
}


CodegenApply::ArraySliceAccessData
CodegenApply::emitArraySliceAddress(const ApplyNode* node) const
{
  const NodeList& args = node->children();
  hr_assert(args.size() >= 2);
  hr_assert(args[0]->type().isArray());

  Type arrayBaseType = args[0]->type().arrayBaseType();

  auto arrayAtom = generator().codegenNode(*args[0]);

  llvm::Value* arrayAtomPayload = builder().CreateStructGEP(types().getAtomType(),
                                                            arrayAtom, 1);

  llvm::Type* payloadType = types().getArrayPayloadType()->getPointerTo()
                                   ->getPointerTo();
  llvm::Value* arrayPayloadTyped = builder().CreatePointerCast(arrayAtomPayload,
                                                               payloadType);

  // access the data member in the array struct
  llvm::Value* loadedPayload = builder().CreateLoad(arrayPayloadTyped);
  llvm::Value* arrayData = builder().CreateStructGEP(types().getArrayPayloadType(),
                                                     loadedPayload, 1);
  llvm::Type* arrayType = llvm::ArrayType::get(types().getType(arrayBaseType), 0);
  llvm::Type* arrayPtrType = arrayType->getPointerTo();
  llvm::Value* typedArray = builder().CreatePointerCast(arrayData, arrayPtrType);

  llvm::Value* addr;
  if (auto idxNode = dynamic_cast<const IntNode*>(args[1].get())) {
    addr = builder().CreateStructGEP(arrayType,
                                     typedArray,
                                     idxNode->value());
  }
  else {
    auto idxValue = tools().wrapLoad(generator().codegenNode(*args[1]));
    auto idxValue2 = ( (args[1]->type().isPlainType())
                       ? idxValue
                       : tools().convertToPlainInt(idxValue,
                                                   Type::newInt32(),
                                                   kAtom2PlainConv) );

    std::vector<llvm::Value*> argv = vector_of(tools().emitSizeTValue(0))
                                              (idxValue2);
    addr = builder().CreateGEP(typedArray, argv);
  }

  ArraySliceAccessData retv;
  retv.fArray = arrayAtom;
  retv.fAddr = addr;

  return retv;
}


llvm::Value*
CodegenApply::emitArraySliceAccess(const ApplyNode* node) const
{
#if defined(IS_DEBUG)
  const NodeList& args = node->children();
  hr_assert(args.size() == 2);
  hr_assert(args[0]->type().isArray());
#endif

  ArraySliceAccessData arrayAccces = emitArraySliceAddress(node);
  llvm::Value* arraySliceVal = builder().CreateLoad(arrayAccces.fAddr);

  return tools().emitPackCode(node->dstType(), node->typeConv(),
                              arraySliceVal, node->type());
}


llvm::Value*
CodegenApply::emitArraySliceSet(const ApplyNode* node) const
{
  const NodeList& args = node->children();
  hr_assert(args.size() == 3);
  hr_assert(args[0]->type().isArray());

  auto newVal = tools().wrapLoad(generator().codegenNode(*args[2]));

  ArraySliceAccessData arrayAccces = emitArraySliceAddress(node);

  llvm::Type* ptrType = types().getType(args[0]->type().arrayBaseType())->getPointerTo();
  llvm::Value* ptrAddr = builder().CreatePointerCast(arrayAccces.fAddr, ptrType);

  builder().CreateStore(newVal, ptrAddr);

  // the lang|slice! method returns the array itself, not the new set value.
  return arrayAccces.fArray;
}


llvm::Value*
CodegenApply::emitArrayNumItems(const ApplyNode* node) const
{
#if defined(IS_DEBUG)
  const NodeList& args = node->children();
  hr_assert(args.size() == 1);
  hr_assert(args[0]->type().isArray());
#endif

  llvm::Value* numItems = emitArraySize(node);
  llvm::Value* numItemsVal = builder().CreateLoad(numItems);

  return tools().wrapLoad(tools().makeIntAtom(numItemsVal, CodegenTools::kAtomInt32));
}
