/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

//----------------------------------------------------------------------------

#include "codegen.h"
#include "codegen-init.h"
#include "codegen-types.h"
#include "apt.h"
#include "log.h"
#include "properties.h"
#include "symbol.h"
#include "xmlout.h"
#include "predefined.h"

#include <vector>
#include <typeinfo>

#include "llvm/Analysis/Verifier.h"
#include "llvm/DerivedTypes.h"
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
#include "llvm/System/Host.h"
#include "llvm/GlobalVariable.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Intrinsics.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetRegistry.h"
//#include "llvm/DIBuilder.h"


using namespace herschel;


#include "codegen-tools.h"


CodegenTools::CodegenTools(CodeGenerator* generator)
  : CodeGeneratorProxy(generator)
{
}


//------------------------------------------------------------------------------

llvm::Value*
CodegenTools::wrapLoad(llvm::Value* val)
{
  if (val != NULL) {
    if (llvm::AllocaInst::classof(val) ||
        llvm::GlobalValue::classof(val))
      return builder().CreateLoad(val);
  }
  return val;
}


llvm::Value*
CodegenTools::makeInt32Atom(int val)
{
  return makeIntAtom(llvm::ConstantInt::get(fGenerator->context(),
                                            llvm::APInt(32, val, true)),
                     kAtomInt32);
}


llvm::Value*
CodegenTools::makeIntAtom(llvm::Value* val, Typeid atomTypeId)
{
  llvm::Function *curFunction = builder().GetInsertBlock()->getParent();
  llvm::AllocaInst* atom = createEntryBlockAlloca(curFunction, String("int"),
                                                  fGenerator->fTypes->getAtomType());

  setAtom(atom, atomTypeId, val);

  return atom;
}


llvm::Value*
CodegenTools::makeBoolAtom(llvm::Value* val)
{
  llvm::Function *curFunction = builder().GetInsertBlock()->getParent();
  llvm::AllocaInst* atom = createEntryBlockAlloca(curFunction, String("bool"),
                                                  fGenerator->fTypes->getAtomType());

  setAtom(atom, kAtomBool, val);

  return atom;
}


llvm::Value*
CodegenTools::makeBoolAtom(bool val)
{
  if (val)
    return makeBoolAtom(llvm::ConstantInt::getTrue(fGenerator->context()));
  else
    return makeBoolAtom(llvm::ConstantInt::getFalse(fGenerator->context()));
}


llvm::Value*
CodegenTools::makeKeywordAtom(const String& keyword)
{
  llvm::Function *curFunction = builder().GetInsertBlock()->getParent();
  llvm::AllocaInst* atom = createEntryBlockAlloca(curFunction, keyword + "_kw",
                                                  fGenerator->fTypes->getAtomType());

  setAtom(atom, kAtomKeyword,
          builder().CreateLoad(fGenerator->fInitializer->registerKeyword(keyword)));

  return atom;
}


//------------------------------------------------------------------------------

llvm::Function*
CodegenTools::getIntrinsic(unsigned int iid,
                           const llvm::Type** tys, unsigned int numTys)
{
  return llvm::Intrinsic::getDeclaration(fGenerator->fModule,
                                         (llvm::Intrinsic::ID)iid, tys, numTys);
}


llvm::Function*
CodegenTools::getMemCpyFn(const llvm::Type* dstType,
                          const llvm::Type* srcType,
                          const llvm::Type* sizeType)
{
  const llvm::Type* argTypes[3] = { dstType, srcType, sizeType };
  return getIntrinsic(llvm::Intrinsic::memcpy, argTypes, 3);
}


//------------------------------------------------------------------------------

void
CodegenTools::setAtom(llvm::AllocaInst* atom, Typeid typid, llvm::Value* value)
{
  llvm::Value* typidSlot = builder().CreateStructGEP(atom, 0);
  llvm::Value* typeIdValue = NULL;
  if (fGenerator->is64Bit())
    typeIdValue = llvm::ConstantInt::get(fGenerator->context(),
                                         llvm::APInt(64, (int)typid, !K(isSigned)));
  else
    typeIdValue = llvm::ConstantInt::get(fGenerator->context(),
                                         llvm::APInt(32, (int)typid, !K(isSigned)));
  
  builder().CreateStore(typeIdValue, typidSlot);

  llvm::Value* payload = builder().CreateStructGEP(atom, 1);
  llvm::Value* slot = builder().CreateStructGEP(payload, 0);

  if (typid == kAtomBool) {
    const llvm::Type *dstBasePtr = llvm::Type::getInt1PtrTy(fGenerator->context());
    slot = builder().CreateBitCast(slot, dstBasePtr, "tmp");
    builder().CreateStore(value, slot);
  }
  else if (typid == kAtomInt32) {
    llvm::Value* val = ( fGenerator->is64Bit()
                         ? builder().CreateIntCast(value,
                                                   llvm::Type::getInt64Ty(context()),
                                                   K(isSigned),
                                                   "tmp")
                         : value );
    builder().CreateStore(val, slot);
  }
  else if (typid == kAtomKeyword) {
    builder().CreateStore(createCastPtrToNativeInt(value), slot);
  }
  else
    builder().CreateStore(value, slot);
}


void
CodegenTools::assignAtom(llvm::Value* src, llvm::Value* dst)
{
#if 1
  // this way is probably a tick slower on an i386, but the memcpy approach
  // below does not work.  With that we get some strange byte ordering
  // problems...  Performance wise it does not make any difference on x86_64
  // (with full optimization).

  llvm::Value* dst_pl = builder().CreateStructGEP(dst, 1);
  llvm::Value* dst_ty = builder().CreateStructGEP(dst, 0);

  llvm::Value* src_pl = builder().CreateStructGEP(src, 1);
  llvm::Value* src_ty = builder().CreateStructGEP(src, 0);

  builder().CreateStore(builder().CreateLoad(src_ty), dst_ty);
  builder().CreateStore(builder().CreateLoad(src_pl), dst_pl);

#else

  const llvm::Type* dstBasePtr = llvm::Type::getInt8PtrTy(context());
  llvm::Value* dst2 = builder().CreateBitCast(dst, dstBasePtr, "dst_tmp");

  const llvm::Type *srcBasePtr = llvm::Type::getInt8PtrTy(context());
  llvm::Value* src2 = builder().CreateBitCast(src, srcBasePtr, "src_tmp");

  std::vector<llvm::Value*> argv;
  argv.push_back(dst2);
  argv.push_back(src2);
  // number


  const llvm::StructLayout* layout = fGenerator->fTargetData
    ->getStructLayout((const llvm::StructType*)fGenerator->fTypes->getAtomType());

  argv.push_back(llvm::ConstantInt::get(fGenerator->fContext,
                                        llvm::APInt(32, layout->getSizeInBytes(),
                                                    K(isSigned))));
  // align
  argv.push_back(llvm::ConstantInt::get(fGenerator->fContext,
                                        llvm::APInt(32, layout->getAlignment(),
                                                    K(isSigned))));
  // is volatile
  argv.push_back(llvm::ConstantInt::getFalse(context()));


  builder().CreateCall(getMemCpyFn(dst2->getType(), src2->getType(),
                                   llvm::Type::getInt32Ty(context())),
                       argv.begin(), argv.end());
#endif
}


const char*
CodegenTools::getConvFuncNameByType(const Type& type) const
{
  if (type.typeName() == String("lang|Int32"))
    return "atom_2_int32";
  else if (type.typeName() == String("lang|Int64"))
    return "atom_2_int64";
  else if (type.typeName() == String("lang|Int16"))
    return "atom_2_int16";
  else if (type.typeName() == String("lang|Int8"))
    return "atom_2_int8";
  else if (type.typeName() == String("lang|UInt32"))
    return "atom_2_uint32";
  else if (type.typeName() == String("lang|UInt64"))
    return "atom_2_uint64";
  else if (type.typeName() == String("lang|UInt16"))
    return "atom_2_uint16";
  else if (type.typeName() == String("lang|UInt8"))
    return "atom_2_uint8";
  else if (type.typeName() == String("lang|Float32"))
    return "atom_2_float32";
  else if (type.typeName() == String("lang|Float64"))
    return "atom_2_float64";
  else if (type.typeName() == String("lang|Char"))
    return "atom_2_char";
  else if (type.typeName() == String("lang|Bool"))
    return "atom_2_bool";

  else if (type.typeName() == String("lang|Keyword"))
    return "atom_2_keyword";

  if (type.typeName() == String("clang|int")) // TODO
    return "atom_2_int32";

  hr_invalid((const char*)StrHelper(String("unhandled type: ") + type.typeId()));
  return NULL;
}


const llvm::Type*
CodegenTools::getConvTypeByType(const Type& type) const
{
  if (type.typeName() == String("lang|Keyword"))
    return llvm::Type::getInt8PtrTy(context());

  return types()->getType(type);
}


llvm::Value*
CodegenTools::makeTypeCastAtomToPlain(llvm::Value* val, const Type& dstType) const
{
  const char* funcName = getConvFuncNameByType(dstType);

  llvm::Function* convFunc = module()->getFunction(llvm::StringRef(funcName));
  if (convFunc == NULL) {
    std::vector<const llvm::Type*> sign;
    sign.push_back(types()->getAtomType());

    llvm::FunctionType *ft = llvm::FunctionType::get(getConvTypeByType(dstType),
                                                     sign,
                                                     false);

    convFunc = llvm::Function::Create(ft,
                                      llvm::Function::ExternalLinkage,
                                      llvm::Twine(funcName),
                                      module());
  }

  std::vector<llvm::Value*> argv;
  argv.push_back(val);
  return builder().CreateCall(convFunc, argv.begin(), argv.end(), "calltmp");
}


llvm::Value*
CodegenTools::emitPackCode(const Type& dstType, TypeConvKind convKind,
                           llvm::Value* value,
                           const Type& valType)
{
  if (dstType.isDef()) {
    // fprintf(stderr, "-----------------------\n");
    // fprintf(stderr, "type conv: %d\n", convKind);
    // tyerror(dstType, "dstType");
    // tyerror(valType, "valType");
    // fprintf(stderr, "Value to emit: "); value->dump();

    switch (convKind) {
    case kNoConv:
      return value;
    case kAtom2PlainConv:
      return makeTypeCastAtomToPlain(value, dstType);
    case kPlain2AtomConv:
      if (valType.typeName() == String("lang|Int32"))
        return wrapLoad(makeIntAtom(value, CodegenTools::kAtomInt32));
      else if (valType.typeName() == String("lang|Bool"))
        return wrapLoad(makeBoolAtom(value));
      // TODO
      //return value;

    case kTypeCheckConv:
      // fprintf(stderr, "Not implemented yet\n");
      // hr_invalid("not implemented yet");
      return value;
    }
  }

  return value;
}


llvm::AllocaInst*
CodegenTools::createEntryBlockAlloca(llvm::Function *func, const String& name,
                                     const llvm::Type* type)
{
  llvm::IRBuilder<> tmp(&func->getEntryBlock(), func->getEntryBlock().begin());
  return tmp.CreateAlloca(type, 0, llvm::Twine(name));
}


llvm::Value*
CodegenTools::createCastPtrToNativeInt(llvm::Value* value) const
{
  return ( fGenerator->is64Bit()
           ? builder().CreatePtrToInt(value,
                                      llvm::Type::getInt64Ty(context()))
           : builder().CreatePtrToInt(value,
                                      llvm::Type::getInt32Ty(context())) );
}

