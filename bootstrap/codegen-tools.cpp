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
#include "typeprops.h"
#include "utils.h"

#include <vector>
#include <typeinfo>

#include "llvm/Analysis/Verifier.h"
#include "llvm/DerivedTypes.h"
#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/PassManager.h"
#include "llvm/Support/IRBuilder.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Intrinsics.h"


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
CodegenTools::makeFloatAtom(llvm::Value* val, Typeid atomTypeId)
{
  llvm::Function *curFunction = builder().GetInsertBlock()->getParent();
  llvm::AllocaInst* atom = createEntryBlockAlloca(curFunction, String("float"),
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
          builder().CreateLoad(initializer()->registerKeyword(keyword)));

  return atom;
}


llvm::Value*
CodegenTools::makeStringAtom(const String& str)
{
  String funcnm = String("allocate_string");

  llvm::Function *allocFunc = module()->getFunction(llvm::StringRef(funcnm));
  if (allocFunc == NULL) {
    // void h7_allocate_array(ATOM* instance, Type* ty, size_t items);
    llvm::FunctionType *ft = llvm::FunctionType::get(
      llvm::Type::getVoidTy(context()),
      vector_of<const llvm::Type*>(types()->getAtomType()->getPointerTo())
                                  (llvm::Type::getInt8PtrTy(context())),
      !K(isVarArg));

    allocFunc = llvm::Function::Create(ft,
                                       llvm::Function::ExternalLinkage,
                                       llvm::Twine(StrHelper(funcnm)),
                                       module());
  }

  llvm::Function* curFunction = builder().GetInsertBlock()->getParent();
  llvm::AllocaInst* retv = tools()->createEntryBlockAlloca(curFunction,
                                                           String("string_retv"),
                                                           types()->getAtomType());
  hr_assert(retv != NULL);

  std::vector<llvm::Value*> argv =
    vector_of<llvm::Value*>(retv)
                           (builder().CreateGlobalStringPtr(StrHelper(str),
                                                            llvm::Twine(StrHelper(str + "_str"))));

  builder().CreateCall(allocFunc, argv.begin(), argv.end());
  return retv;
}


llvm::Value*
CodegenTools::makeCharAtom(llvm::Value* val)
{
  llvm::Function *curFunction = builder().GetInsertBlock()->getParent();
  llvm::AllocaInst* atom = createEntryBlockAlloca(curFunction, String("char"),
                                                  fGenerator->fTypes->getAtomType());

  setAtom(atom, kAtomChar, val);

  return atom;
}


llvm::Value*
CodegenTools::makeCharAtom(Char val)
{
  return makeCharAtom(llvm::ConstantInt::get(fGenerator->context(),
                                             llvm::APInt(32, val, !K(issigned))));
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

llvm::Value*
CodegenTools::emitTypeId(Typeid typid) const
{
  if (fGenerator->is64Bit())
    return llvm::ConstantInt::get(fGenerator->context(),
                                  llvm::APInt(64, (int)typid, !K(isSigned)));
  else
    return llvm::ConstantInt::get(fGenerator->context(),
                                  llvm::APInt(32, (int)typid, !K(isSigned)));
}


void
CodegenTools::setAtom(llvm::AllocaInst* atom, Typeid typid, llvm::Value* value)
{
  llvm::Value* typidSlot = builder().CreateStructGEP(atom, 0);
  llvm::Value* typeIdValue = emitTypeId(typid);

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
  else if (typid == kAtomUInt32) {
    llvm::Value* val = ( fGenerator->is64Bit()
                         ? builder().CreateIntCast(value,
                                                   llvm::Type::getInt64Ty(context()),
                                                   !K(isSigned),
                                                   "tmp")
                         : value );
    builder().CreateStore(val, slot);
  }
  else if (typid == kAtomInt16 || typid == kAtomInt8) {
    llvm::Value* val = builder().CreateIntCast(value,
                                               ( fGenerator->is64Bit()
                                                 ? llvm::Type::getInt64Ty(context())
                                                 : llvm::Type::getInt32Ty(context()) ),
                                               K(isSigned),
                                               "tmp");
    builder().CreateStore(val, slot);
  }
  else if (typid == kAtomUInt16 || typid == kAtomUInt8) {
    llvm::Value* val = builder().CreateIntCast(value,
                                               ( fGenerator->is64Bit()
                                                 ? llvm::Type::getInt64Ty(context())
                                                 : llvm::Type::getInt32Ty(context()) ),
                                               !K(isSigned),
                                               "tmp");
    builder().CreateStore(val, slot);
  }
  else if (typid == kAtomInt64 || typid == kAtomUInt64) {
    builder().CreateStore(value, slot);
  }
  else if (typid == kAtomFloat32) {
    hr_invalid("code missing");
  }

  else if (typid == kAtomChar) {
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

  const llvm::StructLayout* layout = fGenerator->fTargetData
    ->getStructLayout((const llvm::StructType*)fGenerator->fTypes->getAtomType());

  std::vector<llvm::Value*> argv =
    vector_of(dst2)
             (src2)
             (llvm::ConstantInt::get(fGenerator->fContext,
                                     llvm::APInt(32, layout->getSizeInBytes(),
                                                 K(isSigned))))
             // align
             (llvm::ConstantInt::get(fGenerator->fContext,
                                     llvm::APInt(32, layout->getAlignment(),
                                                 K(isSigned))))
             // is volatile
             (llvm::ConstantInt::getFalse(context()));


  builder().CreateCall(getMemCpyFn(dst2->getType(), src2->getType(),
                                   llvm::Type::getInt32Ty(context())),
                       argv.begin(), argv.end());
#endif
}


const char*
CodegenTools::getConvFuncNameByType(const Type& type) const
{
  const TypeProperty& prop = type.typeProperty();
  return prop.convFuncName();
}


const llvm::Type*
CodegenTools::getConvTypeByType(const Type& type) const
{
  if (type.typeId() == Names::kKeywordTypeName)
    return llvm::Type::getInt8PtrTy(context());

  return types()->getType(type);
}


llvm::Value*
CodegenTools::makeTypeCastAtomToPlain(llvm::Value* val, const Type& dstType) const
{
  const char* funcName = getConvFuncNameByType(dstType);

  llvm::Function* convFunc = module()->getFunction(llvm::StringRef(funcName));
  if (convFunc == NULL) {
    llvm::FunctionType *ft = llvm::FunctionType::get(getConvTypeByType(dstType),
                                                     vector_of(types()->getAtomType()),
                                                     false);

    convFunc = llvm::Function::Create(ft,
                                      llvm::Function::ExternalLinkage,
                                      llvm::Twine(funcName),
                                      module());
  }

  std::vector<llvm::Value*> argv = vector_of(val);
  return builder().CreateCall(convFunc, argv.begin(), argv.end());
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
      {
         const TypeProperty& prop = valType.typeProperty(!K(mustExist));
         llvm::Value* retval = prop.isValid()
           ? prop.emitPackCode(this, value)
           : value;
         return retval;
      }

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



//----------------------------------------------------------------------------------------

llvm::Value*
CodegenTools::emitSizeTValue(size_t value) const
{
  if (fGenerator->is64Bit())
    return llvm::ConstantInt::get(fGenerator->context(),
                                  llvm::APInt(64,
                                              value,
                                              !K(isSigned)));
  else
    return llvm::ConstantInt::get(fGenerator->context(),
                                  llvm::APInt(32,
                                              value,
                                              !K(isSigned)));
}


llvm::Value*
CodegenTools::coerceIntOperand(llvm::Value* value, const Type& dstType) const
{
  return builder().CreateIntCast(value,
                                 types()->getType(dstType),
                                 dstType.isSigned());
}


llvm::Value*
CodegenTools::convertToPlainInt(llvm::Value* value,
                                const Type& dstType,
                                TypeConvKind typeConv) const
{
  switch (typeConv) {
  case kNoConv:
    return coerceIntOperand(value, dstType);
  case kAtom2PlainConv:
    return makeTypeCastAtomToPlain(value, dstType);
  case kPlain2AtomConv:
  case kTypeCheckConv:
    hr_invalid("");
  }

  return NULL;
}


