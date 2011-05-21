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
  : fGenerator(generator)
{
}


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


void
CodegenTools::assignAtom(llvm::Value* src, llvm::Value* dst)
{
  llvm::IRBuilder<>& builder = fGenerator->builder();
#if 0
  // this way is probably a tick slower on an i386, but the memcpy approach
  // below does not work.  With that we get some strange byte ordering
  // problems...  Performance wise it does not make any difference on x86_64
  // (with full optimization).

  llvm::Value* dst_pl = builder.CreateStructGEP(dst, 1);
  llvm::Value* dst_ty = builder.CreateStructGEP(dst, 0);

  llvm::Value* src_pl = builder.CreateStructGEP(src, 1);
  llvm::Value* src_ty = builder.CreateStructGEP(src, 0);

  builder.CreateStore(builder.CreateLoad(src_ty), dst_ty);
  builder.CreateStore(builder.CreateLoad(src_pl), dst_pl);

#else

  const llvm::Type* dstBasePtr = llvm::Type::getInt8PtrTy(fGenerator->fContext);
  llvm::Value* dst2 = builder.CreateBitCast(dst, dstBasePtr, "dst_tmp");

  const llvm::Type *srcBasePtr = llvm::Type::getInt8PtrTy(fGenerator->fContext);
  llvm::Value* src2 = builder.CreateBitCast(src, srcBasePtr, "src_tmp");

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
  argv.push_back(llvm::ConstantInt::getFalse(fGenerator->fContext));


  builder.CreateCall(getMemCpyFn(dst2->getType(), src2->getType(),
                                 llvm::Type::getInt32Ty(fGenerator->fContext)),
                     argv.begin(), argv.end());
#endif
}

