/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.
*/

#include "codegen.h"
#include "apt.h"
#include "log.h"
#include "properties.h"
#include "symbol.h"
#include "xmlout.h"
#include "predefined.h"

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


//------------------------------------------------------------------------------

CodegenTypeUtils::CodegenTypeUtils(CodeGenerator* generator)
  : fGenerator(generator)
{
}


//------------------------------------------------------------------------------

llvm::LLVMContext&
CodegenTypeUtils::context() const
{
  return fGenerator->fContext;
}


llvm::IRBuilder<>&
CodegenTypeUtils::builder() const
{
  return fGenerator->fBuilder;
}


llvm::Module*
CodegenTypeUtils::module() const
{
  return fGenerator->fModule;
}


static std::vector<const llvm::Type*>
newLlvmTypeVector(const llvm::Type* ty)
{
  std::vector<const llvm::Type*> v;
  v.push_back(ty);
  return v;
}


static std::vector<const llvm::Type*>
newLlvmTypeVector(const llvm::Type* ty1, const llvm::Type* ty2)
{
  std::vector<const llvm::Type*> v;
  v.push_back(ty1);
  v.push_back(ty2);
  return v;
}


static std::vector<const llvm::Type*>
newLlvmTypeVector(const llvm::Type* ty1, const llvm::Type* ty2,
                  const llvm::Type* ty3)
{
  std::vector<const llvm::Type*> v;
  v.push_back(ty1);
  v.push_back(ty2);
  v.push_back(ty3);
  return v;
}


const llvm::Type*
CodegenTypeUtils::getAtomType() const
{
  llvm::StringRef typeName("struct.ATOM");

  const llvm::Type* atomType = module()->getTypeByName(typeName);
  if (atomType == NULL) {
    const llvm::Type* payloadType = NULL;
    const llvm::Type* tagType = NULL;

    if (fGenerator->is64Bit()) {
      payloadType = llvm::Type::getInt64Ty(context());
      tagType = llvm::Type::getInt64Ty(context());
    }
    else {
      payloadType = llvm::Type::getInt32Ty(context());
      tagType = llvm::Type::getInt32Ty(context());
    }

    const llvm::StructType* payloadStruct =
      llvm::StructType::get(context(), newLlvmTypeVector(payloadType), false);

    atomType =
      llvm::StructType::get(context(), newLlvmTypeVector(tagType, payloadStruct),
                            false);
    module()->addTypeName(llvm::StringRef("union.AtomPayload"), payloadStruct);
    module()->addTypeName(typeName, atomType);
  }

  return atomType;
}


const llvm::Type*
CodegenTypeUtils::getTagIdType() const
{
  if (fGenerator->is64Bit())
    return llvm::Type::getInt64Ty(context());
  else
    return llvm::Type::getInt32Ty(context());
}

const llvm::Type*
CodegenTypeUtils::getTypeType() const
{
  // TODO
  return llvm::Type::getInt8PtrTy(context());
}


const llvm::Type*
CodegenTypeUtils::getTypeSlotPairType() const
{
  // TODO
  return llvm::Type::getInt8PtrTy(context());
}


const llvm::Type*
CodegenTypeUtils::getGenericFuncType() const
{
  // TODO
  return llvm::Type::getInt8PtrTy(context());
}


const llvm::Type*
CodegenTypeUtils::getMethodType() const
{
  llvm::StringRef typeName("struct.Method");

  const llvm::Type* methodType = module()->getTypeByName(typeName);
  if (methodType == NULL) {
    methodType =
      llvm::StructType::get(context(),
                            newLlvmTypeVector(getTypeType()->getPointerTo(),
                                              llvm::Type::getInt32Ty(context()),
                                              llvm::Type::getInt8PtrTy(context())),
                            !K(isPacked))->getPointerTo();

    module()->addTypeName(typeName, methodType);
  }

  return methodType;
}


const llvm::Type*
CodegenTypeUtils::getType(const Type& type) const
{
  if (type.typeName() == String("clang|int")) {
    return llvm::Type::getInt32Ty(context());
  }
  else if (type.typeName() == String("clang|char")) {
    return llvm::Type::getInt8Ty(context());
  }

  else if (type.typeName() == String("lang|Int8")) {
    return llvm::Type::getInt8Ty(context());
  }
  else if (type.typeName() == String("lang|Int16")) {
    return llvm::Type::getInt16Ty(context());
  }
  else if (type.typeName() == String("lang|Int32")) {
    return llvm::Type::getInt32Ty(context());
  }
  else if (type.typeName() == String("lang|Int64")) {
    return llvm::Type::getInt64Ty(context());
  }

  else if (type.typeName() == String("lang|UInt8")) {
    return llvm::Type::getInt8Ty(context());
  }
  else if (type.typeName() == String("lang|UInt16")) {
    return llvm::Type::getInt16Ty(context());
  }
  else if (type.typeName() == String("lang|UInt32")) {
    return llvm::Type::getInt32Ty(context());
  }
  else if (type.typeName() == String("lang|UInt64")) {
    return llvm::Type::getInt64Ty(context());
  }

  else if (type.typeName() == String("lang|Char")) {
    return llvm::Type::getInt32Ty(context());
  }
  else if (type.typeName() == String("lang|Bool")) {
    return llvm::Type::getInt1Ty(context());
  }

  else if (type.typeName() == String("lang|Float32")) {
    return llvm::Type::getFloatTy(context());
  }
  else if (type.typeName() == String("lang|Float64")) {
    return llvm::Type::getDoubleTy(context());
  }
  // else if (type.typeName() == String("lang|Float128")) {
  //   return llvm::Type::getInt1Ty(context());
  // }

  return getAtomType();
}
