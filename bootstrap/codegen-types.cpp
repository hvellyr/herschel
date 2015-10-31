/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "codegen.h"
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


//------------------------------------------------------------------------------

CodegenTypeUtils::CodegenTypeUtils(CodeGenerator* generator)
  : CodeGeneratorProxy(generator)
{
}


//------------------------------------------------------------------------------

llvm::Type*
CodegenTypeUtils::getAtomPayloadType() const
{
  llvm::StringRef typeName("union.AtomPayload");

  llvm::StructType* atomPayloadType =
    (llvm::StructType*)module()->getTypeByName(typeName);
  if (!atomPayloadType) {
    llvm::Type* payloadType = nullptr;

    if (fGenerator->is64Bit()) {
      payloadType = llvm::Type::getInt64Ty(context());
    }
    else {
      payloadType = llvm::Type::getInt32Ty(context());
    }

    auto atomPayloadStructType = llvm::StructType::create(context(), typeName);
    atomPayloadStructType->setBody(std::vector<llvm::Type*>{payloadType},
                                   !K(isPacked));
    atomPayloadType = atomPayloadStructType;
  }

  return atomPayloadType;
}


llvm::Type*
CodegenTypeUtils::getAtomType() const
{
  llvm::StringRef typeName("struct.ATOM");

  llvm::Type* atomType = module()->getTypeByName(typeName);
  if (!atomType) {
    llvm::Type* tagType = nullptr;

    if (fGenerator->is64Bit()) {
      tagType = llvm::Type::getInt64Ty(context());
    }
    else {
      tagType = llvm::Type::getInt32Ty(context());
    }

    auto atomStructType = llvm::StructType::create(context(), typeName);
    atomStructType->setBody(std::vector<llvm::Type*>{ tagType,
                                                      getAtomPayloadType() },
                            !K(isPacked));
    atomType = atomStructType;
  }

  return atomType;
}


llvm::Type*
CodegenTypeUtils::getTagIdType() const
{
  if (fGenerator->is64Bit())
    return llvm::Type::getInt64Ty(context());
  else
    return llvm::Type::getInt32Ty(context());
}


llvm::Type*
CodegenTypeUtils::getTypeType() const
{
  // TODO
  return llvm::Type::getInt8PtrTy(context());
}


llvm::StructType*
CodegenTypeUtils::getTypeSlotPairType() const
{
  llvm::StringRef typeName("struct.TypeSlotPair");

  llvm::StructType* typeSlotPairType =
    (llvm::StructType*)module()->getTypeByName(typeName);
  if (!typeSlotPairType) {
    auto typeSlotPairStructType = llvm::StructType::create(context(), typeName);
    typeSlotPairStructType->setBody(
      std::vector<llvm::Type*>{ llvm::Type::getInt8PtrTy(context()),
                                llvm::Type::getInt32Ty(context()) },
      !K(isPacked));
    typeSlotPairType = typeSlotPairStructType;
  }

  return typeSlotPairType;
}


llvm::Type*
CodegenTypeUtils::getGenericFuncType() const
{
  // TODO
  return llvm::Type::getInt8PtrTy(context());
}


llvm::Type*
CodegenTypeUtils::getMethodStructType() const
{
  llvm::StringRef typeName("struct.Method");

  llvm::StructType* methodStructType = module()->getTypeByName(typeName);
  if (!methodStructType) {
    methodStructType = llvm::StructType::create(context(), typeName);
    methodStructType->setBody(
      std::vector<llvm::Type*>{ getTypeType()->getPointerTo(),
                                llvm::Type::getInt32Ty(context()),
                                llvm::Type::getInt8PtrTy(context()) },
      !K(isPacked));
  }

  return methodStructType;
}


llvm::Type*
CodegenTypeUtils::getMethodType() const
{
  return getMethodStructType()->getPointerTo();
}


llvm::Type*
CodegenTypeUtils::getSizeTTy() const
{
  if (fGenerator->is64Bit())
    return llvm::Type::getInt64Ty(context());
  else
    return llvm::Type::getInt32Ty(context());
}


llvm::Type*
CodegenTypeUtils::getArrayPayloadType() const
{
  llvm::StringRef typeName("struct.ArrayPayload");

  llvm::Type* arrayPLType = module()->getTypeByName(typeName);
  if (!arrayPLType) {
    auto arrayPLStructType = llvm::StructType::create(context(), typeName);
    arrayPLStructType->setBody(std::vector<llvm::Type*>{ getSizeTTy(),
                                                         llvm::Type::getInt8PtrTy(context()) },
                               !K(isPacked));
    arrayPLType = arrayPLStructType;
  }

  return arrayPLType;
}


llvm::Type*
CodegenTypeUtils::getType(const Type& type) const
{
  if (type.isArray())
  {
    return getAtomType();
  }

  const TypeProperty& prop = type.typeProperty(!K(mustExist));
  if (prop.isValid())
    return prop.getLLVMType(this);

  return getAtomType();
}


size_t
CodegenTypeUtils::getAtomTypeSize() const
{
  // all other types are atoms
  const llvm::StructLayout* layout = generator()->dataLayout()
    ->getStructLayout((llvm::StructType*)getAtomType());
  return layout->getSizeInBytes();
}


size_t
CodegenTypeUtils::getSlotSize(const Type& type) const
{
  const TypeProperty& prop = type.typeProperty(!K(mustExist));
  if (prop.isValid())
    return prop.getSlotSize(this);

  // all other types are atoms
  return getAtomTypeSize();
}
