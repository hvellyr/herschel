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
  : CodeGeneratorProxy(generator)
{
}


//------------------------------------------------------------------------------

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


const llvm::StructType*
CodegenTypeUtils::getTypeSlotPairType() const
{
  llvm::StringRef typeName("struct.TypeSlotPair");

  const llvm::StructType* typeSlotPairType =
    (const llvm::StructType*)module()->getTypeByName(typeName);
  if (typeSlotPairType == NULL) {
    typeSlotPairType =
      llvm::StructType::get(context(),
                            newLlvmTypeVector(llvm::Type::getInt8PtrTy(context()),
                                              llvm::Type::getInt32Ty(context())),
                            !K(isPacked));

    module()->addTypeName(typeName, typeSlotPairType);
  }

  return typeSlotPairType;
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
CodegenTypeUtils::getSizeTTy() const
{
  if (fGenerator->is64Bit())
    return llvm::Type::getInt64Ty(context());
  else
    return llvm::Type::getInt32Ty(context());
}


const llvm::Type*
CodegenTypeUtils::getArrayPayloadType() const
{
  llvm::StringRef typeName("struct.ArrayPayload");

  const llvm::Type* arrayPLType = module()->getTypeByName(typeName);
  if (arrayPLType == NULL) {
    arrayPLType =
      llvm::StructType::get(context(),
                            newLlvmTypeVector(getSizeTTy(),
                                              llvm::Type::getInt8PtrTy(context())),
                            !K(isPacked));

    module()->addTypeName(typeName, arrayPLType);
  }

  return arrayPLType;
}


const llvm::Type*
CodegenTypeUtils::getType(const Type& type) const
{
  String typeId = type.typeId();

  // tyerror(type, "Other type");
  // fprintf(stderr, " ---> %s\n", (const char*)StrHelper(typeId));

  if (typeId == Names::kClangIntTypeName) {
    return llvm::Type::getInt32Ty(context());
  }
  else if (typeId == Names::kClangCharTypeName) {
    return llvm::Type::getInt8Ty(context());
  }

  else if (typeId == Names::kInt8TypeName) {
    return llvm::Type::getInt8Ty(context());
  }
  else if (typeId == Names::kInt16TypeName) {
    return llvm::Type::getInt16Ty(context());
  }
  else if (typeId == Names::kInt64TypeName) {
    return llvm::Type::getInt64Ty(context());
  }

  else if (typeId == Names::kUInt8TypeName) {
    return llvm::Type::getInt8Ty(context());
  }
  else if (typeId == Names::kUInt16TypeName) {
    return llvm::Type::getInt16Ty(context());
  }
  else if (typeId == Names::kUInt64TypeName) {
    return llvm::Type::getInt64Ty(context());
  }

  else if (typeId == Names::kCharTypeName) {
    return llvm::Type::getInt32Ty(context());
  }
  else if (typeId == Names::kBoolTypeName) {
    return llvm::Type::getInt1Ty(context());
  }

  else if (typeId == Names::kFloat32TypeName) {
    return llvm::Type::getFloatTy(context());
  }
  else if (typeId == Names::kFloat64TypeName) {
    return llvm::Type::getDoubleTy(context());
  }
  // else if (typeId == Names::kFloat128TypeName) {
  //   return llvm::Type::getInt1Ty(context());
  // }

  //-------------------- array types

  else if (typeId == String("lang|Int32[]") ||
           typeId == String("lang|UInt32[]"))
  {
    return getAtomType();
  }
  else if (typeId == String("lang|Char[]")) {
    return getAtomType();
  }

  const TypeProperty& prop = type.typeProperty(!K(mustExist));
  if (prop.isValid())
    return prop.getLLVMType();

  return getAtomType();
}


size_t
CodegenTypeUtils::getSlotSize(const Type& type) const
{
  String typeId = type.typeId();
  if (typeId == Names::kClangIntTypeName)
    // TODO: shouldn't this be 32/64 depending on host platform?
    return 4; // llvm::Type::getInt32Ty(context());

  else if (typeId == Names::kClangCharTypeName)
    return 1; // llvm::Type::getInt8Ty(context());


  else if (typeId == Names::kInt8TypeName)
    return 1; // llvm::Type::getInt8Ty(context());

  else if (typeId == Names::kInt16TypeName)
    return 2; // llvm::Type::getInt16Ty(context());

  else if (typeId == Names::kInt64TypeName)
    return 8; // llvm::Type::getInt64Ty(context());


  else if (typeId == Names::kUInt8TypeName)
    return 1; // llvm::Type::getInt8Ty(context());

  else if (typeId == Names::kUInt16TypeName)
    return 2; // llvm::Type::getInt16Ty(context());

  else if (typeId == Names::kUInt64TypeName)
    return 8; // llvm::Type::getInt64Ty(context());


  else if (typeId == Names::kCharTypeName)
    return 4; // llvm::Type::getInt32Ty(context());

  else if (typeId == Names::kBoolTypeName)
    return 1; // llvm::Type::getInt1Ty(context());


  else if (typeId == Names::kFloat32TypeName)
    return 4; // llvm::Type::getFloatTy(context());

  else if (typeId == Names::kFloat64TypeName)
    return 8; // llvm::Type::getDoubleTy(context());

  // else if (typeId == Names::kFloat128TypeName)
  //   return llvm::Type::getInt1Ty(context());
  // }

  const TypeProperty& prop = type.typeProperty(!K(mustExist));
  if (prop.isValid())
    return prop.getSlotSize();

  // all other types are atoms
  const llvm::StructLayout* layout = generator()->targetData()
    ->getStructLayout((const llvm::StructType*)getAtomType());
  return layout->getSizeInBytes();
}


