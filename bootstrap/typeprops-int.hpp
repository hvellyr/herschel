/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include "typeenum.hpp"
#include "typeprops.hpp"
// #include "codegen-tools.hpp"

// #include "llvm/IR/DerivedTypes.h"
// #include "llvm/IR/LLVMContext.h"


namespace herschel {

template <typename EnumMaker, int bitsize, bool issigned>  //, int atominttype>
class BaseIntTypeProperty : public TypeProperty {
  // llvm::Value* emitPackCode(CodegenTools* tools, llvm::Value* value) const override
  // {
  //   return tools->makeIntAtom(value, (CodegenTools::Typeid)atominttype);
  // }

  // size_t getSlotSize(const CodegenTypeUtils* typeUtils) const override
  // {
  //   return bitsize / 8;
  // }

  bool isBaseType() const override { return true; }

  bool isPlainType() const override { return true; }

  bool isSigned() const override { return issigned; }

  bool isAnyNumber() const override { return true; }

  bool isAnyInt() const override { return true; }
  bool isAnyFloat() const override { return false; }

  int typeBitsize() const override { return bitsize; }

  std::unique_ptr<TypeEnumMaker> makeBaseTypeEnumMaker() const override
  {
    return std::unique_ptr<TypeEnumMaker>(new EnumMaker);
  }
};


// -----------------------------------------------------------------------------

//! TypeProperty implementation for the int32 type.
class Int32TypeProperty
    : public BaseIntTypeProperty<Int32TypeEnumMaker, 32, K(issigned)  // ,
                                 // CodegenTools::kAtomInt32
                                 > {
public:
  Int32TypeProperty() {}

  // zstring convFuncName() const override { return "h7_atom_2_int32"; }

  // llvm::Type* getLLVMType(const CodegenTypeUtils* typeUtils) const override
  // {
  //   return llvm::Type::getInt32Ty(typeUtils->context());
  // }
};


//! TypeProperty implementation for the clang.int type.  TODO: shouldn't
//! this be 32/64 depending on host platform?
class ClangIntTypeProperty
    : public BaseIntTypeProperty<Int32TypeEnumMaker, 32, K(issigned)  // ,
                                 // CodegenTools::kAtomInt32
                                 > {
public:
  ClangIntTypeProperty() {}

  // zstring convFuncName() const override { return "h7_atom_2_int32"; }

  // llvm::Type* getLLVMType(const CodegenTypeUtils* typeUtils) const override
  // {
  //   return llvm::Type::getInt32Ty(typeUtils->context());
  // }
};


// -----------------------------------------------------------------------------

//! TypeProperty implementation for the int32 type.
class UInt32TypeProperty
    : public BaseIntTypeProperty<UInt32TypeEnumMaker, 32, !K(issigned)  // ,
                                 // CodegenTools::kAtomUInt32
                                 > {
public:
  UInt32TypeProperty() {}

  // zstring convFuncName() const override { return "h7_atom_2_uint32"; }

  // llvm::Type* getLLVMType(const CodegenTypeUtils* typeUtils) const override
  // {
  //   return llvm::Type::getInt32Ty(typeUtils->context());
  // }
};


// -----------------------------------------------------------------------------

//! TypeProperty implementation for the int16 type.
class Int16TypeProperty
    : public BaseIntTypeProperty<Int16TypeEnumMaker, 16, K(issigned)  // ,
                                 // CodegenTools::kAtomInt16
                                 > {
public:
  Int16TypeProperty() {}

  // zstring convFuncName() const override { return "h7_atom_2_int16"; }

  // llvm::Type* getLLVMType(const CodegenTypeUtils* typeUtils) const override
  // {
  //   return llvm::Type::getInt16Ty(typeUtils->context());
  // }
};


// -----------------------------------------------------------------------------

//! TypeProperty implementation for the uint16 type.
class UInt16TypeProperty
    : public BaseIntTypeProperty<UInt16TypeEnumMaker, 16, !K(issigned)  // ,
                                 // CodegenTools::kAtomUInt16
                                 > {
public:
  UInt16TypeProperty() {}

  // zstring convFuncName() const override { return "h7_atom_2_uint16"; }

  // llvm::Type* getLLVMType(const CodegenTypeUtils* typeUtils) const override
  // {
  //   return llvm::Type::getInt16Ty(typeUtils->context());
  // }
};


// -----------------------------------------------------------------------------

//! TypeProperty implementation for the int8 type.
class Int8TypeProperty
    : public BaseIntTypeProperty<Int8TypeEnumMaker, 8, K(issigned)  // ,
                                 // CodegenTools::kAtomInt8
                                 > {
public:
  Int8TypeProperty() {}

  // zstring convFuncName() const override { return "h7_atom_2_int8"; }

  // llvm::Type* getLLVMType(const CodegenTypeUtils* typeUtils) const override
  // {
  //   return llvm::Type::getInt8Ty(typeUtils->context());
  // }
};


// -----------------------------------------------------------------------------

//! TypeProperty implementation for the uint8 type.
class UInt8TypeProperty
    : public BaseIntTypeProperty<UInt8TypeEnumMaker, 8, !K(issigned)  // ,
                                 // CodegenTools::kAtomUInt8
                                 > {
public:
  UInt8TypeProperty() {}

  // zstring convFuncName() const override { return "h7_atom_2_uint8"; }

  // llvm::Type* getLLVMType(const CodegenTypeUtils* typeUtils) const override
  // {
  //   return llvm::Type::getInt8Ty(typeUtils->context());
  // }
};


// -----------------------------------------------------------------------------

//! TypeProperty implementation for the int8 type.
class Int64TypeProperty
    : public BaseIntTypeProperty<Int64TypeEnumMaker, 64, K(issigned)  // ,
                                 // CodegenTools::kAtomInt64
                                 > {
public:
  Int64TypeProperty() {}

  // zstring convFuncName() const override { return "h7_atom_2_int64"; }

  // llvm::Type* getLLVMType(const CodegenTypeUtils* typeUtils) const override
  // {
  //   return llvm::Type::getInt64Ty(typeUtils->context());
  // }
};


// -----------------------------------------------------------------------------

//! TypeProperty implementation for the uint8 type.
class UInt64TypeProperty
    : public BaseIntTypeProperty<UInt64TypeEnumMaker, 64, !K(issigned)  // ,
                                 // CodegenTools::kAtomUInt64
                                 > {
public:
  UInt64TypeProperty() {}

  // zstring convFuncName() const override { return "h7_atom_2_uint64"; }

  // llvm::Type* getLLVMType(const CodegenTypeUtils* typeUtils) const override
  // {
  //   return llvm::Type::getInt64Ty(typeUtils->context());
  // }
};

}  // namespace herschel
